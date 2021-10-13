package BilTranslating;

import Facts.inst.*;
import java.util.*;
import Util.AssumptionViolationException;

// todo: throw exceptions when assumptions aren't met
// todo: sort out functions which don't explicitly end in return statements

/**
 * A flow graph is a directed, disconnected graph of blocks, where a block represents an ordered list of facts.
 * These clusters of connected graphs within the FlowGraph are referred to here as "clusters", and each cluster has a
 * single access point, which is a special block called a HeadBlock, which was partially named after how I felt creating
 * this file.
 *
 * Each HeadBlock represents one of two clusters:
 * A function cluster, representing a function/procedure/subroutine.
 * The global cluster, representing global code. There is only one such cluster.
 *
 * Now with C programs compiled to BIL, the starting point for any program is the main function. However,
 * StatementLoader will add global code before main to initialise global variables and such. Therefore, we assume:
 * - The starting point for the program is the HeadBlock of the global cluster.
 * - Global lines only exist in a consecutive sequence starting from the top of a given facts list.
 *
 * Using these assumptions, as well as some others listed below (todo), we can define the rules for block and cluster
 * creation as follows:
 * todo
 *
 * todo assumptions
 */
public class FlowGraph {

    // a list of all FunctionBlocks in the program
    List<FunctionBlock> functions;
    // a non-function block that is first executed upon running the program
    Block root;

    FlowGraph(List<FunctionBlock> functions, Block root) {
        this.functions = functions;
        this.root = root;
    }

    /**
     * Algorithm:
     * Put all facts into a single block.
     * Add a split below every jump, function-call-goto and conditional jump.
     * Add a split above every line that is jumped to by the above three instruction types.
     * Add a split above every function header and below every function return.
     * Add children as follows:
     * Blocks ending in jumps have one child - the target.
     * Blocks ending in conditional jumps have two children - the target and the next line.
     * Blocks ending in call facts have one child - the target of the return goto.
     * Blocks ending in function returns or program terminations have no children.
     * All other blocks have one child - the next line.
     * Although explicit program terminations like exit(0) are currently not handled, we define one program termination
     * as the last block in the ordered sequence of blocks where all blocks reachable by functions are removed.
     *
     * Assumptions:
     * Any block reachable by a function block cannot reach, nor can be reached from, a block reachable by another
     * function, nor a block reachable from the root node.
     * In other words, for all blocks: the block can only be reached by exactly one function, or the root node, but
     * not both.
     * Note that unreachable blocks are removed from the flow graph; all blocks contained in the flow graph are either:
     * - A root block.
     * - A function block.
     * - A block reachable by the root block.
     * - A block reachable by a function block.
     */
    FlowGraph fromFactsList(List<InstFact> facts) {
        Set<Integer> splitsSet = getSplits(facts);
        List<Integer> splits = new ArrayList<>(splitsSet);
        splits.sort(Integer::compareTo);
        List<Block> blocks = getBlocks(facts, splits);
        setChildren(blocks, facts);
        List<FunctionBlock> functionBlocks = new ArrayList<>();
        for (Block block : blocks) {
            if (block instanceof FunctionBlock) {
                functionBlocks.add((FunctionBlock) block);
            }
        }
        Block root = blocks.get(0);
        if (root instanceof FunctionBlock) {
            throw new AssumptionViolationException(
                    String.format("It is assumed the program entry point is a non-function line at the beginning of " +
                            "the program. Instead, the function header %s was found.",
                            blocks.get(0).lines.get(0)));
        }
        return new FlowGraph(functionBlocks, root);
    }

    private Set<Integer> getSplits(List<InstFact> facts) {
        // splits are represented as the index of the element following the split; i.e. the beginning of a block
        Set<Integer> splits = new HashSet<>();
        for (int i = 0; i < facts.size(); i++) {
            InstFact fact = facts.get(i);
            if (fact instanceof JmpFact) {
                splits.add(i + 1);
                int targetIndex = findInstWithPc(((JmpFact) fact).target, facts);
                splits.add(targetIndex);
            } else if (fact instanceof CjmpFact) {
                splits.add(i + 1);
                int targetIndex = findInstWithPc(((CjmpFact) fact).target, facts);
                splits.add(targetIndex);
            } else if (fact instanceof CallFact) {
                splits.add(i + 1);
                int targetIndex = findInstWithPc(((CallFact) fact).returnAddr, facts);
                splits.add(targetIndex);
            }
            else if (fact instanceof EnterSubFact) {
                splits.add(i);
            }
            else if (fact instanceof ExitSubFact) {
                splits.add(i + 1);
            }
        }
        // ensure there is a split at the start and end of the program
        splits.add(0);
        splits.add(facts.size());
        return splits;
    }

    // takes a sorted list of splits with no duplicates
    // returns a list of blocks reflecting the order of the splits list
    private List<Block> getBlocks(List<InstFact> facts, List<Integer> splits) {
        List<Block> blocks = new ArrayList<>();
        for (int i = 0; i < splits.size() - 1; i++) {
            List<InstFact> blockLines = facts.subList(splits.get(i), splits.get(i + 1));
            Block block = new Block(blockLines, new ArrayList<>());
            blocks.add(block);
        }
        return blocks;
    }

    /**
     * Add children as follows:
     * Blocks ending in jumps have one child - the target.
     * Blocks ending in conditional jumps have two children - the target and the next line.
     * Blocks ending in call facts have one child - the target of the return goto.
     * Blocks ending in function returns or program terminations have no children.
     * All other blocks have one child - the next line, except if that next line is a function header.
     * Although explicit program terminations like exit(0) are currently not handled, we define one program termination
     * as the last block in the ordered sequence of blocks where all blocks reachable by functions are removed.
     *
     * Assumes that the order of blocks reflects the original order of BIL lines (i.e. facts).
     *
     *
     *
     * also todo: some function like infinite loops don't necessarily end in return statements, so we need to add them in.
     */
    private void setChildren(List<Block> blocks, List<InstFact> facts) {
        for (int i = 0; i < blocks.size(); i++) {
            Block block = blocks.get(i);
            InstFact lastLine = block.lastLine();
            if (lastLine instanceof JmpFact) {
                JmpFact jump = (JmpFact) lastLine;
                int targetIndex = findInstWithPc(jump.target, facts);
                Block targetBlock = getBlockStartingWith(facts.get(targetIndex), blocks);
                block.children.add(targetBlock);
            } else if (lastLine instanceof CjmpFact) {
                CjmpFact call = (CjmpFact) lastLine;
                int targetIndex = findInstWithPc(call.target, facts);
                Block targetBlock = getBlockStartingWith(facts.get(targetIndex), blocks);
                Block otherChild;
                try {
                    otherChild = blocks.get(i + 1);
                } catch (IndexOutOfBoundsException e) {
                    throw new AssumptionViolationException("The final block should not end in a conditional jump.");
                }
                block.children.add(targetBlock);
                block.children.add(otherChild);
            } else if (lastLine instanceof CallFact) {
                CallFact call = (CallFact) lastLine;
                int targetIndex = findInstWithPc(call.returnAddr, facts);
                Block targetBlock = getBlockStartingWith(facts.get(targetIndex), blocks);
                block.children.add(targetBlock);
            } else if (lastLine instanceof ExitSubFact) {
                continue;
            } else if
        }
    }

    private Block getBlockStartingWith(InstFact fact, List<Block> blocks) {
        for (Block block : blocks) {
            if (block.firstLine() == fact) {
                return block;
            }
        }
        throw new AssumptionViolationException(
                String.format("Failed to build flow graph: Could not find block beginning with target %s", fact));
    }

    private int findInstWithPc(String pc, List<InstFact> facts) {
        for (int i = 0; i < facts.size(); i++) {
            if (facts.get(i).label.pc.equals(pc)) return i;
        }
        System.err.printf("Error in constructing flow graph: No inst found with pc %s.\n", pc);
        return -1;
    }

    /**
     * A block is an ordered list of facts.
     * Every block must have at least one child, or end in a function return statement or program terminate statement.
     * However, it is not necessary that blocks end in jumps, calls or conditional jumps to have children, as a child
     * may simply be the next line in the program.
     */
    class Block {
        List<InstFact> lines;
        List<Block> children;

        Block(List<InstFact> lines, List<Block> children) {
            this.lines = lines;
            this.children = children;
        }

        InstFact firstLine() {
            return lines.get(0);
        }

        InstFact lastLine() {
            return lines.get(lines.size() - 1);
        }
    }

    /**
     * A function block is block which begins with an EnterSubFact, and is the head of a self-contained graph, with all
     * leaf nodes ending in a function return statement.
     */
    class FunctionBlock extends Block {
        EnterSubFact function;

        FunctionBlock(List<InstFact> lines, List<Block> children) {
            super(lines, children);
            function = (EnterSubFact) lines.get(0);
        }
    }
}
