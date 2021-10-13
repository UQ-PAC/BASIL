package BilTranslating;

import Facts.inst.*;

import java.util.ArrayList;
import java.util.List;

public class FlowGraph {

    // a list of all FunctionBlocks in the program
    private List<FunctionBlock> functions;
    // a non-function block that is first executed upon running the program
    private Block root;

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
     * Program terminations are identified by analysing the blocks which remain after removing the function clusters.
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
        List<FunctionBlock> functionBlocks;
        Block root;

        List<Integer> splits = getSplits(facts);
        splits.sort(Integer::compareTo);

        for (Integer split : splits) {

        }
    }

    private List<Integer> getSplits(List<InstFact> facts) {
        List<Integer> splits = new ArrayList<>();
        for (int i = 0; i < facts.size(); i++) {
            InstFact fact = facts.get(i);
            if (fact instanceof JmpFact) {
                splits.add(i);
                int targetIndex = findInstWithPc(((JmpFact) fact).target, facts);
                splits.add(targetIndex - 1);
            } else if (fact instanceof CjmpFact) {
                splits.add(i);
                int targetIndex = findInstWithPc(((CjmpFact) fact).target, facts);
                splits.add(targetIndex - 1);
            } else if (fact instanceof CallFact) {
                splits.add(i);
                int targetIndex = findInstWithPc(((CallFact) fact).returnAddr, facts);
                splits.add(targetIndex - 1);
            }
            else if (fact instanceof EnterSubFact) {
                splits.add(i - 1);
            }
            else if (fact instanceof ExitSubFact) {
                splits.add(i);
            }
        }
        return splits;
    }

    private int findInstWithPc(String pc, List<InstFact> facts) {
        for (int i = 0; i < facts.size(); i++) {
            if (facts.get(i).label.pc.equals(pc)) return i;
        }
        System.err.printf("Error in constructing flow graph: No inst found with pc %s.\n", pc);
        return -2;
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
