package BilTranslating;

import Facts.inst.*;
import Util.AssumptionViolationException;
import java.util.*;

public class FlowGraph {

    // the head of all global code; the starting point for the boogie program
    Block globalBlock;
    // a list of all function heads; represents all functions/procedures in the boogie program
    List<Block> functionBlocks;

    FlowGraph(List<Block> functionBlocks) {
        this.globalBlock = new Block(new ArrayList<>(), new ArrayList<>());
        this.functionBlocks = functionBlocks;
    }

    /**
     * Creates a FlowGraph from the given list of facts.
     *
     * Assumptions:
     * No code is shared between functions. I.e. if a line belongs to one function cluster, then it will not belong to
     * any other function cluster.
     */
    public FlowGraph fromFactsList(List<InstFact> facts) {
        List<Block> functionBlocks = new ArrayList<>();
        List<Integer> splits = getSplits(facts);
        for (int i = 0; i < splits.size(); i++) {
            Integer split = splits.get(i);
            InstFact fact = facts.get(split);
            if (fact instanceof EnterSubFact) {
                functionBlocks.add(createBlockFromSplit(split, splits, facts, new ArrayList<>()));
            }
        }
        return new FlowGraph(functionBlocks);
    }

    /**
     * Recursively creates a graph of blocks beginning from the given split.
     *
     * Algorithm:
     * Create a block representing this split.
     * Recursively create children blocks representing any target splits, if those blocks don't already exist.
     * Add those children blocks to the created block's list of children.
     *
     * Requires list of splits to be ordered.
     */
    private Block createBlockFromSplit(Integer split, List<Integer> splits, List<InstFact> facts,
                                              List<Block> existingBlocks) {
        Integer nextSplit = splits.get(splits.indexOf(split) + 1);
        List<InstFact> blockLines = facts.subList(split, nextSplit);
        Block block = new Block(blockLines, new ArrayList<>());
        existingBlocks.add(block);
        Integer lastLineInBlock = nextSplit - 1;
        List<Integer> splitsOfChildren = getTargetIndexes(facts, lastLineInBlock);
        // do not create children blocks that already exist - just add them instead
        for (Block existingBlock : existingBlocks) {
            Integer existingBlockSplit = facts.indexOf(existingBlock.firstLine());
            if (splitsOfChildren.contains(existingBlockSplit)) {
                block.children.add(existingBlock);
                splitsOfChildren.remove(existingBlockSplit);
            }
        }
        // now recursively create the rest of the children
        for (Integer childSplit : splitsOfChildren) {
            Block child = createBlockFromSplit(childSplit, splits, facts, existingBlocks);
            block.children.add(child);
        }
        return block;
    }

    /**
     * Returns a list of the indexes of all lines that may follow this line in a control flow graph.
     * Hard jumps (jumps, calls) may jump to their targets.
     * Soft jumps (conditional jumps) may jump to the next line or their targets.
     * Function returns (ExitSubs) necessarily jump to nowhere.
     * All other instructions simply jump to the next line.
     *
     * It is assumed that in a parsed BIL file, any lines immediately preceding a function header will be either a hard
     * jump or a function return. In future, if we wanted to avoid this assumption, we could add a check to this code
     * that creates a function return fact after any lines which immediately precede a function header and do not meet
     * these conditions.
     */
    private List<Integer> getTargetIndexes(List<InstFact> facts, Integer jumperIndex) {
        InstFact jumper = facts.get(jumperIndex);
        List<Integer> targets = new ArrayList<>();
        if (jumper instanceof JmpFact) {
            int targetIndex = findInstWithPc(((JmpFact) jumper).target, facts);
            targets.add(targetIndex);
        } else if (jumper instanceof CjmpFact) {
            targets.add(jumperIndex + 1);
            int targetIndex = findInstWithPc(((CjmpFact) jumper).target, facts);
            targets.add(targetIndex);
        } else if (jumper instanceof CallFact) {
            int targetIndex = findInstWithPc(((CallFact) jumper).returnAddr, facts);
            targets.add(targetIndex);
        } else if (!(jumper instanceof ExitSubFact)) {
            targets.add(jumperIndex + 1);
        }
        for (Integer target : targets) {
            if (facts.get(target) instanceof EnterSubFact) {
                throw new AssumptionViolationException("Assumption not met while creating control flow graph: \n" +
                        "Found a block with a function block as its child.");
            }
        }
        return targets;
    }

    /**
     * Returns an ordered list of all fact indexes ('splits') which represent the start of a block.
     * Any given facts list will also be provided a split at the beginning and end of the program.
     * Splits are identified as any lines which either follow some jump, or are jumped to (conditionally or otherwise).
     */
    private List<Integer> getSplits(List<InstFact> facts) {
        // splits are represented as the index of the element following the split; i.e. the beginning of a block
        List<Integer> splits = new ArrayList<>();
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
        splits.sort(Integer::compareTo);
        return splits;
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

        Set<InstFact> linesInCluster() {
            Set<InstFact> allLines = new HashSet<>(lines);
            for (Block child : children) {
                allLines.addAll(child.linesInCluster());
            }
            return allLines;
        }

        Set<Block> blocksInCluster() {
            Set<Block> allBlocks = new HashSet<>();
            allBlocks.add(this);
            for (Block child : children) {
                allBlocks.addAll(child.blocksInCluster());
            }
            return allBlocks;
        }

        /**
         * Blocks are equal if their first lines are equal.
         * Assumes that all InstFacts are distinct, which holds due to unique pc identifiers.
         */
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Block block = (Block) o;
            return firstLine().equals(block.firstLine());
        }

        @Override
        public int hashCode() {
            return Objects.hash(firstLine());
        }
    }
}
