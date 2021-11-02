package BilTranslating;

import Facts.inst.*;
import Util.AssumptionViolationException;
import java.util.*;

/**
 * A flow graph is a graphical representation of a bil/boogie program.
 * Nodes in the graph represent basic blocks, which are defined by jumps, conditional jumps, function headers, function
 * returns and lines which are jumped to.
 * Edges in the graph represent 'links' (i.e. conditional jumps, jumps or simply the following line) between blocks.
 * In this file, a block 'cluster' refers to a complete subgraph of linked blocks, which is disjoint from all other
 * blocks in the flow graph. An example of a cluster is a function cluster, which represents all blocks within a
 * self-contained function (or 'procedure' in boogie).
 * The 'head' of a cluster refers to the root block of that cluster. For example, the first line of a function cluster's
 * root block will always be an EnterSubFact.
 * The globalBlock is the head of a cluster which represents all global code and is the presumed starting point for the
 * boogie program.
 * Each functionBlock is the head of a function cluster.
 *
 * A flow graph self-maintains particular guarantees for user programs:
 * 1. All lines (i.e. InstFacts) in the flow graph are unique; !line1.equals(line2) for all line1, line2.
 * 2. Clusters are disjoint; no block reachable by a head block is reachable by any other head block.
 */
public class FlowGraph {
    // the head of all global code; the starting point for the boogie program
    private final Block globalBlock;
    // a list of all function heads; represents all functions/procedures in the boogie program
    private final List<Block> functionBlocks;

    private FlowGraph(List<Block> functionBlocks) {
        this.globalBlock = new Block(new ArrayList<>(), new ArrayList<>());
        this.functionBlocks = functionBlocks;
    }

    /**
     * Creates a FlowGraph from the given list of facts.
     * Assumes no line is reachable from more than one function header (i.e. EnterSubFact).
     */
    public static FlowGraph fromFactsList(List<InstFact> facts) {
        FlowGraph flowGraph = FlowGraphFactory.fromFactsList(facts);
        flowGraph.enforceDisjointFunctions();
        return flowGraph;
    }

    public Block getGlobalBlock() {
        return globalBlock;
    }

    public List<Block> getFunctionBlocks() {
        return functionBlocks;
    }

    public List<InstFact> getLines() {
        List<InstFact> lines = new ArrayList<>(globalBlock.getLines());
        functionBlocks.forEach(block -> lines.addAll(block.getLines()));
        return lines;
    }

    public void removeLine(InstFact line) {
        globalBlock.lines.remove(line);
        functionBlocks.forEach(block -> block.lines.remove(line));
    }

    private void enforceDisjointFunctions() {
        Set<Block> usedBlocks = new HashSet<>();
        for (Block functionBlock : functionBlocks) {
            List<Block> cluster = functionBlock.getBlocksInCluster();
            for (Block clusterBlock : cluster) {
                if (usedBlocks.contains(clusterBlock)) {
                    throw new AssumptionViolationException(String.format(
                            "Flow graph error. The following block can be accessed by two different functions:\n%s",
                            clusterBlock.toString()
                    ));
                }
            }
            usedBlocks.addAll(cluster);
        }
    }

    /**
     * A complete traversal on a flow graph should encounter no line twice.
     * This ensures:
     * 1. No blocks overlap (i.e. share lines).
     * 2. No block exists in multiple locations within the flow graph.
     */
    private void enforceUniqueLines() {
        List<InstFact> lines = new ArrayList<>();
        List<Block> blocks = new ArrayList<>(functionBlocks);
        blocks.add(globalBlock);
        blocks.forEach(block -> lines.addAll(block.getLines()));
        Set<InstFact> linesSet = new HashSet<>(lines);
        if (lines.size() != linesSet.size()) {
            // find duplicates
            linesSet.forEach(lines::remove);
            StringBuilder builder = new StringBuilder();
            builder.append("Flow graph contains duplicate lines:\n");
            lines.forEach(builder::append);
            throw new AssumptionViolationException(builder.toString());
        }
    }

    /**
     * Factory for a flow graph. Does not enforce any constraints.
     */
    private static class FlowGraphFactory {

        private static FlowGraph fromFactsList(List<InstFact> facts) {
            List<Integer> splits = getSplits(facts);
            List<Block> availableBlocks = createBlocksFromSplits(splits, facts);
            setChildren(availableBlocks, facts);
            List<Block> functionBlocks = getFunctionBlocksFromList(availableBlocks);
            return new FlowGraph(functionBlocks);
        }

        /**
         * Returns an ordered list of all fact indexes ('splits') which represent the start of a block.
         * Any given facts list will also be provided a split at the beginning and end of the program.
         * Splits are identified as any lines which either follow some jump, or are jumped to (conditionally or otherwise).
         */
        private static List<Integer> getSplits(List<InstFact> facts) {
            // splits are represented as the index of the element following the split; i.e. the beginning of a block
            // we use a set to avoid double-ups
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
            List<Integer> splitsList = new ArrayList<>(splits);
            splitsList.sort(Integer::compareTo);
            return splitsList;
        }

        private static int findInstWithPc(String pc, List<InstFact> facts) {
            for (int i = 0; i < facts.size(); i++) {
                if (facts.get(i).label.pc.equals(pc)) return i;
            }
            System.err.printf("Error in constructing flow graph: No inst found with pc %s.\n", pc);
            return -1;
        }

        // requires that splits are sorted and that there is a split at the beginning and end of the facts list
        private static List<Block> createBlocksFromSplits(List<Integer> splits, List<InstFact> lines) {
            List<Block> blocks = new ArrayList<>();
            for (int i = 0; i < splits.size() - 1; i++) {
                List<InstFact> blockLines = lines.subList(splits.get(i), splits.get(i + 1));
                List<Block> children = new ArrayList<>();
                Block block = new Block(blockLines, children);
                blocks.add(block);
            }
            return blocks;
        }

        private static void setChildren(List<Block> blocks, List<InstFact> facts) {
            for (Block block : blocks) {
                List<String> childrenPcs = getChildrenPcs(block, facts);
                for (String childPc : childrenPcs) {
                    Block child = findBlockStartingWith(childPc, blocks);
                    if (child == null) {
                        throw new AssumptionViolationException(String.format(
                                "Error creating flow graph. Could not find a block starting with target pc '%s'.",
                                childPc
                        ));
                    }
                    block.children.add(child);
                }
            }
        }

        private static List<String> getChildrenPcs(Block block, List<InstFact> lines) {
            List<String> childrenPcs = new ArrayList<>();
            InstFact lastLine = block.lastLine();
            if (lastLine instanceof JmpFact) {
                childrenPcs.add(((JmpFact) lastLine).target);
            } else if (lastLine instanceof CjmpFact) {
                childrenPcs.add(((CjmpFact) lastLine).target);
                childrenPcs.add(lines.get(lines.indexOf(lastLine) + 1).label.pc);
            } else if (lastLine instanceof CallFact) {
                childrenPcs.add(((CallFact) lastLine).returnAddr);
            } else if (!(lastLine instanceof ExitSubFact)) {
                childrenPcs.add(lines.get(lines.indexOf(lastLine) + 1).label.pc);
            }
            return childrenPcs;
        }

        private static List<Block> getFunctionBlocksFromList(List<Block> blocks) {
            List<Block> functionBlocks = new ArrayList<>();
            for (Block block : blocks) {
                if (block.firstLine() instanceof EnterSubFact) {
                    functionBlocks.add(block);
                }
            }
            return functionBlocks;
        }

        private static Block findBlockStartingWith(String pc, List<Block> blocks) {
            for (Block block : blocks) {
                if (block.firstLine().label.pc.equals(pc)) {
                    return block;
                }
            }
            return null;
        }
    }

    /**
     * A block is an ordered list of instruction facts (i.e. lines).
     */
    public static class Block {
        private List<InstFact> lines;
        private List<Block> children;

        Block(List<InstFact> lines, List<Block> children) {
            this.lines = lines;
            this.children = children;
        }

        public List<InstFact> getLines() {
            return lines;
        }

        public List<Block> getChildren() {
            return children;
        }

        public void setLines(List<InstFact> lines) {
            // fixme: need to work out constraints and guarantees for the flow graph. what if we remove the first line, or even modify it internally? is using the first line a sensible option for uniquely identifying blocks?
            this.lines = lines;
        }

        public InstFact firstLine() {
            return lines.get(0);
        }

        public InstFact lastLine() {
            return lines.get(lines.size() - 1);
        }

        public List<InstFact> getLinesInCluster() {
            List<InstFact> allLines = new ArrayList<>(lines);
            List<Block> blocksInCluster = getBlocksInCluster();
            blocksInCluster.forEach(block -> allLines.addAll(block.getLinesInCluster()));
            return allLines;
        }

        public List<Block> getBlocksInCluster() {
            List<Block> cluster = new ArrayList<>();
            recursivelyAddBlocks(cluster);
            return cluster;
        }

        private void recursivelyAddBlocks(List<Block> cluster) {
            cluster.add(this);
            for (Block child : children) {
                if (!cluster.contains(child)) {
                    child.recursivelyAddBlocks(cluster);
                }
            }
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("Block:\n");
            for (InstFact line : lines) {
                builder.append(line.toString());
            }
            return builder.toString();
        }
    }
}
