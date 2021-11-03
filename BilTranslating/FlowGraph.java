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
        List<InstFact> lines = new ArrayList<>(globalBlock.getLinesInCluster());
        functionBlocks.forEach(block -> lines.addAll(block.getLinesInCluster()));
        return lines;
    }

    public void removeLine(InstFact line) {
        globalBlock.lines.remove(line);
        functionBlocks.forEach(block -> block.lines.remove(line));
    }

    public void enforceConstraints() {
        enforceDisjointFunctions();
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
     * A complete traversal of a flow graph should encounter no line twice, or no line with the same PC twice.
     */
    private void enforceUniqueLines() {
        List<InstFact> linesList = getLines();
        List<String> pcList = new ArrayList<>();
        linesList.forEach(line -> pcList.add(line.label.pc));
        Set<InstFact> linesSet = new HashSet<>(linesList);
        Set<String> pcSet = new HashSet<>(pcList);
        if (linesSet.size() != linesList.size()) {
            linesSet.forEach(linesList::remove);
            StringBuilder stringBuilder = new StringBuilder();
            linesList.forEach(line -> stringBuilder.append(line.toString()));
            throw new AssumptionViolationException(String.format(
                    "Flow graph error. The following lines were found twice throughout the program:\n%s",
                    stringBuilder
            ));
        }
        if (pcSet.size() != pcList.size()) {
            pcSet.forEach(pcList::remove);
            StringBuilder stringBuilder = new StringBuilder();
            pcList.forEach(stringBuilder::append);
            throw new AssumptionViolationException(String.format(
                    "Flow graph error. The following lines were found twice throughout the program:\n%s",
                    stringBuilder
            ));
        }
    }

    /**
     * Factory for a flow graph. It works, at a high level, as follows:
     * Take a list of facts.
     * Partition the list into chunks of facts called blocks. These are the nodes in the flow graph.
     * Go through all the blocks and link them to all the other blocks that they might transition to.
     * Identify all the function blocks (i.e. blocks with function headers as first lines).
     * Return a flow graph of these function blocks.
     */
    private static class FlowGraphFactory {

        /**
         * Creates a FlowGraph from the given list of facts. Does not enforce any constraints.
         *
         * @requires facts are ordered and there are no duplicates
         * @param facts an ordered list of facts from which to create the flow graph
         * @return a new flow graph with an empty global block and no constraints
         */
        private static FlowGraph fromFactsList(List<InstFact> facts) {
            // an ordered list of indexes of the given facts list, indicating where the list should be split into blocks
            List<Integer> splits = getSplits(facts);
            // an ordered list of blocks, defined by the given given list of splits
            // essentially creates the nodes for this flow graph
            List<Block> blocks = createBlocksFromSplits(splits, facts);
            // links each block in the list to each other block that may follow this one (e.g. via a jump)
            // essentially creates the edges for this flow graph
            setChildren(blocks, facts);
            // create a flow graph consisting of the function cluster heads
            FlowGraph flowGraph = new FlowGraph(getFunctionBlocksFromList(blocks));
            // ensure the created flow graph maintains the required properties
            flowGraph.enforceConstraints();
            return new FlowGraph(getFunctionBlocksFromList(blocks));
        }

        /**
         * Returns an ordered list of all fact indexes ('splits') which represent block boundaries.
         * Splits are defined such that the splits 3 and 6 should define a block consisting of lines facts.get(3),
         * facts.get(4) and facts.get(5).
         * Any given facts list will also be provided a split at the beginning and end of the program.
         *
         * @requires facts are ordered and there are no duplicates
         * @ensures the returned list of splits are ordered, and there is a split at index 0 and at index facts.size()
         * @param facts an ordered list of facts from which to create the splits
         * @return a list of splits indicating where blocks should be defined in the given facts list
         */
        private static List<Integer> getSplits(List<InstFact> facts) {
            // we use a set to avoid double-ups, as some lines may be jumped to twice
            Set<Integer> splits = new HashSet<>();
            for (int i = 0; i < facts.size(); i++) {
                InstFact fact = facts.get(i);
                if (fact instanceof JmpFact) {
                    // for jumps, add a split below the jump and above the target line
                    splits.add(i + 1);
                    int targetIndex = findInstWithPc(((JmpFact) fact).target, facts);
                    splits.add(targetIndex);
                } else if (fact instanceof CjmpFact) {
                    // for conditional jumps, add a split below the jump and above the target line
                    splits.add(i + 1);
                    int targetIndex = findInstWithPc(((CjmpFact) fact).target, facts);
                    splits.add(targetIndex);
                } else if (fact instanceof CallFact) {
                    /* we treat calls here (i.e. call x(); goto y;) like jumps (i.e. to y). this may change in future
                    updates where we split the "call x()" part from the "goto y" part and make them separate facts */
                    splits.add(i + 1);
                    int targetIndex = findInstWithPc(((CallFact) fact).returnAddr, facts);
                    splits.add(targetIndex);
                }
                else if (fact instanceof EnterSubFact) {
                    // for function headers, add a split before the header
                    splits.add(i);
                }
                else if (fact instanceof ExitSubFact) {
                    // for function returns, add a split after the return
                    splits.add(i + 1);
                }
            }
            // ensure there is a split at the start and end of the program
            splits.add(0);
            splits.add(facts.size());
            List<Integer> splitsList = new ArrayList<>(splits);
            // ensure the list is sorted
            splitsList.sort(Integer::compareTo);
            return splitsList;
        }

        /**
         * Locates the fact in the given list that has the given PC.
         *
         * @param pc of the fact to find
         * @param facts the list of facts to search
         * @return the fact in the given list that has the given PC.
         */
        private static int findInstWithPc(String pc, List<InstFact> facts) {
            for (int i = 0; i < facts.size(); i++) {
                if (facts.get(i).label.pc.equals(pc)) return i;
            }
            throw new AssumptionViolationException(String.format(
                    "Error in constructing flow graph: No inst found with pc %s.\n", pc
            ));
        }

        /**
         * Creates a list of blocks from each pair of consecutive splits.
         * For example, for splits = [0, 3, 4, 8] and lines = [a, b, c, d, e, f, g, h], we will get blocks of:
         * [a, b, c], [d], [e, f, g, h]
         *
         * @requires splits and lines are sorted and splits.contains(0) and splits.contains(lines.size())
         * @param splits a list of indexes to define the boundaries of blocks
         * @param lines a list of facts from which to extract blocks at the indexes given by splits
         * @return a list of blocks consisting of sublists of the given facts list, as defined by the given splits list
         */
        private static List<Block> createBlocksFromSplits(List<Integer> splits, List<InstFact> lines) {
            List<Block> blocks = new ArrayList<>();
            for (int i = 0; i < splits.size() - 1; i++) {
                List<InstFact> blockLines = lines.subList(splits.get(i), splits.get(i + 1));
                // blocks are initially created with no children
                Block block = new Block(blockLines, new ArrayList<>());
                blocks.add(block);
            }
            return blocks;
        }

        /**
         * Sets the children of all blocks in the given list. Think of this like "wiring up" all the blocks in the list
         * with each other.
         *
         * @requires all possible children of blocks within the given list are also contained in the list, and the given
         * list of facts is ordered
         * @param blocks the list of blocks to create children for
         * @param facts the ordered list of facts the given blocks were created from; necessary for finding facts which
         *              directly follow the last fact in a block, as it may represent a child of this block if the last
         *              line is, for instance, not a jump
         */
        private static void setChildren(List<Block> blocks, List<InstFact> facts) {
            for (Block block : blocks) {
                // the PCs of all facts this block jumps to (for instance, the targets of jumps)
                List<String> childrenPcs = getChildrenPcs(block, facts);
                for (String childPc : childrenPcs) {
                    // tries to find a block in the list with a first line that has the child's PC
                    Block child = findBlockStartingWith(childPc, blocks);
                    // no such block was found, which means there is no block defined for this jump/cjump etc.
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

        /**
         * Finds all PCs of lines that the given block may transition to. Presuming the given block was properly
         * created, this will solely depend on the last line in the block.
         *
         * @requires the list of lines contains all PCs which may be jumped to, and the given block was properly
         * created (e.g. does not contain any jumps halfway through), and the list of lines is ordered
         * @param block to find the children PCs of
         * @param lines see #setChildren@facts
         * @return a list of PCs representing the children of the given block
         */
        private static List<String> getChildrenPcs(Block block, List<InstFact> lines) {
            List<String> childrenPcs = new ArrayList<>();
            InstFact lastLine = block.lastLine();
            if (lastLine instanceof JmpFact) {
                // for jumps, simply add the target
                childrenPcs.add(((JmpFact) lastLine).target);
            } else if (lastLine instanceof CjmpFact) {
                // for conditional jumps, add the target as well as the following line
                childrenPcs.add(((CjmpFact) lastLine).target);
                childrenPcs.add(lines.get(lines.indexOf(lastLine) + 1).label.pc);
            } else if (lastLine instanceof CallFact) {
                // for calls, add the target of the goto portion
                childrenPcs.add(((CallFact) lastLine).returnAddr);
            } else if (!(lastLine instanceof ExitSubFact)) {
                // for any other line that is not a function return, simply add the following line
                childrenPcs.add(lines.get(lines.indexOf(lastLine) + 1).label.pc);
            }
            return childrenPcs;
        }

        /**
         * Returns all the blocks within the given list that are heads of function clusters.
         *
         * @param blocks to search for function blocks
         * @return all function blocks within the given list
         */
        private static List<Block> getFunctionBlocksFromList(List<Block> blocks) {
            List<Block> functionBlocks = new ArrayList<>();
            for (Block block : blocks) {
                if (block.firstLine() instanceof EnterSubFact) {
                    functionBlocks.add(block);
                }
            }
            return functionBlocks;
        }

        /**
         * Searches for a block in the given list that contains a first line with the given PC.
         * Returns null if none found.
         *
         * @param pc the PC to search for
         * @param blocks the list of blocks to search through
         * @return the first block with a first line that contains the given pc, or null if none found
         */
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
     * A block is an ordered list of instruction facts (i.e. lines). They represent nodes in the flow graph.
     * Blocks have a list of children blocks, which represent directed edges in the flow graph (from block, to child).
     * They are designed to be highly malleable, and therefore do not provide any guarantees about duplicates or other
     * constraints.
     * Since they are constantly changing, blocks are defined according to their memory addresses (i.e. they do not
     * override equals) in order to maintain consistent identity.
     */
    public static class Block {
        // the lines of code this block contains
        private List<InstFact> lines;
        // a list of other blocks this block may transition to
        private List<Block> children;

        /**
         * Constructor for a block.
         */
        Block(List<InstFact> lines, List<Block> children) {
            this.lines = lines;
            this.children = children;
        }

        /**
         * The returned list may be mutated in order to update this block.
         * @return the list object containing lines of code this block contains
         */
        public List<InstFact> getLines() {
            return lines;
        }

        /**
         * The returned list may be mutated in order to update this block.
         * @return the list object containing the children of this block
         */
        public List<Block> getChildren() {
            return children;
        }

        /**
         * Sometimes, rather than updating the list returned by getLines(), we may want to re-set the list of lines of
         * this block entirely.
         * @param lines to set
         */
        public void setLines(List<InstFact> lines) {
            this.lines = lines;
        }

        /**
         * Sometimes, rather than updating the list returned by getChildren(), we may want to re-set the children of
         * this block entirely.
         * @param children to set
         */
        public void setChildren(List<Block> children) {
            this.children = children;
        }

        /**
         * A convenience method that returns the first line of this block.
         * @return the first line of this block
         */
        public InstFact firstLine() {
            return lines.get(0);
        }

        /**
         * A convenience method that returns the last line of this block.
         * @return the last line of this block
         */
        public InstFact lastLine() {
            return lines.get(lines.size() - 1);
        }

        /**
         * Reads through all lines of all blocks reachable (directly or indirectly) by this block and returns a list of
         * all these lines.
         * As such, this list may contain duplicate lines if flow graph properties are not properly constrained.
         *
         * @return all of the lines of all blocks reachable by this block - including all lines of this block
         */
        public List<InstFact> getLinesInCluster() {
            List<InstFact> allLines = new ArrayList<>(lines);
            getBlocksInCluster().forEach(block -> allLines.addAll(block.lines));
            return allLines;
        }

        /**
         * @return a list of all blocks reachable by this block via a depth first search
         */
        public List<Block> getBlocksInCluster() {
            List<Block> cluster = new ArrayList<>();
            recursivelyAddBlocks(cluster);
            return cluster;
        }

        /**
         * Helper method for {@link Block#getBlocksInCluster()}.
         * @param cluster list of blocks which have already been explored
         */
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
