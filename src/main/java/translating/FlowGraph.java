package translating;

import facts.exp.Var;
import facts.stmt.*;
import facts.stmt.Assign.AssignFact;
import facts.stmt.Assign.MoveFact;
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
 *
 * Example usage:
 * Retrieve all blocks within a function: getFunctionBlocks().get(0).getBlocksInCluster()
 * Insert a line in a particular location: getFunctionBlocks().get(0).getChildren().get(0).getLines().add(4, line)
 *
 * todo:
 * [ ] make block.getLines() return an immutable list, because we want to practice good java
 */
public class FlowGraph {
    // a list of all function heads; represents all functions/procedures in the boogie program
    private List<Function> functions;
    private List<InitStmt> globalInits;

    private FlowGraph(List<Function> functions) {
        this.functions = functions;
        globalInits = new LinkedList<>();
        globalInits.add(new InitStmt(new Var("mem"), "mem", "[int] int")); // TODO label.none
    }

    /**
     * Creates a translating.FlowGraph from the given list of facts.
     * Assumes no line is reachable from more than one function header (i.e. EnterSubFact).
     */
    public static FlowGraph fromFactsList(List<Stmt> facts) {
        FlowGraph flowGraph = FlowGraphFactory.fromFactsList(facts);
        flowGraph.enforceDisjointFunctions();
        return flowGraph;
    }

    public List<InitStmt> getGlobalInits() {
        return globalInits;
    }

    public void setFunctions(List<Function> functions) {
        this.functions = functions;
    }

    /**
     * @return the list of functions of this flow graph
     */
    public List<Function> getFunctions() {
        return functions;
    }

    /**
     * @return all lines of all blocks within this flow graph
     */
    public List<Stmt> getViewOfLines() {
        List<Stmt> lines = new ArrayList<>();
        getBlocks().forEach(block -> lines.addAll(block.getLines()));
        return lines;
    }

    /**
     * @return all blocks within this flow graph
     */
    public List<Block> getBlocks() {
        List<Block> blocks = new ArrayList<>();
        functions.forEach(function -> blocks.addAll(function.rootBlock.getBlocksInCluster()));
        return blocks;
    }

    public void removeLine(Stmt line) {
        getBlocks().forEach(block -> block.getLines().remove(line));
    }

    /**
     * Enforce guaranteed properties of this flow graph. Exceptions are thrown when these constraints are not met.
     */
    public void enforceConstraints() {
        enforceDisjointFunctions();
        enforceUniqueLines();
    }

    /**
     * No block should be accessible (directly or indirectly) by two different function blocks.
     */
    private void enforceDisjointFunctions() {
        Set<Block> usedBlocks = new HashSet<>();
        for (Function function : functions) {
            List<Block> cluster = function.rootBlock.getBlocksInCluster();
            for (Block block : cluster) {
                if (usedBlocks.contains(block)) {
                    throw new AssumptionViolationException(String.format(
                            "Flow graph error. The following block can be accessed by two different functions:\n%s",
                            block.toString()
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
        List<Stmt> linesList = getViewOfLines();
        List<String> pcList = new ArrayList<>();
        linesList.forEach(line -> pcList.add(line.getLabel().getPc()));
        Set<Stmt> linesSet = new HashSet<>(linesList);
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

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        globalInits.forEach(init -> builder.append(init).append("\n"));
        functions.forEach(function -> builder.append(function.toString()));
        return builder.toString();
    }

    public static class Function {
        // name of the function
        private final EnterSub header;
        // the first block in this list must be the first block executed for the function in BIL
        private final Block rootBlock;

        private List<InitStmt> initStmts;

        public Function(EnterSub header, Block rootBlock) {
            this.header = header;
            this.rootBlock = rootBlock;
            this.initStmts = new LinkedList<>();
        }

        public EnterSub getHeader() {
            return header;
        }

        public Block getRootBlock() {
            return rootBlock;
        }

        public void addInitFact(InitStmt initStmt) {
            initStmts.add(initStmt);
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append(header).append("\n");
            initStmts.forEach(fact -> builder.append(fact).append("\n"));
            rootBlock.getBlocksInCluster().forEach(builder::append);
            builder.append("}");
            return builder.toString().replaceAll("\n", "\n  ") + "\n";
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
         * Creates a translating.FlowGraph from the given list of facts. Does not enforce any constraints.
         *
         * @requires facts are ordered and there are no duplicates
         * @param facts an ordered list of facts from which to create the flow graph
         * @return a new flow graph with an empty global block and no constraints
         */
        private static FlowGraph fromFactsList(List<Stmt> facts) {
            facts = setFunctionsWithReturns(facts);
            // an ordered list of indexes of the given facts list, indicating where the list should be split into blocks
            List<Integer> splits = getSplits(facts);
            // an ordered list of blocks, defined by the given given list of splits
            // essentially creates the nodes for this flow graph
            List<Block> blocks = createBlocksFromSplits(splits, facts);
            blocks = stripBlocks(blocks);
            // links each block in the list to each other block that may follow this one (e.g. via a jump)
            // essentially creates the edges for this flow graph
            setChildren(blocks, facts);
            // create a flow graph consisting of the functions formed from these blocks
            FlowGraph flowGraph = new FlowGraph(convertBlocksToFunctions(blocks));
            // ensure the created flow graph maintains the required properties
            flowGraph.enforceConstraints();
            return flowGraph;
        }

        // TODO can this be moved to translating.StatementLoader?
        private static List<Stmt> setFunctionsWithReturns (List<Stmt> facts) {
            for (int i = 0; i < facts.size(); i++) {
                if (!(facts.get(i) instanceof CallStmt) || !(facts.get(i + 1) instanceof JmpFact) || !(facts.get(i + 3) instanceof MoveFact)) continue;

                CallStmt call = (CallStmt) facts.get(i);
                JmpFact cjmp = (JmpFact) facts.get(i + 1);
                AssignFact assign = (MoveFact) facts.get(i + 3);

                if (cjmp.getTarget().equals(facts.get(i + 1).getLabel().getPc())) throw new AssumptionViolationException("Expected jump to next line");

                call.setLHS(assign.getLhs());
                facts.remove(i + 1);
                facts.remove(i + 2);
                facts.remove(i + 3);
            }

            return facts;
        }

        private static Block findFunction(List<Block> blocks, String functionName) {
            return blocks.stream().filter(b -> b.firstLine() instanceof EnterSub && ((EnterSub) b.firstLine()).getFuncName().equals(functionName)).findFirst().get();
        }

        private static List<Block> stripBlocks (List<Block> blocks) {
            List<Block> reachableBlocks = new LinkedList<>();
            LinkedList<Block> queue = new LinkedList<>();

            Block mainBlock = blocks.stream().filter(block -> block.getLines().get(0) instanceof EnterSub && ((EnterSub) block.getLines().get(0)).getFuncName().equals("main")).findFirst().get();
            queue.add(mainBlock);

            while (!queue.isEmpty()) {
                Block block = queue.poll();
                if (reachableBlocks.contains(block)) continue;

                reachableBlocks.add(block);

                block.getLines().forEach(fact -> {
                    // TODO rewrite using match
                    if (fact instanceof JmpFact) {
                        String targetPC = ((JmpFact) fact).getTarget();
                        queue.add(findBlockStartingWith(targetPC, blocks));
                    } else if (fact instanceof CJmpStmt) {
                        String targetPC = ((CJmpStmt) fact).getTarget();
                        queue.add(findBlockStartingWith(targetPC, blocks));
                    } else if (fact instanceof CallStmt) {
                        queue.add(findFunction(blocks, ((CallStmt) fact).getFuncName()));
                    }
                });
            }

            return reachableBlocks;
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
        private static List<Integer> getSplits(List<Stmt> facts) {
            // we use a set to avoid double-ups, as some lines may be jumped to twice
            Set<Integer> splits = new HashSet<>();
            for (int i = 0; i < facts.size(); i++) {
                Stmt fact = facts.get(i);
                if (fact instanceof JmpFact) {
                    // for jumps, add a split below the jump and above the target line
                    splits.add(i + 1);
                    int targetIndex = findInstWithPc(((JmpFact) fact).getTarget(), facts);
                    if (targetIndex != -1) splits.add(targetIndex);
                } else if (fact instanceof CJmpStmt) {
                    // for conditional jumps, add a split above the target line
                    int targetIndex = findInstWithPc(((CJmpStmt) fact).getTarget(), facts);
                    if (targetIndex != -1) splits.add(targetIndex);
                } else if (fact instanceof EnterSub) {
                    // for function headers, add a split before the header
                    splits.add(i);
                }
                else if (fact instanceof ExitSub) {
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
        private static int findInstWithPc(String pc, List<Stmt> facts) {
            if (pc.substring(0, 2).equals("__")) return -1; // TODO when jumping to a function e.g. goto @__gmon_start__

            for (int i = 0; i < facts.size(); i++) {
                if (facts.get(i).getLabel().getPc().equals(pc)) return i;
            }
            throw new AssumptionViolationException(String.format(
                    "Error in constructing flow graph: No facts.inst found with pc %s.\n", pc
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
        private static List<Block> createBlocksFromSplits(List<Integer> splits, List<Stmt> lines) {
            List<Block> blocks = new ArrayList<>();
            for (int i = 0; i < splits.size() - 1; i++) {
                // need to convert the sublist view to a real arraylist to avoid ConcurrentModificationException
                List<Stmt> blockLines = new ArrayList<>(lines.subList(splits.get(i), splits.get(i + 1)));
                // blocks are initially created with no children
                Block block = new Block(blockLines.get(0).getLabel().getPc(), blockLines, new ArrayList<>());
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
        private static void setChildren(List<Block> blocks, List<Stmt> facts) {
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
         * @param lines see {@link #setChildren}
         * @return a list of PCs representing the children of the given block
         */
        private static List<String> getChildrenPcs(Block block, List<Stmt> lines) {
            List<String> childrenPcs = new ArrayList<>();
            Stmt lastLine = block.lastLine();
            if (lastLine instanceof JmpFact) {
                // for jumps, simply add the target
                childrenPcs.add(((JmpFact) lastLine).getTarget());
            } else if (!(lastLine instanceof ExitSub)) {
                // for any other line that is not a function return, simply add the following line
                childrenPcs.add(lines.get(lines.indexOf(lastLine) + 1).getLabel().getPc());
            }
            // add conditional jumps. these will always be succeeded by a jump or conditional jump
            for (int i = block.lines.size() - 2; i >= 0; i--) {
                Stmt line = block.lines.get(i);
                if (!(line instanceof CJmpStmt)) break;
                childrenPcs.add(((CJmpStmt) line).getTarget());
            }
            return childrenPcs;
        }

        private static List<Function> convertBlocksToFunctions(List<Block> blocks) {
            List<Function> functions = new ArrayList<>();
            for (Block block : blocks) {
                Stmt firstLine = block.firstLine();
                if (firstLine instanceof EnterSub) {
                    Function function = new Function((EnterSub) firstLine, block);
                    functions.add(function);
                }
            }
            functions.forEach(function -> function.rootBlock.lines.remove(0));
            return functions;
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
                if (block.firstLine().getLabel().getPc().equals(pc)) {
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
     */
    public static class Block {
        // the label of this block
        private final String label;
        // the lines of code this block contains
        private List<Stmt> lines;
        // a list of other blocks this block may transition to
        private List<Block> children;

        /**
         * Constructor for a block.
         */
        Block(String label, List<Stmt> lines, List<Block> children) {
            this.label = label;
            this.lines = lines;
            this.children = children;
        }

        /**
         * The returned list may be mutated in order to update this block.
         * @return the list object containing lines of code this block contains
         */
        public List<Stmt> getLines() {
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
        public void setLines(List<Stmt> lines) {
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
        public Stmt firstLine() {
            return lines.get(0);
        }

        /**
         * A convenience method that returns the last line of this block.
         * @return the last line of this block
         */
        public Stmt lastLine() {
            return lines.get(lines.size() - 1);
        }

        /**
         * Reads through all lines of all blocks reachable (directly or indirectly) by this block and returns a list of
         * all these lines.
         * As such, this list may contain duplicate lines if flow graph properties are not properly constrained.
         *
         * @return all of the lines of all blocks reachable by this block - including all lines of this block
         */
        public List<Stmt> getLinesInCluster() {
            List<Stmt> allLines = new ArrayList<>(lines);
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

        // TODO blocks cant start with a number
        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("label").append(label).append(":\n");
            lines.forEach(line -> builder.append("  ").append(line).append("\n"));
            return builder.toString();
        }
    }
}
