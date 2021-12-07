package analysis

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;
import scala.collection.mutable.Stack;
import scala.jdk.CollectionConverters.IteratorHasAsScala;
import scala.jdk.CollectionConverters.ListHasAsScala;
import scala.jdk.CollectionConverters.ListHasAsScala;
import scala.jdk.CollectionConverters.SeqHasAsJava;
import java.util.MissingResourceException;

import astnodes.stmt.Stmt;
import translating.FlowGraph;
import translating.FlowGraph.Block;
import analysis.AnalysisPoint;

class BlockWorklist(analyses: Set[AnalysisPoint], controlFlow: FlowGraph) {
    var workListQueue: Iterator[Block] = ???;
    var prevState: Set[AnalysisPoint] = createAnalysisEmpty;

    var analysedStmtInfo: HashMap[Stmt, Set[AnalysisPoint]] = ???;
    var blockFinalStates: HashMap[Block, Set[AnalysisPoint]] = HashMap();

    def createAnalysisEmpty: Set[AnalysisPoint] = {
        analyses.map(a => a.createLowest);
    }

    def getAllStates: HashMap[Stmt, Set[AnalysisPoint]] = {
        analysedStmtInfo;
    }

    def getOneState(stmt: Stmt): Set[AnalysisPoint] = {
        analysedStmtInfo.getOrElse(stmt, createAnalysisEmpty);
    }

    /**
     * Sets up the flowgraph into a couple different structures for convenient information, then analyses
     * the blocks according to a queue.
     * 
     * N.B: Because of the way worklist algorithms work, any blocks that depend on a loop will be re-computed
     * every time the loop is computed until that loop is stable (i.e. further iterations make no changes).
     * Ideally, we could analyse the loop on it's own and ignore the children until we know it is stable.
     * 
     * For small programs, this is negligible, but the worst-case is having a large, branching structure with many
     * blocks that all depend on a loop; forcing us to re-analyse every block until that loop stablises.
     */
    def workOnBlocks = {
        workListQueue = topologicalSort(controlFlow); // topo sort with rm back-edges, save as iterator - depth-first search

        while (workListQueue.hasNext) {
            var nextBlock: Block = workListQueue.next;

            // clear prevState
            prevState = createAnalysisEmpty;

            // for each parent
            findParents(nextBlock).foreach(parent => {
                // if the block hasn't been analysed, getOrElse becomes useful and gives us an empty set.
                // empty set means the next loop gets skipped and we go straight to the next parent.
                var parentFinalState: Set[AnalysisPoint] = blockFinalStates.getOrElse(parent, Set());

                // for each analysis of the parent
                parentFinalState.foreach(parentAnalysisPoint => {
                    var analysisFound: Boolean = false;

                    // parentAnalysisPoint: AnalysisPoint[ValueAnalysis] = ValueAnalysis(foo);
                    
                    // if an analysis of that type is in prevState, update it with the union of the two
                    prevState.foreach({
                        case prevAnalysisPoint: parentAnalysisPoint.type => {
                            prevState.remove(prevAnalysisPoint);
                            prevState.add(prevAnalysisPoint.union(parentAnalysisPoint));
                            analysisFound = false;
                        }
                    });
                    
                    // otherwise, add it to prevState
                    if (!analysisFound) {
                        prevState.add(parentAnalysisPoint);
                    }
                });
            });

            // prevState is now the union of all parent's analyses.
            analyseSingleBlock(nextBlock);
        }
    }

    def findParents(block: Block) = {
        var output: Set[Block] = Set();

        controlFlow.getBlocks.asScala.foreach(b => {
            if (b.getChildren.asScala.contains(block)) {
                output.add(b);
            }
        });

        output;
    }

    /**
     * Analyses a block (full of statements) by analysing the statements in getLines().
     * 
     * Updates the blockFinalStates map with the prevState at the end of the lines, and adds all block children
     * to queue on update, if they weren't already there.
     */
    def analyseSingleBlock(block: Block) = {
        block.getLines.asScala.foreach(l => {
            analyseSinglePoint(l);
        });
        
        var currentFinalBlockState = blockFinalStates.getOrElse(block, null);

        if (currentFinalBlockState != null) {
            if (currentFinalBlockState != prevState) {
                blockFinalStates.remove(block);
                blockFinalStates.concat(HashMap(block -> prevState));
                
                // if queue doesn't contain child, add child, *and* if queue doesn't contain this, add this
                if (!workListQueue.contains(block)) {
                    workListQueue ++ Iterator(block);
                }

                block.getChildren.asScala.foreach(c => {
                    if (!workListQueue.contains(c)) {
                        workListQueue ++ Iterator(c);
                    }
                });
            }
        } else {
            blockFinalStates.concat(HashMap(block -> prevState));

            // if queue doesn't contain child, add child, *and* if queue doesn't contain this, add this
            if (!workListQueue.contains(block)) {
                workListQueue ++ Iterator(block);
            }
            
            block.getChildren.asScala.foreach(c => {
                if (!workListQueue.contains(c)) {
                    workListQueue ++ Iterator(c);
                }
            });
        }
    }

    /**
     * Analyses a single statement, from the known previous state.
     * 
     * Saves the new "prevState" and updates the analysedStmtInfo map.
     */
    def analyseSinglePoint(stmt: Stmt) = {
        var newAnalysedPoint: Set[AnalysisPoint] = Set[AnalysisPoint]();

        prevState.foreach(p => {
            newAnalysedPoint.add(p.transfer(stmt));
        });

        // if anything already exists for this stmt, replace it.
        if (analysedStmtInfo.getOrElse(stmt, null) != null) {
            analysedStmtInfo.remove(stmt);
        }
        analysedStmtInfo.concat(HashMap(stmt -> newAnalysedPoint));

        prevState = newAnalysedPoint;
    }

    /**
     * Takes a FlowGraph (w/r/t code "blocks") and returns a copy of it, sorted ideally for analysis.
     * 
     * Do a depth-first search, removing back-edges as we see them. Once every child of a node has been finished,
     * append that node to the *start* of the output iterator.
     * Output list is now a topologically ordered representation of the graph. Tada!
     */
    def topologicalSort(controlFlow: FlowGraph): Iterator[Block] = {
        var nodeStack: Stack[Block] = new Stack[Block]();
        var visited: List[Block] = List[Block]();
        var output: Iterator[Block] = Iterator[Block]();
        var rmChildren: Map[Block, List[Block]] = Map[Block, List[Block]]();

        nodeStack.addOne(controlFlow.getBlocks.get(0));

        while (!nodeStack.isEmpty) {
            var vertex: Block = nodeStack.pop;

            if (!visited.contains(vertex)) {
                visited.concat(List(vertex));
            }
            
            vertex.getChildren.asScala.foreach(c => {
                if (visited.contains(c)) {
                    // note that this portion of the code will change significantly once FlowGraph is refactored.
                    // The basic approach is there, though.
                    vertex.children.remove(c);

                    // remove children so that we don't have cycles, but remember to keep them around so we can return
                    // the graph to it's original state further down
                    if (rmChildren.contains(vertex)) {
                        rmChildren.get(vertex).concat(List(c));
                    } else {
                        rmChildren.concat(HashMap[Block, List[Block]](vertex -> List(c)));
                    }
                } else {
                    nodeStack.addOne(c);
                }
            });

            // add blocks to the beginning of an iterator as we finish all their children
            if (vertex.getChildren.isEmpty) {
                output = Iterator(vertex) ++ output;
            }
        }

        rmChildren.keys.foreach(k => {
            // scala thinks we need getOrElse here despite the fact we're literally iterating over known keys
            k.children = rmChildren.getOrElse(k, List()).asJava;
        });
        
        output;
    }
}

class FunctionWorklist(analyses: Set[AnalysisPoint], controlFlow: FlowGraph) {

}