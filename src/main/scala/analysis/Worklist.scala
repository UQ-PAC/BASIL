package analysis;

import scala.collection.mutable.Map;   // functional programming is silly when computers are machines with memory (state).
import scala.collection.mutable.Set;   // why have state if you're not going to use it?
import scala.collection.mutable.Stack; // hence, mutability in everything is good
import scala.collection.mutable.ArrayDeque;
import scala.jdk.CollectionConverters.IteratorHasAsScala;
import scala.jdk.CollectionConverters.ListHasAsScala;
import scala.jdk.CollectionConverters.ListHasAsScala;
import scala.jdk.CollectionConverters.SeqHasAsJava;
import java.util.MissingResourceException;

import astnodes.stmt.*;
import translating.FlowGraph;
import translating.FlowGraph.Block;
import translating.FlowGraph.Function;
import analysis.AnalysisPoint;
import java.lang.invoke.CallSite

class BlockWorklist(analyses: Set[AnalysisPoint], controlFlow: FlowGraph) {
    var currentWorkListQueue: ArrayDeque[Block] = ArrayDeque(); // queue of blocks to work on, for the current function.

    var previousStmtAnalysisState: Set[AnalysisPoint] = null; // previous state on a per stmt basis.

    var finalAnalysedStmtInfo: Map[Stmt, Set[AnalysisPoint]] = Map(); // "output" info as the end result of the analysis
    var allBlockFinalAnalysisStates: Map[Block, Set[AnalysisPoint]] = Map(); // final states of all analysed blocks

    // clears out everything except analysedStmtInfo
    def finish = {
        currentWorkListQueue = null;
        previousStmtAnalysisState = null;
        allBlockFinalAnalysisStates = null;
    }

    def createAnalysisEmpty: Set[AnalysisPoint] = {
        analyses.map(analysis => analysis.createLowest);
    }

    def getAllStates: Map[Stmt, Set[AnalysisPoint]] = {
        finalAnalysedStmtInfo;
    }

    def getOneState(stmt: Stmt): Set[AnalysisPoint] = {
        finalAnalysedStmtInfo.getOrElse(stmt, createAnalysisEmpty);
    }

    /**
     * Standard "start from main" function.
     */
    def analyseFromMain = {
        workOnFunction("main");
        finish;
    }

    /**
     * Generic function analysis function.
     * 
     * N.B. Because of the way worklist algorithms work, any blocks that depend on a loop will be re-computed
     * every time the loop is computed until that loop is stable (i.e. further iterations make no changes).
     * Ideally, we could analyse the loop on it's own and ignore the children until we know it is stable.
     * 
     * For small programs, this is negligible, but the worst-case is having a large, branching structure with many
     * blocks that all depend on a loop; forcing us to re-analyse every block until that loop stablises.
     */
    def workOnFunction(functionName: String): Unit = {
        println("analysing function: " + functionName);

        currentWorkListQueue = topologicalSortFromFunction(controlFlow, functionName);

        if (previousStmtAnalysisState == null) {
            previousStmtAnalysisState = createAnalysisEmpty;
        }

        var functionStartAnalysisState = previousStmtAnalysisState;
        var currentFunctionAnalysedInfo: Map[Stmt, Set[AnalysisPoint]] = Map();
        
        while(!currentWorkListQueue.isEmpty) {
            var nextBlockToAnalyse: Block = currentWorkListQueue.removeHead();
            
            // for blocks *with* parents (i.e. not function start blocks) we take the previous state to be the union
            // of all parents' final states.
            if (!findBlockParents(nextBlockToAnalyse).isEmpty) {
                previousStmtAnalysisState = createAnalysisEmpty;

                findBlockParents(nextBlockToAnalyse).foreach(nextBlockToAnalyseParent => {
                    var nextBlockToAnalyseParentFinalState: Set[AnalysisPoint] = allBlockFinalAnalysisStates.getOrElse(nextBlockToAnalyseParent, Set());

                    // for every parent final state
                    nextBlockToAnalyseParentFinalState.foreach(nextBlockToAnalyseParentPoint => {
                        var analysisFound: Boolean = false;

                        // for every current final state
                        previousStmtAnalysisState.foreach(previousStmtAnalysisPoint => {
                            if (previousStmtAnalysisPoint.getClass == nextBlockToAnalyseParentPoint.getClass) {

                                previousStmtAnalysisState.remove(previousStmtAnalysisPoint);
                                previousStmtAnalysisState.add(previousStmtAnalysisPoint.union(nextBlockToAnalyseParentPoint));
                                analysisFound = true;

                            }
                        });
                        
                        // if there's no matches, then add it
                        if (!analysisFound) {
                            previousStmtAnalysisState.add(nextBlockToAnalyseParentPoint);
                        }
                    });
                });
            }

            workOnBlock(nextBlockToAnalyse, currentFunctionAnalysedInfo);

            if (!currentWorkListQueue.isEmpty) {
                // if it's gonna analyse another block, we need to reset the previous state
                previousStmtAnalysisState = functionStartAnalysisState;
            }
        }

        // Once the entire function has been analysed to stability, save and/or update the info in the "output" map.
        saveNewAnalysisInfo(currentFunctionAnalysedInfo);
    }

    /**
     * Analyses a block (of statements) by analysing the statements in getLines(). We assume that getLines() is in execution order.
     * 
     * Updates the blockFinalStates map with the prevState at the end of the lines, and adds all block children
     * to queue on update, if they weren't already there.
     */
    def workOnBlock(blockToWorkOn: Block, currentFunctionAnalysedInfo: Map[Stmt, Set[AnalysisPoint]]): Unit = {
        println("analysing block: " + blockToWorkOn.toString);

        blockToWorkOn.getLines.asScala.foreach(blockStmtLine => {
            workOnStmt(blockStmtLine, currentFunctionAnalysedInfo);
        });
        
        var currentFinalBlockState = allBlockFinalAnalysisStates.getOrElse(blockToWorkOn, null);

        if (currentFinalBlockState != null) {
            if (!(currentFinalBlockState.toString == previousStmtAnalysisState.toString)) { // TODO: fix this to be not string comparison
                allBlockFinalAnalysisStates.remove(blockToWorkOn);
                allBlockFinalAnalysisStates.update(blockToWorkOn, previousStmtAnalysisState);
                
                // if queue doesn't contain child, add child, *and* if queue doesn't contain this, add this
                if (!currentWorkListQueue.contains(blockToWorkOn)) {
                    currentWorkListQueue.append(blockToWorkOn);
                }

                blockToWorkOn.getChildren.asScala.foreach(c => {
                    if (!currentWorkListQueue.contains(c)) {
                        currentWorkListQueue.append(c);
                    }
                });
            }
        } else {
            allBlockFinalAnalysisStates.update(blockToWorkOn, previousStmtAnalysisState);

            // if queue doesn't contain child, add child, *and* if queue doesn't contain this, add this
            if (!currentWorkListQueue.contains(blockToWorkOn)) {
                currentWorkListQueue.append(blockToWorkOn);
            }
            
            blockToWorkOn.getChildren.asScala.foreach(c => {
                if (!currentWorkListQueue.contains(c)) {
                    currentWorkListQueue.append(c);
                }
            });
        }
    }

    /**
     * Analyses a single statement, from the known previous state.
     * 
     * Saves the new "prevState" and updates the analysedStmtInfo map.
     */
    def workOnStmt(singleStmt: Stmt, functionAnalysedInfo: Map[Stmt, Set[AnalysisPoint]]): Unit = {
        println("analysing stmt: " + singleStmt.toString);
        println(previousStmtAnalysisState);
        print("\n\n");

        var newAnalysedPoint: Set[AnalysisPoint] = Set[AnalysisPoint]();

        singleStmt match {
            case functionCallStmt: CallStmt => {
                // if we have a function call, pause the current analysis and analyse the given function
                // effectively just inlines every function at every time it's called
                var inProgressWorkListQueue: ArrayDeque[Block] = currentWorkListQueue;

                workOnFunction(functionCallStmt.funcName);

                currentWorkListQueue = inProgressWorkListQueue;
            }
            case _ => {
                previousStmtAnalysisState.foreach(p => {
                    newAnalysedPoint.add(p.transfer(singleStmt));
                });

                // if anything already exists for this stmt, replace it.
                if (functionAnalysedInfo.getOrElse(singleStmt, null) != null) {
                    functionAnalysedInfo.remove(singleStmt);
                }
                functionAnalysedInfo.update(singleStmt, newAnalysedPoint);

                previousStmtAnalysisState = newAnalysedPoint;
            }
        }
    }

    /**
     * Takes a FlowGraph (w/r/t code "blocks") and returns a copy of it, sorted ideally for analysis.
     * 
     * Do a depth-first search, removing back-edges as we see them. Once every child of a node has been finished,
     * append that node to the *start* of the output iterator.
     * Output list is now a topologically ordered representation of the graph. Tada!
     */    
    def topologicalSortFromFunction(controlFlow: FlowGraph, functionName: String): ArrayDeque[Block] = {
        // initialise our references to stuff
        var sorted = ArrayDeque[Block]();
        var rmChildren = Map[Block, List[Block]]();
        var dfsPath = Set[Block]();

        // recursive DFS from main node
        var output = dfsHelper(
            controlFlow.getFunctions.asScala.toList.find((func: Function) => {
                func.getHeader.getFuncName == functionName;
            }).get.getBlocks.asScala.head, rmChildren, dfsPath, sorted
        );

        // add back all the cycles we removed
        rmChildren.keys.foreach(key => {
            rmChildren.getOrElse(key, List[Block]()).foreach(rmdChild => {
                key.children.add(rmdChild);
            })
        });

        output;
    }

    def dfsHelper(node: Block, rmChildren: Map[Block, List[Block]], dfsPath: Set[Block], sorted: ArrayDeque[Block]): ArrayDeque[Block] = {
        dfsPath.concat(Set(node));

        node.getChildren.asScala.foreach(child => {
            if (dfsPath.contains(child)) {
                // if backedge, add to our rmChildren list. we can't remove them yet cause ConcurrentModificationException

                if (rmChildren.contains(node)) {
                    rmChildren.update(node, (rmChildren.getOrElse(node, null) ++ List(child)));
                } else {
                    rmChildren.update(node, List(child));
                }
            } else {
                // or not on path, so recurse

                dfsHelper(child, rmChildren, dfsPath, sorted);
            }
        });

        // remove rmChildren
        rmChildren.getOrElse(node, List()).foreach(cycle => {
            node.children.remove(cycle);
        })

        dfsPath.remove(node);

        if (!sorted.contains(node)) {
            sorted.prepend(node);
        }
        sorted;
    }

    def findBlockParents(block: Block) = {
        var output: Set[Block] = Set();

        controlFlow.getBlocks.asScala.foreach(b => {
            if (b.getChildren.asScala.contains(block)) {
                output.add(b);
            }
        });

        output;
    }

    def saveNewAnalysisInfo(functionAnalysedInfo: Map[Stmt, Set[AnalysisPoint]]) = {
        functionAnalysedInfo.keys.foreach(updStmt => { // for every key in new analysis map
            var curStmtVal: Set[AnalysisPoint] = finalAnalysedStmtInfo.getOrElse(updStmt, Set());

            if (curStmtVal.isEmpty) {
                finalAnalysedStmtInfo.update(updStmt, functionAnalysedInfo.getOrElse(updStmt, Set())); // if empty, add the new analysis
            } else {
                var newStmtVal: Set[AnalysisPoint] = Set();

                // for every updated stmt
                functionAnalysedInfo.getOrElse(updStmt, Set()).foreach(updAnalysis => {
                    var analysisFound: Boolean = false;

                    // for every old stmt
                    curStmtVal.foreach(curAnalysis => {
                        if (curAnalysis.getClass == updAnalysis.getClass) {
                            // if they match classes then take union
                            newStmtVal.add(curAnalysis.union(updAnalysis));
                            analysisFound = true;
                        }
                    });

                    if (!analysisFound) {
                        // otherwise just add the new one
                        newStmtVal.add(updAnalysis);
                    }
                });

                finalAnalysedStmtInfo.remove(updStmt);
                finalAnalysedStmtInfo.update(updStmt, newStmtVal);
            }
        });
    }
}

class FunctionWorklist(analyses: Set[AnalysisPoint], controlFlow: FlowGraph) {

}
