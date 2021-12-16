package analysis;

import scala.collection.mutable.Map;
import scala.collection.mutable.Set;
import scala.collection.mutable.Stack;
import scala.collection.mutable.ArrayDeque;
import scala.jdk.CollectionConverters.IteratorHasAsScala;
import scala.jdk.CollectionConverters.ListHasAsScala;
import scala.jdk.CollectionConverters.SeqHasAsJava;
import java.util.MissingResourceException;

import astnodes.stmt.*;
import translating.FlowGraph;
import translating.FlowGraph.Block;
import translating.FlowGraph.Function;
import analysis.AnalysisPoint;

class InlineWorklist(analysis: AnalysisPoint, controlFlow: FlowGraph) {
    private final val debug: Boolean = true;

    // functions that we abstract away and don't traverse. If we encounter a call to any of these, it's 
    // passed through to the analyses as a call instruction so they can decide what to do with it
    val libraryFunctions: Set[String] = Set("malloc");

    var currentWorkListQueue: ArrayDeque[Block] = ArrayDeque(); // queue of blocks to work on, for the current function.

    var previousStmtAnalysisState: AnalysisPoint = null; // previous state on a per stmt basis.

    var finalAnalysedStmtInfo: Map[Stmt, AnalysisPoint] = Map(); // "output" info as the end result of the analysis
    var allBlockFinalAnalysisStates: Map[Block, AnalysisPoint] = Map(); // final states of all analysed blocks

    // clears out everything except analysedStmtInfo
    def finish = {
        currentWorkListQueue = null;
        previousStmtAnalysisState = null;
        allBlockFinalAnalysisStates = null;
    }

    def getAllStates: Map[Stmt, AnalysisPoint] = {
        finalAnalysedStmtInfo;
    }

    def getOneState(stmt: Stmt): AnalysisPoint = {
        finalAnalysedStmtInfo.getOrElse(stmt, analysis.createLowest);
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
        if (debug) {
            println("analysing function: " + functionName);
        }

        currentWorkListQueue = topologicalSortFromFunction(controlFlow, functionName);

        if (previousStmtAnalysisState == null) {
            previousStmtAnalysisState = analysis.createLowest;
        }

        var functionStartAnalysisState = previousStmtAnalysisState;
        var currentFunctionAnalysedInfo: Map[Stmt, AnalysisPoint] = Map();
        
        while(!currentWorkListQueue.isEmpty) {
            var nextBlockToAnalyse: Block = currentWorkListQueue.removeHead();
            
            // for blocks *with* parents (i.e. not function start blocks) we take the previous state to be the union
            // of all parents' final states.
            if (!findBlockParents(nextBlockToAnalyse).isEmpty) {
                previousStmtAnalysisState = analysis.createLowest;

                findBlockParents(nextBlockToAnalyse).foreach(nextBlockToAnalyseParent => {
                    var nextBlockToAnalyseParentFinalState: AnalysisPoint = allBlockFinalAnalysisStates.getOrElse(nextBlockToAnalyseParent, analysis.createLowest);

                    previousStmtAnalysisState = previousStmtAnalysisState.union(nextBlockToAnalyseParentFinalState);
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
    def workOnBlock(blockToWorkOn: Block, currentFunctionAnalysedInfo: Map[Stmt, AnalysisPoint]): Unit = {
        if (debug) {
            println("analysing block: " + blockToWorkOn.toString);
        }

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
    def workOnStmt(singleStmt: Stmt, functionAnalysedInfo: Map[Stmt, AnalysisPoint]): Unit = {
        if (debug) {
            println("analysing stmt: " + singleStmt.toString);
        }

        singleStmt match {
            case functionCallStmt: CallStmt => {
                // if we have a function call, pause the current analysis and analyse the given function
                // effectively just inlines every function at every time it's called
                var inProgressWorkListQueue: ArrayDeque[Block] = currentWorkListQueue;

                if (!libraryFunctions.contains(functionCallStmt.funcName)) {
                    workOnFunction(functionCallStmt.funcName);
                } else {
                    // treat it like a normal statement and let the analyses define how they deal with it
                    previousStmtAnalysisState = previousStmtAnalysisState.transfer(singleStmt);
                
                    // if anything already exists for this stmt, replace it.
                    if (functionAnalysedInfo.getOrElse(singleStmt, null) != null) {
                        functionAnalysedInfo.remove(singleStmt);
                    }

                    functionAnalysedInfo.update(singleStmt, previousStmtAnalysisState);
                }

                currentWorkListQueue = inProgressWorkListQueue;
            }
            case _ => {
                previousStmtAnalysisState = previousStmtAnalysisState.transfer(singleStmt);

                // if anything already exists for this stmt, replace it.
                if (functionAnalysedInfo.getOrElse(singleStmt, null) != null) {
                    functionAnalysedInfo.remove(singleStmt);
                }

                functionAnalysedInfo.update(singleStmt, previousStmtAnalysisState);
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

    def saveNewAnalysisInfo(functionAnalysedInfo: Map[Stmt, AnalysisPoint]) = {
        functionAnalysedInfo.keys.foreach(currentFunctionAnalysisPoint => {
            finalAnalysedStmtInfo.update(currentFunctionAnalysisPoint, finalAnalysedStmtInfo.getOrElse(currentFunctionAnalysisPoint, analysis.createLowest).union(functionAnalysedInfo.getOrElse(currentFunctionAnalysisPoint, analysis.createLowest)));
        });
    }
}