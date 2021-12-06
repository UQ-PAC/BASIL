package analysis

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;
import scala.collection.mutable.ListBuffer;
import scala.jdk.CollectionConverters.IteratorHasAsScala;
import scala.jdk.CollectionConverters.ListHasAsScala;
import java.util.MissingResourceException;

import astnodes.stmt.Stmt;
import translating.FlowGraph;
import translating.FlowGraph.Block;
import analysis.AnalysisPoint;
import java.util.Stack

class BlockWorklist(analyses: Set[AnalysisPoint[_]], controlFlow: FlowGraph) {
    var workListQueue: Iterator[Block] = ???;
    var prevState: Set[AnalysisPoint[_]] = createAnalysisEmpty;

    var analysedStmtInfo: HashMap[Stmt, Set[AnalysisPoint[_]]] = ???;
    var blockFinalStates: HashMap[Block, Set[AnalysisPoint[_]]] = HashMap();

    def createAnalysisEmpty: Set[AnalysisPoint[_]] = {
        analyses.map(a => a.createLowest);
    }

    def getAllStates: HashMap[Stmt, Set[AnalysisPoint[_]]] = {
        analysedStmtInfo;
    }

    def getOneState(stmt: Stmt): Set[AnalysisPoint[_]] = {
        analysedStmtInfo.getOrElse(stmt, createAnalysisEmpty);
    }

    /**
     * Sets up the flowgraph into a couple different structures for convenient information, then analyses
     * the blocks according to a queue.
     */
    def workOnBlocks = {
        var acyclic = ???; // Remove back-edge of all cycles in cfg - depth-first search
        workListQueue = topologicalSort(controlFlow); // topo sort acyclic, save as iterator - depth-first search 2

        while (workListQueue.hasNext) {
            var nextBlock: Block = workListQueue.next;
            prevState = ???; // union of all *acyclic* block's parent's final states. If any parent is somehow un-analysed, throw an exception because this shouldn't happen.

            analyseSingleBlock(nextBlock);
        }
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
                
                if (!workListQueue.contains(block)) {
                    workListQueue ++ Iterator(block.getChildren.asScala);
                }
            }
        } else {
            blockFinalStates.concat(HashMap(block -> prevState));
        }
    }

    /**
     * Analyses a single statement, from the known previous state.
     * 
     * Saves the new "prevState" and updates the analysedStmtInfo map.
     */
    def analyseSinglePoint(stmt: Stmt) = {
        var newAnalysedPoint: Set[AnalysisPoint[_]] = Set[AnalysisPoint[_]]();

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
     * Takes a FlowGraph (w/r/t code "blocks") and returns a copy of it, having removed every back-edge
     * by iterative depth-first-search.
     */
    def decycleFlowGraph(controlFlow: FlowGraph): FlowGraph = {
        var nodeStack: Stack[Block] = new Stack[Block]();
        var discovered: ListBuffer[Block] = new ListBuffer[Block]();

        // add first node to S

        while (!nodeStack.isEmpty) {
            var vertex = nodeStack.pop;
            discovered += vertex;
            
            // for every edge from V
            // if the edge leads to discovered, remove it
            // otherwise, add it to nodeStack.
        }

        controlFlow;
    }

    def topologicalSort(controlFlow: FlowGraph): Iterator[Block] = {
        controlFlow.getBlocks.iterator.asScala;
    }
}

class FunctionWorklist(analyses: Set[AnalysisPoint[_]], controlFlow: FlowGraph) {

}