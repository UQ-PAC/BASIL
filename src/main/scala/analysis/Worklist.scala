package analysis

import scala.collection.mutable.HashMap;
import scala.collection.mutable.Set;
import scala.collection.mutable.Stack;
import scala.jdk.CollectionConverters.IteratorHasAsScala;
import scala.jdk.CollectionConverters.ListHasAsScala;
import java.util.MissingResourceException;

import astnodes.stmt.Stmt;
import translating.FlowGraph;
import translating.FlowGraph.Block;
import analysis.AnalysisPoint;

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
            prevState = ???; // union of all parent's final states; unknown final states are considered as "no information" and add nothing to the union

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
     * Takes a FlowGraph (w/r/t code "blocks") and returns a copy of it, sorted ideally for analysis.
     * 
     * Do a depth-first search, removing back-edges as we see them. Once every node's children have been finished,
     * append it to the output list. If this list is FILO, we have a topological sort.
     */
    def topologicalSort(controlFlow: FlowGraph): Iterator[Block] = {
        var nodeStack: Stack[Block] = new Stack[Block]();
        var visited: List[Block] = List[Block]();
        var output: Iterator[Block] = Iterator[Block]();

        nodeStack.addOne(controlFlow.getBlocks.get(0));

        while (!nodeStack.isEmpty) {
            var vertex = nodeStack.pop;
            
            vertex.getChildren.asScala.foreach(c => {
                if (visited.contains(c)) {
                    // note that this portion of the code will change significantly once FlowGraph is refactored.
                    // The basic approach is there, though.
                    vertex.children.remove(c);
                } else {
                    nodeStack.addOne(c);
                }
            });

            if (vertex.getChildren.isEmpty) {
                output = Iterator(vertex) ++ output;
            }
        }

        output;
    }
}

class FunctionWorklist(analyses: Set[AnalysisPoint[_]], controlFlow: FlowGraph) {

}