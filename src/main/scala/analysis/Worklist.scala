package analysis;

import scala.collection.mutable.Map;
import scala.collection.mutable.Set;
import scala.collection.mutable.Stack;
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

class BlockWorklist(analyses: Set[AnalysisPoint], controlFlow: FlowGraph) {
  var workListQueue: ArrayDeque[Block] = ArrayDeque();
  var prevState: Set[AnalysisPoint] = createAnalysisEmpty;

  var analysedStmtInfo: Map[Stmt, Set[AnalysisPoint]] = Map();
  var blockFinalStates: Map[Block, Set[AnalysisPoint]] = Map();

  def createAnalysisEmpty: Set[AnalysisPoint] = {
    analyses.map(a => a.createLowest);
  }

  def getAllStates: Map[Stmt, Set[AnalysisPoint]] = {
    analysedStmtInfo;
  }

  def getOneState(stmt: Stmt): Set[AnalysisPoint] = {
    analysedStmtInfo.getOrElse(stmt, createAnalysisEmpty);
  }

  /** Sets up the flowgraph into a couple different structures for convenient information, then analyses the blocks
    * according to a queue.
    *
    * N.B: Because of the way worklist algorithms work, any blocks that depend on a loop will be re-computed every time
    * the loop is computed until that loop is stable (i.e. further iterations make no changes). Ideally, we could
    * analyse the loop on it's own and ignore the children until we know it is stable.
    *
    * For small programs, this is negligible, but the worst-case is having a large, branching structure with many blocks
    * that all depend on a loop; forcing us to re-analyse every block until that loop stablises.
    */
  def workOnBlocks = {
    workListQueue = topologicalSort(controlFlow); // topo sort with rm back-edges, save as iterator - depth-first search
    var break: Int = 0;

    while (!workListQueue.isEmpty) {
      var nextBlock: Block = workListQueue.removeHead();

      // clear prevState
      prevState = createAnalysisEmpty;

      // for each parent
      findParents(nextBlock).foreach(parent => {
        // if the block hasn't been analysed, getOrElse becomes useful and gives us an empty set.
        // empty set means the next loop gets skipped and we go straight to the next parent.
        var parentFinalState: Set[AnalysisPoint] = blockFinalStates.getOrElse(parent, Set());

        // for every parent final state
        parentFinalState.foreach(parentAnalysisPoint => {
          var analysisFound: Boolean = false;

          // for every current final state
          prevState.foreach(prevAnalysisPoint => {
            if (prevAnalysisPoint.getClass == parentAnalysisPoint.getClass) {
              prevState.remove(prevAnalysisPoint);
              prevState.add(prevAnalysisPoint.union(parentAnalysisPoint));
              analysisFound = true;
            }
          });

          // if there's no matches, then add it
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

  /** Analyses a block (full of statements) by analysing the statements in getLines().
    *
    * Updates the blockFinalStates map with the prevState at the end of the lines, and adds all block children to queue
    * on update, if they weren't already there.
    */
  def analyseSingleBlock(block: Block) = {
    println("analysing block: " + block.toString);
    block.getLines.asScala.foreach(l => {
      analyseSinglePoint(l);
    });

    var currentFinalBlockState = blockFinalStates.getOrElse(block, null);

    if (currentFinalBlockState != null) {
      if (!(currentFinalBlockState.toString == prevState.toString)) { // TODO: fix this
        blockFinalStates.remove(block);
        blockFinalStates.update(block, prevState);

        // if queue doesn't contain child, add child, *and* if queue doesn't contain this, add this
        if (!workListQueue.contains(block)) {
          workListQueue.append(block);
        }

        block.getChildren.asScala.foreach(c => {
          if (!workListQueue.contains(c)) {
            workListQueue.append(c);
          }
        });
      }
    } else {
      blockFinalStates.update(block, prevState);

      // if queue doesn't contain child, add child, *and* if queue doesn't contain this, add this
      if (!workListQueue.contains(block)) {
        workListQueue = workListQueue.append(block);
      }

      block.getChildren.asScala.foreach(c => {
        if (!workListQueue.contains(c)) {
          workListQueue = workListQueue.append(c);
        }
      });
    }
  }

  /** Analyses a single statement, from the known previous state.
    *
    * Saves the new "prevState" and updates the analysedStmtInfo map.
    */
  def analyseSinglePoint(stmt: Stmt) = {
    println("analysing stmt: " + stmt.toString);
    var newAnalysedPoint: Set[AnalysisPoint] = Set[AnalysisPoint]();

    prevState.foreach(p => {
      newAnalysedPoint.add(p.transfer(stmt));
    });

    // if anything already exists for this stmt, replace it.
    if (analysedStmtInfo.getOrElse(stmt, null) != null) {
      analysedStmtInfo.remove(stmt);
    }
    analysedStmtInfo.update(stmt, newAnalysedPoint);

    prevState = newAnalysedPoint;
  }

  /** Takes a FlowGraph (w/r/t code "blocks") and returns a copy of it, sorted ideally for analysis.
    *
    * Do a depth-first search, removing back-edges as we see them. Once every child of a node has been finished, append
    * that node to the *start* of the output iterator. Output list is now a topologically ordered representation of the
    * graph. Tada!
    */

  // use scope as a pass-by-reference facsimile because I cba passing through and returning from the recursive method
  var rmChildren: Map[Block, List[Block]] = null;
  var dfsPath: List[Block] = null;
  var sorted: ArrayDeque[Block] = null;

  def topologicalSort(controlFlow: FlowGraph): ArrayDeque[Block] = {
    // initialise our references to stuff
    sorted = ArrayDeque[Block]();
    rmChildren = Map[Block, List[Block]]();
    dfsPath = List[Block]();

    // recursive DFS from main node
    var output = dfsHelper(
      controlFlow.getFunctions.asScala.toList
        .find((func: Function) => {
          func.getHeader.funcName == "main";
        })
        .get
        .getBlocks
        .asScala
        .head
    );

    // add back all the cycles we removed
    rmChildren.keys.foreach(key => {
      rmChildren
        .getOrElse(key, List[Block]())
        .foreach(rmdChild => {
          key.children.add(rmdChild);
        })
    });

    // clear cause no-one should ever use again
    rmChildren = null;
    dfsPath = null;
    sorted = null

    output;
  }

  def dfsHelper(node: Block): ArrayDeque[Block] = {
    dfsPath = dfsPath ++ List(node);

    node.getChildren.asScala.foreach(child => {
      if (dfsPath.contains(child)) {
        // if backedge, add to our rmChildren list

        if (rmChildren.contains(node)) {
          rmChildren.update(node, (rmChildren.getOrElse(node, null) ++ List(child)));
        } else {
          rmChildren.update(node, List(child));
        }
      } else {
        // or not on path, so recurse

        dfsHelper(child);
      }
    });

    // remove rmChildren
    rmChildren
      .getOrElse(node, List())
      .foreach(cycle => {
        node.children.remove(cycle);
      })

    dfsPath = dfsPath.filter(_ != node);

    if (!sorted.contains(node)) {
      sorted.prepend(node);
    }
    sorted;
  }
}

class FunctionWorklist(analyses: Set[AnalysisPoint], controlFlow: FlowGraph) {}
