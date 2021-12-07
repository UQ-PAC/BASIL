import astnodes.exp.{Expr, Var}
import astnodes.stmt.assign.Assign
import translating.FlowGraph
import vcgen.Block

import java.lang.invoke.MethodHandles.loop
import scala.util.control.Breaks._
import java.util
import java.util.{HashMap, LinkedList, List}
import scala.util.control.Breaks

//TODO: all of these functions use object methods I don't know if will remain once moved to scala
//TODO: needs a top & bottom lattice object
class ConstantPropagation(functions: List[FlowGraph.Function]) {

  /**
   * Performs constant propagation on all functions given
   */
  def foldVariables(): Unit = {
    functions.forEach(function => {
      val blockStates =
        propagationWorklistAlgorithm(function.getBlocks)
      constantPropagation(blockStates, function.getBlocks)
    })
  }

  /**
   * Uses the propagation worklist algorithm to calculate variable constraints on each block.
   * @param blocks
   * @return
   */
  private def propagationWorklistAlgorithm(blocks: util.List[FlowGraph.Block]):
  util.HashMap[FlowGraph.Block, util.HashMap[Var, Expr]] = {
    // the most straightforward way I can think of storing the blocks abstract states is
    // creating a map from each block to another map that stores the variable to the expression
    val blockAbstractState = new util.HashMap[FlowGraph.Block, util.HashMap[Var, Expr]]
    val predecessorMap = findBlockPredecessors(blocks.get(0))
    val worklist = new util.LinkedList[FlowGraph.Block]
    worklist.add(blocks.get(0))
    while ( {
      worklist.size > 0
    }) {
      val next = worklist.poll
      // if the block has more than one incoming edge, perform a meet
      if (predecessorMap.get(next).size > 0) meetFunction(blockAbstractState, next, predecessorMap.get(next))
      transferFunction(next, blockAbstractState.get(next))
      // for each dependent block, if a join with it and the current results changes the state
      // of the child, add it to the worklist queue
      next.getChildren.forEach(child => {
        if (!joinFunction(blockAbstractState, next, predecessorMap.get(next).get(0))) worklist.add(child)
      })
    }

    blockAbstractState
  }

  /**
   * Returns a map of all blocks to a list of their predecessors.
   *
   * @param root the first block executed by the function
   * @return a map from blocks to a list of their predecessors
   */
  private def findBlockPredecessors(root: FlowGraph.Block) = {
    val predecessorMap = new util.HashMap[FlowGraph.Block, util.List[FlowGraph.Block]]
    // the queue to expand blocks children in the search
    val FIFO = new util.LinkedList[FlowGraph.Block]
    // the blocks already expanded
    val visited = new util.LinkedList[FlowGraph.Block]
    FIFO.add(root)
    while ( {
      FIFO.size > 0
    }) {
      val parent = FIFO.poll
      visited.add(parent)
      parent.getChildren.forEach(child =>  { // add to the map if not already added
        if (!predecessorMap.containsKey(child)) {
          predecessorMap.put(child, new util.LinkedList[FlowGraph.Block])
          predecessorMap.get(child).add(parent)
        }
        else if (!predecessorMap.get(child).contains(parent)) predecessorMap.get(child).add(parent)
        // add to queue if not already visited
        if (!visited.contains(child)) FIFO.add(child)
      })
    }
    predecessorMap
  }

  /**
   * Performs a join operation and updates node. Returns true if there were no differences
   * between the child and parent node, else false.
   * - for each var in pred node block
   *    - if node contains var
   *        - if expr equal
   *            - don't change node state
   *        - else
   *            - assign top
   *    - else add var to node state
   */
  private def joinFunction(blockAbstractState: util.HashMap[FlowGraph.Block,
    util.HashMap[Var, Expr]], node: FlowGraph.Block, predecessorNode: FlowGraph.Block) = {
    var conflict: Boolean = false;

    val predState = blockAbstractState.get(predecessorNode)
    val currState = blockAbstractState.get(node)

    predState.keySet().forEach(variable => {
      if (currState.containsKey(variable)) {
        if (!currState.get(variable).equals(predState.get(variable))) {
          currState.remove(variable)
          conflict = true
        }
      } else {
        currState.put(variable, predState.get(variable))
      }
    })

    conflict
  }

  /**
   * Performs a meet operation on one or more incoming edges and updates the current block (i.e.
   * the block popped from the queue in the worklist algorithm) accordingly.
   *
   * @param blockAbstractState list of all block abstract states.
   * @param node               the current block popped in the worklist algorithm to be updated.
   * @param predecessors       list of all predecessors of the node.
   */
  private def meetFunction(blockAbstractState: util.HashMap[FlowGraph.Block, util.HashMap[Var, Expr]], node: FlowGraph.Block, predecessors: util.List[FlowGraph.Block]): Unit = {
    val newAbstractState = new util.HashMap[Var, Expr]
    // determine the variables in all blocks
    val exists = new util.LinkedList[Var]
    predecessors.forEach(block => {
      val map = blockAbstractState.get(block)
      map.keySet.forEach(variable => {
        if (!exists.contains(variable)) exists.add(variable)
      })
    })

    // perform the meet operator for constant propagation
    var allStatesAgree = true
    exists.forEach(variable => {
      var expFactToCompare: Expr = null
      val loop = new Breaks
      loop.breakable{
        predecessors.forEach(block => {
          if (blockAbstractState.get(block).containsKey(variable)) if (expFactToCompare != null) if (!(expFactToCompare == blockAbstractState.get(block).get(variable))) {
            allStatesAgree = false
            break
          }
          else expFactToCompare = blockAbstractState.get(block).get(variable)
        })
      }
      if (allStatesAgree) newAbstractState.put(variable, expFactToCompare)
    })
    // update the new state
    blockAbstractState.replace(node, newAbstractState)
  }

  /**
   * Updates the abstract state of the block.
   * - for each line in the block
   *    - if statement is an Assign
   *        - if state contains variable
   *            - new entry = var, calculateVarConstraint
   *            - add entry to state
   *        - else
   *            - add var, expr to state
   */
  private def transferFunction(block: FlowGraph.Block, state: util.HashMap[Var, Expr]): Unit = {
    block.getLines.forEach(line => {
      if (line.isInstanceOf[Assign]) {
        val assignment = line.asInstanceOf[Assign]
        val variable = assignment.getLhs.asInstanceOf[Var]
        calculateVarConstraint(state, assignment)
      }
    })
  }

  /**
   * Helper function for the transfer function. Performs any substitution or calculation on the
   * expression and updates/adds the variable to the map.
   * TODO
   */
  private def calculateVarConstraint(state: util.HashMap[Var, Expr], assignment: Assign): Unit = {
    var lhs : Var = assignment.getLhs.asInstanceOf[Var]
    var rhs : Expr = assignment.getRhs

    if (state.containsKey(lhs)) {
      state.replace(lhs, rhs)
    } else {
      state.put(lhs, rhs)
    }
  }

  /**
   * Performs constant propagation on the lines and updates the variables.
   * @param states
   * @param blocks
   */
  private def constantPropagation(states: HashMap[FlowGraph.Block, util.HashMap[Var, Expr]],
                                  blocks: List[FlowGraph.Block])
  : Unit = {
    states.keySet().forEach(block => {
      states.get(block).keySet().forEach(variable => {
        states.keySet()
      })
    })
  }
}