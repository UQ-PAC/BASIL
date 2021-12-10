package translating

import astnodes.exp.{Expr, Literal, Var}
import astnodes.stmt.Stmt
import astnodes.stmt.assign.Assign
import translating.FlowGraph
import vcgen.Block

import java.lang.invoke.MethodHandles.loop
import scala.util.control.Breaks._
import java.util
import java.util.{ArrayList, HashMap, LinkedList, List}
import javax.swing.SpringLayout.Constraints
import scala.util.control.Breaks
import scala.collection.mutable.ArrayBuffer

class ConstantPropagation[functions: util.List[FlowGraph.Function]] {

  /**
   * Performs constant propagation on all functions given
   */
  def foldVariables(): Unit = {
    functions.forEach(function => {
      val blockStates =
        propagationWorklistAlgorithm(function.getBlocks, function.getLines)
      constantPropagation(blockStates, function.getLines)
    })
  }

  /**
   * Uses the propagation worklist algorithm to calculate variable constraints on each block.
   * - while worklist not empty
   *    - next := worklist.poll
   *        - if next has >1 predecessor
   *            - perform meet on last lines & update first of next
   *        - for each line in next
   *            - line.join
   *            - line.transfer
   *
   * @param blocks
   * @return
   */
  private def propagationWorklistAlgorithm(blocks: util.List[FlowGraph.Block], lines: List[Stmt]):
  util.HashMap[String, ArrayBuffer[String]] = {
    var constraints : util.HashMap[String, ArrayBuffer[String]] = new util.HashMap[String,
      ArrayBuffer[String]]()
    val predecessorMap = findBlockPredecessors(blocks.get(0))
    val worklist = new util.LinkedList[FlowGraph.Block]
    worklist.add(blocks.get(0))
    while ( {
      worklist.size > 0
    }) {
      val next = worklist.poll

      // perform meet on all predecessor statements if next has more than one
      if (predecessorMap.get(next).size > 0) {
        val predStmts = ArrayBuffer[Stmt]()
        predecessorMap.get(next).forEach(block => {
          predStmts+=block.lastLine.asInstanceOf[Stmt]
        })
        meetFunction(constraints, next.firstLine.asInstanceOf[Stmt], predStmts)
      }

      var i : Int = 0
      var predecessor : Stmt = next.firstLine.asInstanceOf[Stmt]
      while (i < next.getLines.size) {
        // join then transfer
        joinFunction(constraints, next.getLines(i).asInstanceOf[Stmt], predecessor, lines)
        transferFunction(predecessor, next.getLines(i).asInstanceOf[Stmt], constraints, lines)
        i+=1
      }

      next.getChildren.foreach(child => {
        if (!joinFunction(constraints, next.lastLine.asInstanceOf[Stmt], predecessorMap.get(next)
          .get(0), lines))
          worklist.add(child)
      })
    }
    return constraints
  }

  /**
   * Performs a meet operation on one or more incoming edges and updates the current block (i.e.
   * the block popped from the queue in the worklist algorithm) accordingly.
   *
   * @param blockAbstractState list of all block abstract states.
   * @param node               the current block popped in the worklist algorithm to be updated.
   * @param predecessors       list of all predecessors of the node.
   */
  private def meetFunction(constraints : util.HashMap[String, ArrayBuffer[String]], node: Stmt,
                           predecessors: List[Stmt]): Unit = {
    // TODO: top & bottom lattice elements??

    val predPcs = ArrayBuffer[String]()
    predecessors.forEach(stmt => {
      val pc = stmt.getLabel.getPc
      if (!predPcs.contains(pc)) {
        predPcs+=pc
      }
    })

    val nodeConst = ArrayBuffer[String]()
    var contains : Boolean = true
    predPcs.foreach(pc => {
      predecessors.forEach(stmt => {
        if (!constraints.get(stmt.getLabel.getPc).contains(pc)) {
          contains = false
          break()
        }
      })

      if (contains) {
        nodeConst+=pc
      }
    })

    constraints.replace(node.getLabel.getPc, nodeConst)
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
  private def joinFunction(constraints: util.HashMap[String, ArrayBuffer[String]], node: Stmt,
                           predecessors: Stmt, lines: List[Stmt]): Boolean = {
    var nodeList : util.LinkedList[String] = new util.LinkedList[String]()
    val predNum = predecessors.getLabel.getPc
    val nodeNum = node.getLabel.getPc

    // for all dependencies, must find the var they point to, and compare the vars they have in
    // common
    // for all pcs in predecessor
        // for all pcs in node
            // get stmt from pred
            // get stmt from node
            // if pred var == node var
                // add to node list

    constraints.get(predNum).foreach(predDep => {
      val predDepInst = findInstFromPc(lines, predDep)
      constraints.get(nodeNum).foreach(nodeDep => {
        val nodeDepInst = findInstFromPc(lines, nodeDep)
        if (predDepInst.getLhs.equals(nodeDepInst.getLhs) && predDepInst.getRhs.equals
        (nodeDepInst.getRhs)) {
          nodeList+=nodeNum
        } else if (predDepInst.getLhs.equals(nodeDepInst.getLhs) && !predDepInst.getRhs.equals
        (nodeDepInst.getRhs)) {
          // TODO: add to the list as a top lattice element??
        }
      })
    })

    true
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
  private def transferFunction(predStmt: Stmt, childStmt: Stmt, constraints: util.HashMap[String,
    ArrayBuffer[String]], lines: List[Stmt])
  : Unit = {
    val childStmtDeps = constraints.get(childStmt.getLabel.getPc)

    // eval parent stmt
    // if rhs
    var replaceConstNum : String = null
    if (predStmt.isInstanceOf[Assign]) {
      val predAssign = predStmt.asInstanceOf[Assign]
      val predVar = predAssign.getLhs
      childStmtDeps.foreach(constPC => {
        val constStmt = findInstFromPc(lines, constPC)
        if (predVar.equals(constStmt.getLhs)) {
          constraints.get(childStmt.getLabel.getPc).remove(constPC)
          constraints.get(childStmt.getLabel.getPc)+=predStmt.getLabel.getPc
          break()
        } else {
          constraints.get(childStmt.getLabel.getPc)+=predStmt.getLabel.getPc
          break()
        }
      })
    }
  }

  /**
   * Performs constant propagation on the lines and updates the variables.
   * @param states
   * @param blocks
   */
  private def constantPropagation(constraints: util.HashMap[String, ArrayBuffer[String]],
                                  lines: List[Stmt])
  : Unit = {
    // assignments that will be removed if the lhs variable is re-assigned later
    var pendingRemoval : util.LinkedList[String] = new util.LinkedList[String]()
    // list of lines that will be removed once the loop exits
    val toRemove : util.LinkedList[String] = new util.LinkedList[String]()
    lines.forEach(line => {
      val lineNum = line.getLabel.getPc
      if (constraints.containsKey(lineNum)) {
        constraints.get(lineNum).forEach(constPc => {
          val constraint = findInstFromPc(lines, constPc)
          replaceAllMatchingChildren(line.asInstanceOf[Assign].getRhs, constraint.getLhs,
            constraint.getRhs)
        })

        line.asInstanceOf[Assign].getRhs match {
          case lit: Literal =>
            val computed = computeLiteral(lit)
            val newLiteral: Literal = new Literal(computed) // TODO are these two lines necassary
            pendingRemoval+=lineNum
          case _ => // skip
        }
      }
    })
  }

  // TODO
  private def computeLiteral(exp: Expr): String = exp.toString

  private def findInstFromPc(lines: List[Stmt], pc: String): Assign = {
    var inst : Stmt = null
    lines.forEach(line => {
      if (line.getLabel.getPc.equals(pc)) {
        inst = line
      }
    })
    inst.asInstanceOf[Assign]
  }

  private def replaceAllMatchingChildren(parent: Expr, oldExp: Expr, newExp: Expr): Unit = {
    parent.getChildren.forEach((child: Expr) => replaceAllMatchingChildren(child, oldExp, newExp))
    parent.replace(oldExp, newExp)
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
}