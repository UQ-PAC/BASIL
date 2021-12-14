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

class ConstantPropagation(flowgraph: FlowGraph) {
  
  // TODO: should be able to give boolean for equivalences

  /**
   * Performs constant propagation on all functions given
   */
  def foldVariables(): Unit = {
//    System.out.println("here in Const prop")
    flowgraph.getFunctions.forEach(function => {
      val stmtConstrainst =
        propagationWorklistAlgorithm(function.getBlocks, function.getLines)
//      System.out.println("almost")
      constantPropagation(stmtConstrainst, function.getLines, function.getBlocks)
    })
//    System.out.println("leaving Const prop")
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
    val constraints = new util.HashMap[String, ArrayBuffer[String]]()
    val predecessorMap = findBlockPredecessors(blocks.get(0))
    val worklist = new util.LinkedList[FlowGraph.Block]
    worklist.add(blocks.get(0))
    while ( {
      worklist.size > 0
    }) {
//      System.out.println("in wl")
      val next = worklist.poll

      // perform meet on all predecessor statements if next has more than one
      if (predecessorMap.containsKey(next)) {
        if (predecessorMap.get(next).size > 0) {
          val predStmts = ArrayBuffer[Stmt]()
          predecessorMap.get(next).forEach(block => {
            predStmts+=block.lastLine.asInstanceOf[Stmt]
          })
          meetFunction(constraints, next.firstLine.asInstanceOf[Stmt], predStmts)
        }
      }

//      System.out.println("in wl2")

      var i : Int = 1
      var predecessor : Stmt = next.firstLine.asInstanceOf[Stmt]
      val tempBlockLines = next.getLines.asInstanceOf[ArrayList[Stmt]]
      val blockLines = new ArrayBuffer[Stmt]()
      tempBlockLines.forEach(line => {
        blockLines+= line
      })

//      System.out.println("in wl2.5")
      while (i < next.getLines.size) {
        // join then transfer
        joinFunction(constraints, blockLines(i), predecessor, lines)
//        System.out.println("in wl2.5.1")
        transferFunction(predecessor, blockLines(i).asInstanceOf[Stmt], constraints, lines)
//        System.out.println("in wl2.5.2")
        predecessor = blockLines(i)
        i+=1
      }

//      System.out.println("in wl3")

      next.getChildren.forEach(child => {
        if (joinFunction(constraints, next.lastLine.asInstanceOf[Stmt], 
          child.firstLine.asInstanceOf[Stmt], lines))
          worklist.add(child)
      })
    }
    
//    System.out.println("finished worklist")
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
                           predecessors: ArrayBuffer[Stmt]): Unit = {
    // TODO: top & bottom lattice elements??

    val predPcs = ArrayBuffer[String]()
    predecessors.foreach(stmt => {
      val pc = stmt.getLabel.getPc
      if (!predPcs.contains(pc)) {
        predPcs+=pc
      }
    })

    val nodeConst = ArrayBuffer[String]()
    var contains : Boolean = true
    predPcs.foreach(pc => {
      predecessors.foreach(stmt => {
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
    val predNum = predecessors.getLabel.getPc
    val nodeNum = node.getLabel.getPc
    
    var conflicts : Boolean = false
    val predStmts = constraints.get(predNum)
    val childStmts = constraints.get(nodeNum)
    if (predStmts != null) {      // if map contains parent statement
      if (childStmts != null) {       // if map contains child statement
        predStmts.foreach(predDep => {
          val predDepInst = findInstFromPc(lines, predDep)
          var contains : Boolean = false
          childStmts.foreach(nodeDep => {
            val nodeDepInst = findInstFromPc(lines, nodeDep)
            if (predDepInst.getLhs.equals(nodeDepInst.getLhs) && !predDepInst.getRhs.equals
            (nodeDepInst.getRhs)) {
              // TODO: add to the list as a top lattice element??
              constraints.get(nodeNum)-=predDep
              conflicts = true
              contains = true
            } else if (nodeDep.equals(predDep)) {
              contains = true
            }
          })
          if (!contains) {
            constraints.get(nodeNum)+=predDep
          }
        })
      } else {
        constraints.put(node.getLabel.getPc, new ArrayBuffer[String]())
        predStmts.foreach(predDep => {
          constraints.get(nodeNum)+=predDep
        })
      }
    } else {
      constraints.put(predNum, new ArrayBuffer[String]())
      constraints.put(nodeNum, new ArrayBuffer[String]())
    }

    conflicts
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
    val predStmtDeps = constraints.get(predStmt.getLabel.getPc)

    // TODO: handle case where line is new Assign
    var replaceConstNum : String = null
    
    if (predStmt.isInstanceOf[Assign]) {
      val predAssign = predStmt.asInstanceOf[Assign]
      val predVar = predAssign.getLhs
      var contains : Boolean = false
      val loop = new Breaks
      loop.breakable { childStmtDeps.foreach(constPC => {
        val constStmt = findInstFromPc(lines, constPC)
        if (predStmt.getLabel.getPc.equals(constPC)) {
          contains = true
//          System.out.println("hello there")
          loop.break
        }
        if (predVar.equals(constStmt.getLhs)) {
          constraints.get(childStmt.getLabel.getPc)-=constPC
          constraints.get(childStmt.getLabel.getPc)+=predStmt.getLabel.getPc
          contains = true
//          System.out.println("gk")
          loop.break
        }
      })
      
      if (!contains) {
        constraints.get(childStmt.getLabel.getPc)+=predStmt.getLabel.getPc
      }
    }}
  }

  /**
   * Performs constant propagation on the lines and updates the variables.
   * @param states
   * @param blocks
   */
  private def constantPropagation(constraints: util.HashMap[String, ArrayBuffer[String]],
                                  lines: List[Stmt], blocks: List[FlowGraph.Block])
  : Unit = {
    // assignments that will be removed if the lhs variable is re-assigned later
    val pendingRemoval = new ArrayBuffer[Assign]()
    // list of lines that will be removed once the loop exits
    val toRemove = new ArrayBuffer[Assign]()
    // iterate over all lines in the function
//    System.out.println("in cp")
    lines.forEach(line => {
//      System.out.println("cp2")
      val lineNum = line.getLabel.getPc
      // if the constraint map contains the pc, then for each constraint pc find the instruction 
      // and replace the line with the instruction rhs
      if (constraints.containsKey(lineNum) && line.isInstanceOf[Assign]) {
        constraints.get(lineNum).foreach(constPc => {
          val constraint = findInstFromPc(lines, constPc)
//          System.out.println("cp2.1")
//          System.out.println(line.asInstanceOf[Assign].getRhs)
//          System.out.println(constraint)
//          System.out.println(constraint.getLhs)
//          System.out.println(constraint.getRhs)
          replaceAllMatchingChildren(line.asInstanceOf[Assign].getRhs, constraint.getLhs,
            constraint.getRhs)
          if (line.asInstanceOf[Assign].getRhs == constraint.getLhs) line.asInstanceOf[Assign].replace(line.asInstanceOf[Assign].getRhs,
            constraint.getRhs)
//          System.out.println("cp2.2")
        })

//        System.out.println("cp3")
        // if the line is an instance of an assignment
        if (line.isInstanceOf[Assign]) {
          // for each instruct in pending removal, if the instruct lhs var matches the current 
          // assign var, remove the old stmt from pending and place in remove
          pendingRemoval.foreach(pending => {
            if (pending.getLhs.equals(line.asInstanceOf[Assign].getLhs)) {
              pendingRemoval-=pending
              toRemove+=pending
              pendingRemoval+=line.asInstanceOf[Assign]
            }
          })
          
          // if rhs contains only literals, then compute the rhs
          line.asInstanceOf[Assign].getRhs match {
            case lit: Literal =>
              val computed = computeLiteral(lit)
              val newLiteral: Literal = new Literal(computed) // TODO are these two lines necassary
              pendingRemoval+=line.asInstanceOf[Assign]
            case _ => // skip
          }
        }
//        System.out.println("cp4")
      }
    })

//    System.out.println("cp5")
    // remove all
    toRemove.foreach(flowgraph.removeLine)

//    System.out.println("out cp")
  }

  // TODO
  private def computeLiteral(exp: Expr): String = exp.toString

  /**
   * Iterates over all lines in the function until it finds the statement/instruction with the
   * matching pc
   *
   * @param lines
   * @param pc
   * @return
   */
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