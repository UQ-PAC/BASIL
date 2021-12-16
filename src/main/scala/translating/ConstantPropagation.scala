package translating

import astnodes.exp.{BinOp, Expr, Literal, Var}
import astnodes.stmt.Stmt
import astnodes.stmt.assign.*
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
    flowgraph.getFunctions.forEach(function => {
      System.out.println("Before CP:")
      debugPrinter(function)
//      function.getBlocks.forEach(block => {
//        System.out.println(block)
//      })
      val stmtConstrainst =
        propagationWorklistAlgorithm(function.getBlocks, function.getLines)
//      System.out.println("here5")
      constantPropagation(stmtConstrainst, function, function.getBlocks)
      System.out.println("After CP:")
      debugPrinter(function)
//      function.getBlocks.forEach(block => {
//        System.out.println(block)
//      })
    })
  }
  
  private def debugPrinter(function: FlowGraph.Function): Unit = {
    function.getLines.forEach(line => {
      System.out.println(line.getLabel.getPc + " " + line)
    })
  }

  private def debugFindMemAssigns(function: FlowGraph.Function): Unit = {
    function.getLines.forEach(line => {
      if (line.isInstanceOf[MemAssign]) System.out.println(line)
    })
  }

  private def debugFindRegAssigns(function: FlowGraph.Function): Unit = {
    function.getLines.forEach(line => {
      if (line.isInstanceOf[RegisterAssign]) System.out.println(line)
    })
  }

  private def debugMapPrint(map : util.HashMap[String, ArrayBuffer[String]]): Unit = {
    map.keySet().forEach(pc => {
      System.out.println("Line:")
      System.out.println(pc)
      System.out.println("Dependent lines:")
      map.get(pc).foreach(const => {
        System.out.println(const)
      })
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
    val constraints = new util.HashMap[String, ArrayBuffer[String]]()
    val predecessorMap = findBlockPredecessors(blocks.get(0))
    val worklist = new util.LinkedList[FlowGraph.Block]
    worklist.add(blocks.get(0))
    while ( {
      worklist.size > 0
    }) {
//      System.out.println("here1")
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

//      System.out.println("here2")
      var i : Int = 1
      var predecessor : Stmt = next.firstLine.asInstanceOf[Stmt]
      val tempBlockLines = next.getLines.asInstanceOf[ArrayList[Stmt]]
      val blockLines = new ArrayBuffer[Stmt]()
      tempBlockLines.forEach(line => {
        blockLines+= line
      })

//      System.out.println("here3")
      while (i < next.getLines.size) {
        // join then transfer
        joinFunction(constraints, blockLines(i), predecessor, lines)
        transferFunction(predecessor, blockLines(i).asInstanceOf[Stmt], constraints, lines)
        predecessor = blockLines(i)
        i+=1
      }

//      System.out.println("here4")
      next.getChildren.forEach(child => {
        if (joinFunction(constraints, next.lastLine.asInstanceOf[Stmt], 
          child.firstLine.asInstanceOf[Stmt], lines))
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
          loop.break
        }
        if (predVar.equals(constStmt.getLhs)) {
          constraints.get(childStmt.getLabel.getPc)-=constPC
          constraints.get(childStmt.getLabel.getPc)+=predStmt.getLabel.getPc
          contains = true
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
                                  function: FlowGraph.Function, blocks: List[FlowGraph.Block])
  : Unit = {
    // assignments that will be removed if the lhs variable is re-assigned later
    val pendingRemoval = new ArrayBuffer[Assign]()
    // list of lines that will be removed once the loop exits
    val toRemove = new ArrayBuffer[Assign]()
    // iterate over all lines in the function
    // TODO: remove from pending if accessed??
    function.getLines.forEach(line => {
//      System.out.println("here6")
      val lineNum = line.getLabel.getPc
      // if the constraint map contains the pc, then for each constraint pc find the instruction 
      // and replace the line with the instruction rhs
      if (constraints.containsKey(lineNum) && line.isInstanceOf[Assign]) {
        constraints.get(lineNum).foreach(constPc => {
          // if line is different after replacement, remove constraint from pending
          val constraint = findInstFromPc(function.getLines, constPc)
//          if (line.asInstanceOf[Assign].getRhs.contains(constraint.getLhs) && (line
//            .asInstanceOf[Assign].getLhs != constraint.getLhs)) {
//            System.out.println("found")
//            if (pendingRemoval.contains(constraint)) {
//              pendingRemoval-=constraint
//            }
//          }
          replaceAllMatchingChildren(line.asInstanceOf[Assign].getLhs, constraint.getLhs,
            constraint.getRhs)
          replaceAllMatchingChildren(line.asInstanceOf[Assign].getRhs, constraint.getLhs,
            constraint.getRhs)
          if (line.asInstanceOf[Assign].getRhs == constraint.getLhs) line.asInstanceOf[Assign].replace(line.asInstanceOf[Assign].getRhs,
            constraint.getRhs)
        })
//        System.out.println("here7")

        if (line.isInstanceOf[Assign] && line.asInstanceOf[Assign].getRhs.isInstanceOf[BinOp]) {
          //        System.out.println("here8.2")
          if (line.asInstanceOf[Assign].getRhs.asInstanceOf[BinOp].canCompute()) {
            //          System.out.println("here8.3")
            val newRhs = line.asInstanceOf[Assign].getRhs.asInstanceOf[BinOp].compute()
            //          System.out.println("here8.3.1")
            val newRhsAsStr = newRhs.toString()
            val newRhsExp = new Literal(newRhsAsStr)
            line.asInstanceOf[Assign].replace(line.asInstanceOf[Assign].getRhs, newRhsExp)
            //          System.out.println("here8.4")
          }
        }

        // TODO: where the revision is needed
        var contains : Boolean = false
        val loop = new Breaks
        loop.breakable(pendingRemoval.foreach(pending => {
          if (pending.getLhs.equals(line.asInstanceOf[Assign].getLhs) && pending.getRhs.equals
          (line.asInstanceOf[Assign].getRhs)) {
            toRemove += line.asInstanceOf[Assign]
            contains = true
            loop.break()
          } else if (pending.getLhs.equals(line.asInstanceOf[Assign].getLhs) && !pending.getRhs
            .equals(line.asInstanceOf[Assign].getRhs)) {
            pendingRemoval-=pending
            pendingRemoval+=line.asInstanceOf[Assign]
            contains=true
            loop.break()
          }
        }))
        if (!contains) {
          pendingRemoval+=line.asInstanceOf[Assign]
        }
      }
    })
//    System.out.println("here8")
    
    // remove all
    toRemove.foreach(flowgraph.removeLine)
    
//    function.getLines.forEach(line => {
////      System.out.println("here8.1")
////      System.out.println(line.getLabel.getPc + " " + line)
//      if (line.isInstanceOf[Assign] && line.asInstanceOf[Assign].getRhs.isInstanceOf[BinOp]) {
////        System.out.println("here8.2")
//        if (line.asInstanceOf[Assign].getRhs.asInstanceOf[BinOp].canCompute()) {
////          System.out.println("here8.3")
//          val newRhs = line.asInstanceOf[Assign].getRhs.asInstanceOf[BinOp].compute()
////          System.out.println("here8.3.1")
//          val newRhsAsStr = newRhs.toString()
//          val newRhsExp = new Literal(newRhsAsStr)
//          line.asInstanceOf[Assign].replace(line.asInstanceOf[Assign].getRhs, newRhsExp)
////          System.out.println("here8.4")
//        }
//      }
//    })

//    System.out.println("here9")
  }

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

object TopElement {}

object BottomElement {}