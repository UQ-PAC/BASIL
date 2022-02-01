package analysis

import java.util.List

import scala.collection.mutable.HashMap
import scala.util.control.Breaks
import astnodes.exp.{Expr, Literal, MemStore, BinOp, UniOp, Extract}
import astnodes.exp.`var`.{MemLoad, Var}
import astnodes.stmt.*
import astnodes.stmt.assign.*
import translating.FlowGraph
import translating.FlowGraph.Block
import astnodes.exp.Concat
import astnodes.exp.`var`.Register

// TODO: does not have to take this map?
class ConstantPropagationAnalysis(constraints: HashMap[Expr, String], toRemove: Set[String],
                                  previous : astnodes.stmt.Stmt, flowgraph : FlowGraph) extends AnalysisPoint {

  val state : HashMap[Expr, String] = constraints
  val stmtsToRemove : Set[String] = toRemove
  val previousStmt : Stmt = previous

  def resolveVar(stmt: Stmt): Unit = {
    if (stmtsToRemove.contains(stmt.getLabel.getPc)) {
      flowgraph.removeLine(stmt)

    } else {
      var newStmt = stmt

      if (state.size > 0) { 
        state.foreach(entry => {
          val dependentExp = entry._1
          val dependentInst = entry._2

          if (dependentInst != null) {
            val newExpr = findInstFromPc(flowgraph.getLines, dependentInst)

            // if (assignStmt.lhs.isInstanceOf[Register] && assignStmt.lhs.asInstanceOf[Register].name.equals("#31")) {
            //   println(newStmt)
            // }

            newStmt match {
              case assignStmt : Assign => newStmt = newStmt.asInstanceOf[Assign].fold(dependentExp, newExpr.getRhs)
              case cJump : CJmpStmt => newStmt = newStmt.asInstanceOf[CJmpStmt].fold(dependentExp, newExpr.getRhs)
              case _ =>
            }
            
            // if (assignStmt.lhs.isInstanceOf[Register] && assignStmt.lhs.asInstanceOf[Register].name.equals("#31")) {
            //   println(newStmt)
            // }
          }
        })
      }

      flowgraph.replaceLine(newStmt, stmt)
    }
  }

  private def findInstFromPc(lines: List[Stmt], pc: String): Assign = {
    var inst : Stmt = null
    lines.forEach(line => {
      if (line.getLabel.getPc.equals(pc)) {
        inst = line
      }
    })
    inst.asInstanceOf[Assign]
  }
  
  /**
    * Handle each type of statement.
    * 
    * @param stmt
    */
  override def transfer(stmt: Stmt): AnalysisPoint = {
    val newState : HashMap[Expr, String] = state.clone()
    
    previousStmt match {
      case regAssignStmt : RegisterAssign => {
        if (!regAssignStmt.isFramePointer && !regAssignStmt.isLinkRegister && !regAssignStmt.isStackPointer) {
          val oldStmPc = newState.getOrElse(regAssignStmt.getLhs, null)
          if (oldStmPc != null) {
            val oldStmt = findInstFromPc(flowgraph.getLines, oldStmPc)
            if (oldStmt.getRhs.equals(regAssignStmt.getRhs)) {
              toRemove+oldStmPc
            }
          }
          newState.update(regAssignStmt.getLhs.asInstanceOf[Var], regAssignStmt.getLabel.getPc)
        }
      }
      case memAssignStmt : MemAssign => {
        val oldStmPc = newState.getOrElse(memAssignStmt.getLhs, null)
        if (oldStmPc != null) {
          val oldStmt = findInstFromPc(flowgraph.getLines, oldStmPc)
          if (oldStmt.getRhs.equals(memAssignStmt.getRhs)) {
            toRemove+oldStmPc
          }
        }
        newState.update(memAssignStmt.getLhs.asInstanceOf[Var], memAssignStmt.getLabel.getPc)
      }
      case _ => {
      }
    }
    
    new ConstantPropagationAnalysis(newState, toRemove, stmt, flowgraph)
  }

  /**
    * If this has less variables than other, return 1. 
    * @param other
    * @return
    */
  override def compare(other: AnalysisPoint): Int = {
    val otherAsThis : ConstantPropagationAnalysis = typeCheck(other)
    (this.countEdges - otherAsThis.countEdges).sign
  }

  private def countEdges: Int = {
    var count: Int = 0;
    state.values.foreach(expr => {
      if (expr != null) count += 1;
    });
    return count;
  }

  override def createLowest: AnalysisPoint = {
    new ConstantPropagationAnalysis(new HashMap[Expr, String], Set(), null, flowgraph)
  }

  override def equals(other: AnalysisPoint): Boolean = {
    var otherAsThis: ConstantPropagationAnalysis = typeCheck(other);
    
    if (!state.equals(otherAsThis.state) || !stmtsToRemove.equals(otherAsThis
      .stmtsToRemove)) {
      return false
    }
    
    return true
  }

  /**
    * TODO: intersect of remove & pending
    * Add variable to constraints of new analysis point if it is contained in both this analysis 
    * point and the other. If expressions different, do not add.
    * @param other
    * @return
    */
  override def intersection(other: AnalysisPoint): AnalysisPoint = {
    val otherAsThis : ConstantPropagationAnalysis = typeCheck(other)
    
    val newState = new HashMap[Expr, String]()
    val newRemove : Set[String] = Set()
    val newPending : Set[String] = Set()
    
    state.foreach(entry => {
      if (otherAsThis.state.contains(entry._1)) {
        if (entry._2.equals(otherAsThis.state.get(entry._1))) {
          newState.put(entry._1, entry._2)
        } else if (entry._2 == null && otherAsThis.state.get(entry._1) == null) {
          newState.put(entry._1, null)
        }
      }
    })
    
    new ConstantPropagationAnalysis(newState, newRemove, null, flowgraph)
  }

  /**
    * TODO: union of remove + pending
    * @param other
    * @return
    */
  override def union(other: AnalysisPoint): AnalysisPoint = {
    val otherAsThis : ConstantPropagationAnalysis = typeCheck(other)

    val newState = new HashMap[Expr, String]()
    val newRemove : Set[String] = Set()
    val newPending : Set[String] = Set()
    
    val breakableLoop = new Breaks

    var contains : Boolean = false
    state.foreach(entry => {
      if (otherAsThis.state.contains(entry._1)) {
        contains = true

        if (entry._2 == null) {
          newState.put(entry._1, otherAsThis.state.getOrElse(entry._1, null))
        } else if (otherAsThis.state.get(entry._1) == null) {
          newState.put(entry._1, entry._2)
        } else if (entry._2.equals(otherAsThis.state.get(entry._1))) {
          newState.put(entry._1, entry._2)
        } else if (!entry._2.equals(otherAsThis.state.get(entry._1))) {
          newState.put(entry._1, null)
        }
      }
      
      if (!contains) {
        newState.put(entry._1, entry._2)
      }
    })

    otherAsThis.state.foreach(entry => {
      if (!newState.contains(entry._1) && !state.contains(entry._1)) {
        newState.put(entry._1, entry._2)
      }
    })

    new ConstantPropagationAnalysis(newState, newRemove, null, flowgraph)
  }
}