package analysis

import astnodes.exp.*
import astnodes.stmt.*
import astnodes.stmt.assign.*

//import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.util.control.Breaks

// TODO: does not have to take this map?
class ConstantPropagationAnalysis(constraints: HashMap[Var, String], toRemove: Set[String],
                                  pendingRemoval: Set[String]) extends AnalysisPoint {

  val state : HashMap[Var, String] = constraints
  val stmtsToRemove : Set[String] = toRemove
  val stmtsPendingRemoval : Set[String] = pendingRemoval
  
  /**
    * Handle each type of statement.
    * 
    * @param stmt
    */
  override def transfer(stmt: Stmt): AnalysisPoint = {
    
    val newState : HashMap[Var, String] = state.clone()
    
    stmt match {
      case assignStmt : Assign => {
        newState.update(new Var(assignStmt.getLhs.asInstanceOf[Var].toString), assignStmt.getLabel
          .getPc)
      } case _ => {}
    }
    
    new ConstantPropagationAnalysis(newState, toRemove, pendingRemoval)
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
    new ConstantPropagationAnalysis(new HashMap[Var, String], Set(), Set())
  }

  override def equals(other: AnalysisPoint): Boolean = {
    var otherAsThis: ConstantPropagationAnalysis = typeCheck(other);
    
    if (!state.equals(otherAsThis.state) || !stmtsToRemove.equals(otherAsThis
      .stmtsToRemove) || !stmtsPendingRemoval.equals(otherAsThis.stmtsPendingRemoval)) {
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
    
    val newState = new HashMap[Var, String]()
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
    
    new ConstantPropagationAnalysis(newState, newRemove, newPending)
  }

  /**
    * TODO: union of remove + pending
    * @param other
    * @return
    */
  override def union(other: AnalysisPoint): AnalysisPoint = {
    val otherAsThis : ConstantPropagationAnalysis = typeCheck(other)

    val newState = new HashMap[Var, String]()
    val newRemove : Set[String] = Set()
    val newPending : Set[String] = Set()
    
    val breakableLoop = new Breaks

    var contains : Boolean = false
    state.foreach(entry => {
      if (otherAsThis.state.contains(entry._1)) {
        contains = true
        
        if (entry._2 == null && otherAsThis.state.get(entry._1) == null) {
          newState.put(entry._1, null)
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

    new ConstantPropagationAnalysis(newState, newRemove, newPending)
  }
}
