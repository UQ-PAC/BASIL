package analysis

import astnodes.exp.*
import astnodes.stmt.*
import astnodes.stmt.assign.*

//import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.util.control.Breaks

// TODO: does not have to take this map?
class ConstantPropagationAnalysis(constraints: HashMap[Expr, String], toRemove: Set[String],
                                  pendingRemoval: Set[String]) extends AnalysisPoint {

  val state : HashMap[Expr, String] = constraints
  val stmtsToRemove : Set[String] = toRemove
  val stmtsPendingRemoval : Set[String] = pendingRemoval
  
  /**
    * Handle each type of statement.
    * 
    * @param stmt
    */
  override def transfer(stmt: Stmt): AnalysisPoint = {
    System.out.println("in transfer")
    val newState : HashMap[Expr, String] = state.clone()
    
    stmt match {
      case memAssignStmt : MemAssign => {
        System.out.println("in mem assign")
        newState.update(memAssignStmt.getLhs.asInstanceOf[MemLoad], memAssignStmt.getLabel.getPc)
      } case regAssignStmt : RegisterAssign => {
        System.out.println("in reg assign")
        newState.update(regAssignStmt.getLhs.asInstanceOf[Var], regAssignStmt.getLabel.getPc)
      }
      case _ => {
        System.out.println("elsewhere")
      }
    }

    System.out.println("out transfer")
    
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
//    System.out.println("in countEdges")
    var count: Int = 0;
    state.values.foreach(expr => {
      if (expr != null) count += 1;
    });
    return count;
  }

  override def createLowest: AnalysisPoint = {
//    System.out.println("in createLowest")
    new ConstantPropagationAnalysis(new HashMap[Expr, String], Set(), Set())
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
//    System.out.println("in intersection")
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
    
    new ConstantPropagationAnalysis(newState, newRemove, newPending)
  }

  /**
    * TODO: union of remove + pending
    * @param other
    * @return
    */
  override def union(other: AnalysisPoint): AnalysisPoint = {
    System.out.println("in union")
    val otherAsThis : ConstantPropagationAnalysis = typeCheck(other)

    val newState = new HashMap[Expr, String]()
    val newRemove : Set[String] = Set()
    val newPending : Set[String] = Set()
    
    val breakableLoop = new Breaks

    var contains : Boolean = false
    state.foreach(entry => {
      println(entry)
      if (otherAsThis.state.contains(entry._1)) {
        println(otherAsThis.state.get(entry._1))
        println("in")
        contains = true

        if (entry._2 == null) {
          newState.put(entry._1, otherAsThis.state.getOrElse(entry._1, null))
        } else if (otherAsThis.state.get(entry._1) == null) {
          newState.put(entry._1, entry._2)
        } else if (entry._2.equals(otherAsThis.state.get(entry._1))) {
          println("in 2")
          newState.put(entry._1, entry._2)
        } else if (!entry._2.equals(otherAsThis.state.get(entry._1))) {
          println("in 3")
          newState.put(entry._1, null)
        }
      }

      println("left")
      
      if (!contains) {
        newState.put(entry._1, entry._2)
      }
    })

    otherAsThis.state.foreach(entry => {
      println("for each other constraint")
      if (!newState.contains(entry._1) && !state.contains(entry._1)) {
        newState.put(entry._1, entry._2)
      }
    })

    println("out union")
    new ConstantPropagationAnalysis(newState, newRemove, newPending)
  }
}
