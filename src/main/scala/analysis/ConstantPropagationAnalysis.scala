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
      if (state.size > 0) { 
        var newStmt = stmt

        state.foreach(entry => {
          val dependentExp = entry._1
          val dependentInst = entry._2

          newStmt match {
            case assignStmt : Assign => {
              if (dependentInst != null) {
                // if (assignStmt.getLhs.isInstanceOf[MemLoad] && assignStmt.getLhs.asInstanceOf[MemLoad].onStack) {
                //   val newExpr = findInstFromPc(flowgraph.getLines, entry._2)

                //   newStmt.asInstanceOf[Assign].getLhs.asInstanceOf[MemLoad].replace(entry._1, newExpr.getRhs)
                // }
                val newExpr = findInstFromPc(flowgraph.getLines, dependentInst)
                newStmt = newStmt.asInstanceOf[Assign].replace(dependentExp, newExpr.getRhs)

                if (assignStmt.getRhs.isInstanceOf[BinOp]) {
                  val newStmtRhs = newStmt.asInstanceOf[Assign].getRhs.asInstanceOf[BinOp].simplify()
                  newStmt = newStmt.asInstanceOf[Assign].replace(newStmt.asInstanceOf[Assign].getRhs, newStmtRhs)

                } else if (assignStmt.getRhs.isInstanceOf[Extract]) {
                  val newStmtRhs = SimplificationUtil.bitvecExtract(newStmt.asInstanceOf[Assign].getRhs.asInstanceOf[Extract])
                  newStmt = newStmt.asInstanceOf[Assign].replace(newStmt.asInstanceOf[Assign].getRhs, newStmtRhs)

                } else if (assignStmt.getRhs.isInstanceOf[Concat]) {
                  println(assignStmt.getRhs)
                  val newStmtRhs = SimplificationUtil.bitvecConcat(newStmt.asInstanceOf[Assign].getRhs.asInstanceOf[Concat])
                  println(newStmtRhs)
                  newStmt = newStmt.asInstanceOf[Assign].replace(newStmt.asInstanceOf[Assign].getRhs, newStmtRhs)
                }
              }
            }

            case condJump : CJmpStmt => {
              if (dependentInst != null) newStmt = newStmt.asInstanceOf[CJmpStmt].subst(dependentExp, findInstFromPc(flowgraph.getLines, dependentInst).getRhs)
            }

            case _ => 
          }
        })

        flowgraph.replaceLine(newStmt, stmt)
      }
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
    // System.out.println("in transfer")
    val newState : HashMap[Expr, String] = state.clone()
    
    previousStmt match {
      // case memAssignStmt : MemAssign => {
      //   // System.out.println("in mem assign")
      //   newState.update(memAssignStmt.getLhs.asInstanceOf[MemLoad], memAssignStmt.getLabel.getPc)
      case regAssignStmt : RegisterAssign => {
        // System.out.println("in reg assign")
        if (!regAssignStmt.isFramePointer && !regAssignStmt.isLinkRegister && !regAssignStmt.isStackPointer) {
          // println("can fold")
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
        // System.out.println("elsewhere")
      }
    }

    // System.out.println("out transfer")
    
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
//    System.out.println("in countEdges")
    var count: Int = 0;
    state.values.foreach(expr => {
      if (expr != null) count += 1;
    });
    return count;
  }

  override def createLowest: AnalysisPoint = {
//    System.out.println("in createLowest")
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
    
    new ConstantPropagationAnalysis(newState, newRemove, null, flowgraph)
  }

  /**
    * TODO: union of remove + pending
    * @param other
    * @return
    */
  override def union(other: AnalysisPoint): AnalysisPoint = {
    // System.out.println("in union")
    val otherAsThis : ConstantPropagationAnalysis = typeCheck(other)

    val newState = new HashMap[Expr, String]()
    val newRemove : Set[String] = Set()
    val newPending : Set[String] = Set()
    
    val breakableLoop = new Breaks

    var contains : Boolean = false
    state.foreach(entry => {
      // println(entry)
      if (otherAsThis.state.contains(entry._1)) {
        // println(otherAsThis.state.get(entry._1))
        // println("in")
        contains = true

        if (entry._2 == null) {
          newState.put(entry._1, otherAsThis.state.getOrElse(entry._1, null))
        } else if (otherAsThis.state.get(entry._1) == null) {
          newState.put(entry._1, entry._2)
        } else if (entry._2.equals(otherAsThis.state.get(entry._1))) {
          // println("in 2")
          newState.put(entry._1, entry._2)
        } else if (!entry._2.equals(otherAsThis.state.get(entry._1))) {
          // println("in 3")
          newState.put(entry._1, null)
        }
      }

      // println("left")
      
      if (!contains) {
        newState.put(entry._1, entry._2)
      }
    })

    otherAsThis.state.foreach(entry => {
      // println("for each other constraint")
      if (!newState.contains(entry._1) && !state.contains(entry._1)) {
        newState.put(entry._1, entry._2)
      }
    })

    // println("out union")
    new ConstantPropagationAnalysis(newState, newRemove, null, flowgraph)
  }
}


case object SimplificationUtil {
  /*
  1) apply bitvec concatonations
  */

  // def simplify(expr: Expr): Expr = {
  //   expr match {
  //     case binOp : BinOp => binArithmetic(binOp)
  //     case uniOp : UniOp => uniArithmetic(uniOp)
  //     case concat : Concat => bitvecConcat(concat)
  //     case extract : Extract => bitvecExtract(extract)
  //     case _ => expr
  //   }
  // }

  def bitvecConcat(concat: Concat): Expr = {
    /*
    1) get lhs & rhs
    2) convert to bin
    3) concatonate bitvecs
    4) convert to decimal
    */

    val lhs = concat.getLhs
    val rhs = concat.getRhs
    
    if (lhs.isInstanceOf[Literal] && rhs.isInstanceOf[Literal]) {
      // if pad
      if (lhs.toString.equals("0")) return Literal(rhs.toString, Some(64))

      // if extend
      if (rhs.toString.equals("0")) {
        val lhsExtended = Integer.parseInt(lhs.asInstanceOf[Literal].toString) << 32
        return Literal(lhsExtended.toString, Some(64))
      }
    }

    if (lhs.isInstanceOf[Literal] && rhs.isInstanceOf[MemLoad]) {
      return MemLoad(rhs.asInstanceOf[MemLoad].exp, Some(64))
    }

    concat
  }

  def bitvecExtract(extract: Extract): Expr = {
    if (extract.getStart == 0 && extract.getEnd == 31) {
      val value = Integer.parseInt(extract.getExp.asInstanceOf[Literal].toString) & 0xFFFFFFFF
      Literal(value.toString, Some(32))
    }

    if (extract.getStart == 32 && extract.getEnd == 63) {
      Literal((Integer.parseInt(extract.getExp.asInstanceOf[Literal].toString) >>> 32).toString, Some(32))
    }

    extract
  }

  // def binArithmetic(binOp: BinOp): Expr = {}

  // def uniArithmetic(uniOp: UniOp): Expr = {}
}