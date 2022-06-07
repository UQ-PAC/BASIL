package analysis

import java.util.List
import scala.collection.mutable.HashMap
import scala.util.control.Breaks
import astnodes.exp.{BinOp, Expr, Extract, Literal, MemStore, UniOp}
import astnodes.exp.`var`.{MemLoad, Var}
import astnodes.stmt.*
import astnodes.stmt.assign.*
import translating.FlowGraph
import translating.FlowGraph.Block
import astnodes.exp.Concat
import astnodes.exp.`var`.Register
import vcgen.State
import astnodes.exp.FunctionCall
import astnodes.Label

import java.util as ju
import scala.annotation.varargs
import scala.collection.mutable

class ConstantPropagationAnalysis(state: State,
                                  locals: mutable.HashMap[String, mutable.HashMap[Expr, Assign]],
                                  globals: mutable.HashMap[Label, MemAssign],
                                  simplification: Boolean)
  extends AnalysisPoint[ConstantPropagationAnalysis] {

  // the state of all the local variables of a function
  val localState: mutable.HashMap[String, mutable.HashMap[Expr, Assign]] = locals

  // the state of the program heap i.e. global variables
  val globalState: mutable.HashMap[Label, MemAssign] = globals

  // true for folding + expression simplification, false for just heap address resolution
  val simplificationOn: Boolean = simplification

  def this(state: State, simplify: Boolean) =
    this(state, new mutable.HashMap[String, mutable.HashMap[Expr, Assign]],
      new mutable.HashMap[Label, MemAssign], simplify)

  /**
    * Performs a transfer on the entry analysis point state and returns the exit state.
    *
    */
  override def transfer(stmt: Stmt): ConstantPropagationAnalysis = {
    val functionLocalState: mutable.HashMap[String, mutable.HashMap[Expr, Assign]] =
      new mutable.HashMap[String, mutable.HashMap[Expr, Assign]]
    localState.foreach(function => {
      functionLocalState(function._1) = function._2.clone
    })
    val programGlobalState = globals.clone

    val currStmtFunc = findStmtFunc(stmt)

    if (!functionLocalState.contains(currStmtFunc.header.funcName)) {
      functionLocalState.update(currStmtFunc.header.funcName, new mutable.HashMap[Expr, Assign])
    }

    stmt match {
      case assign: Assign =>
        var newPrevStmt: Assign = assign
        for ((variable, assignment) <- functionLocalState(currStmtFunc.header.funcName)) {
          newPrevStmt = newPrevStmt.fold(variable, assignment.rhs)
        }
        for ((label, memAssign) <- programGlobalState) {
          newPrevStmt = newPrevStmt.fold(memAssign.lhs, memAssign.rhs)
        }
        newPrevStmt match {
          case regAssignStmt: RegisterAssign =>
            if (!regAssignStmt.isFramePointer && !regAssignStmt.isLinkRegister && !regAssignStmt.isStackPointer) {
              functionLocalState(currStmtFunc.header.funcName).update(newPrevStmt.lhs, regAssignStmt)
            }
          case memAssignStmt: MemAssign =>
            if (memAssignStmt.lhs.onStack) {
              functionLocalState(currStmtFunc.header.funcName).update(newPrevStmt.lhs, memAssignStmt)
            } else {
              programGlobalState.update(newPrevStmt.label, memAssignStmt)
            }
          case _ =>
        }
      case _ =>
    }
    ConstantPropagationAnalysis(state, functionLocalState, programGlobalState, simplificationOn)
  }

  /**
   * Returns 1 if the analysis point is higher up in the lattice than other, -1 if it is lower and 0 if they are at the
   * same level.
   */
  override def compare(other: ConstantPropagationAnalysis): Int = (this.countEdges - other.countEdges).sign

  /**
   * Returns the number of edges that the analysis point contains.
   *
   */
  private def countEdges: Int = {
    var count: Int = 0

    for (l <- localState.values) {
      for (constraint <- l.values) {
        count += 1
      }
    }

    for (constraint <- globalState.values) {
      count += 1
    }
    count
  }

  /**
   * Creates an analysis point at the bottom of the lattice.
   *
   */
  override def createLowest: ConstantPropagationAnalysis = {
    ConstantPropagationAnalysis(state, new mutable.HashMap[String, mutable.HashMap[Expr, Assign]],
      new mutable.HashMap[Label, MemAssign], simplificationOn)
  }

  /**
   * Determines whether two ConstantPropagation analysis points are equal.
   *
   */
  override def equals(other: ConstantPropagationAnalysis): Boolean = {
    for ((funcName, localMap) <- localState) {
      if (!other.localState.contains(funcName)) {
        return false
      }
      for ((variable, value) <- localMap) {
        other.localState(funcName).get(variable) match {
          case Some(v) if v != value => return false
          case None => return false
          case _ =>
        }
      }
    }

    for ((funcName, localMap) <- other.localState) {
      if (!localState.contains(funcName)) {
        return false
      }
      for ((variable, value) <- localMap) {
        if (!localState(funcName).contains(variable)) {
          return false
        }
      }
    }

    for ((label, value) <- globalState) {
      if (!other.globalState.contains(label)) {
        return false
      }
      if (value != other.globalState(label)) {
        return false
      }
    }

    for ((label, value) <- globalState) {
      if (!globalState.contains(label)) {
        return false
      }
    }

    true
  }

  /**
   * Constant propagation is a must-analysis, meaning that to derive the entry state of a block a meet operation must
   * be performed on the block parents.
   *
   */
  override def combine(other: ConstantPropagationAnalysis): ConstantPropagationAnalysis = meet(other)

  /**
    * Performs a join on two ConstantPropagation analysis points and returns a new ConstantPropagation analysis point.
    *
    */
  override def join(other: ConstantPropagationAnalysis): ConstantPropagationAnalysis = {
    val functionLocalState: mutable.HashMap[String, mutable.HashMap[Expr, Assign]] =
      new mutable.HashMap[String, mutable.HashMap[Expr, Assign]]

    localState.foreach(function => functionLocalState(function._1) = function._2.clone)
    val programGlobalState = globals.clone

    // if there exists a function state or variable in OTHER not in THIS, add to THIS
    for ((function, locals) <- other.localState) {
      if (functionLocalState.contains(function)) {
        for ((variable, constraint) <- locals) {
          if (!functionLocalState(function).contains(variable)) {
            functionLocalState(function).update(variable, constraint)
          }
        }
      } else {
        functionLocalState.update(function, other.localState(function))
      }
    }

    // iterate over all functions for THIS, and if OTHER contains function, remove all variables that contain
    // different constraints in THIS
    for ((function, locals) <- functionLocalState) {
      if (other.localState.contains(function)) {
        for ((variable, constraint) <- locals) {
          other.localState(function).get(variable) match {
            case Some(s) if constraint != s => functionLocalState(function).remove(variable)
            case None => functionLocalState(function).remove(variable)
            case _ =>
          }
        }
      }
    }

    for ((global, constraint) <- other.globalState) {
      if (!programGlobalState.contains(global)) {
        programGlobalState.update(global, constraint)
      }
    }

    for ((global, constraint) <- programGlobalState) {
      other.globalState.get(global) match {
        case Some(s) if constraint != s => programGlobalState.remove(global)
        case None => programGlobalState.remove(global)
        case _ =>
      }
    }

    ConstantPropagationAnalysis(state, functionLocalState, programGlobalState, simplificationOn)
  }

  /**
    * Performs a meet on two ConstantPropagation analysis points and returns a new ConstantPropagation analysis point.
    *
    */
  override def meet(other: ConstantPropagationAnalysis): ConstantPropagationAnalysis = {
    val functionLocalState : mutable.HashMap[String, mutable.HashMap[Expr, Assign]] =
      new mutable.HashMap[String, mutable.HashMap[Expr, Assign]]
    localState.foreach(function => functionLocalState(function._1) = function._2.clone)
    val programGlobalState = globals.clone

    // does this do what I want???
    for (function <- other.localState.keys) {
      if (functionLocalState.contains(function)) {
        for ((variable, constraint) <- other.localState(function)) {
          if (!functionLocalState(function).contains(variable)) {
            functionLocalState(function).update(variable, constraint)
          }
        }
      }
    }

    for ((function, locals) <- functionLocalState) {
      if (other.localState.contains(function)) {
        for ((variable, constraint) <- locals) {
          other.localState(function).get(variable) match {
            case Some(s) if s != constraint => functionLocalState(function).remove(variable)
            case None => functionLocalState(function).remove(variable)
            case _ =>
          }
        }
      } else {
        functionLocalState.remove(function)
      }
    }

    for ((global, constraint) <- other.globalState) {
      if (!programGlobalState.contains(global)) {
        programGlobalState.update(global, constraint)
      }
    }

    for ((global, constraint) <- programGlobalState) {
      other.globalState.get(global) match {
        // constant
        case Some(stmt) if stmt != constraint => programGlobalState.remove(global)
        // top element
        case None => programGlobalState.remove(global)
        case _ =>
      }
    }

    ConstantPropagationAnalysis(state, functionLocalState, programGlobalState, simplificationOn)
  }

  def debugPrint(): Unit = {
    println(localState)
    println(globalState)
  }

  /**
   * Substitutes each statement in the State with the folded and simplified statement from the analysis.
   *
   */
  override def applyChanges(preState: State, information: Map[Stmt, ConstantPropagationAnalysis]): State = {
    for ((stmt, state) <- information) {
      val func = findStmtFunc(stmt)

      if (simplificationOn) {
        stmt match {
          case regAssign : RegisterAssign =>
            state.localState(func.header.funcName).get(regAssign.lhs) match {
              case Some(s) => func.replaceLine(stmt, s)
              case None =>
            }
          case memAssign : MemAssign =>
            if (memAssign.lhs.onStack) {
              state.localState(func.header.funcName).get(memAssign.lhs) match {
                case Some(s) => func.replaceLine(stmt, s)
                case None =>
              }
            } else {
              state.globalState.get(stmt.label) match {
                case Some(s) => func.replaceLine(stmt, s)
                case None =>
              }
            }
          case _ =>
          /* TODO: this does not really work */
          //   case cJump: CJmpStmt => {
          //     val condition = cJump.condition
          //     condition match {
          //       case reg : Register => {
          //         val constraint = state.localState.getOrElse(func.header.funcName, throw new Exception("CP: applyChanges error.")).get(reg).get
          //         val newCJump = cJump.fold(reg, constraint.get.rhs)
          //         if (!constraint.isEmpty) func.replaceLine(cJump, constraint.get)
          //       }
          //       case heap : MemLoad if !heap.onStack =>
          //       //   val constraint = state.globalState.getOrElse(func.header.funcName, throw new Exception("CP: applyChanges error.")).get(reg).get
          //       //   if (!constraint.isEmpty) func.replaceLine(reg, constraint.get.rhs)
          //       // }
          //       case stack : MemLoad if stack.onStack => {
          //         val constraint = state.localState.getOrElse(func.header.funcName, throw new Exception("CP: applyChanges error.")).get(stack).get
          //         val newCJump = cJump.fold(stack, constraint.get.rhs)
          //         if (!constraint.isEmpty) func.replaceLine(cJump, constraint.get)
          //       }
          //       case _ =>
          //     }
          //   }
        }
      } else {
        // In a perfect world we could exclusively resolve and fold through heap addresses,
        // but unfortunately the current data structure/internal representation makes this quite convoluted,
        // so it is currently not certain this functionality is worth implementing
        stmt match {
          case memAssign : MemAssign if !memAssign.lhs.onStack =>
            state.globalState.get(stmt.label) match {
              case Some(s) => func.replaceLine(stmt, s)
              case None =>
            }
          case _ =>
        }
      }
    }

    // why is this here, it's just returning an untouched parameter??
    preState
  }

  /**
   * Returns the function containing stmt.
   * 
   */
  private def findStmtFunc(stmt: Stmt) = state.functions.find(func => 
        {func.labelToBlock.values.exists(block =>
          block.lines.exists(line =>
            line.label == stmt.label))})
            .getOrElse(throw new Exception("CP Analysis: Statement does not belong to a function."))

}