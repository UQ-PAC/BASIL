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
import vcgen.State
import astnodes.exp.FunctionCall
import astnodes.Label
import java.{util => ju}

class ConstantPropagationAnalysis(state: State) extends AnalysisPoint {

  // the state of all the local variables of a function
  var functionLocalState : HashMap[String, HashMap[Expr, Option[Label]]] = new HashMap[String, HashMap[Expr, Option[Label]]]()

  // the state of the program heap i.e. global variables
  var programGlobalState : HashMap[MemLoad, Option[Label]] = new HashMap[MemLoad, Option[Label]]()

  var previousStmt : Stmt = null
  
  /**
    * Handle each type of statement.
    * 
    * @param stmt
    */
  override def transfer(stmt: Stmt) = {
    val prevStmtFunc = if (previousStmt != null) findStmtFunc(previousStmt) else null
    val currStmtFunc = findStmtFunc(stmt)

    if (!functionLocalState.contains(currStmtFunc.header.funcName)) functionLocalState.update(currStmtFunc.header.funcName, new HashMap[Expr, Option[Label]]())

    previousStmt match {
      case regAssignStmt : RegisterAssign => {
        if (!regAssignStmt.isFramePointer && !regAssignStmt.isLinkRegister && !regAssignStmt.isStackPointer) {
          prevStmtFunc.header.funcName.equals(currStmtFunc.header.funcName) match {
            case true => functionLocalState.getOrElseUpdate(currStmtFunc.header.funcName, new HashMap[Expr, Option[Label]]()).update(regAssignStmt.lhs, Some(previousStmt.label))
            case false => if (!functionLocalState.contains(currStmtFunc.header.funcName)) functionLocalState.update(prevStmtFunc.header.funcName, new HashMap[Expr, Option[Label]]())
          }
          // println(prevStmtFunc.header.funcName)
          // println(functionLocalState.get(prevStmtFunc.header.funcName))
        }
      }
      case memAssignStmt : MemAssign => {
        if (memAssignStmt.lhs.onStack) functionLocalState.getOrElseUpdate(prevStmtFunc.header.funcName, new HashMap[Expr, Option[Label]]()).update(memAssignStmt.lhs, Some(previousStmt.label))
        else programGlobalState.update(memAssignStmt.lhs, Some(previousStmt.label))
      }
      case null =>
      case _ => if (!functionLocalState.contains(currStmtFunc.header.funcName)) functionLocalState+=(prevStmtFunc.header.funcName, new HashMap[Expr, Option[Label]]())
    }

    previousStmt = stmt

    // println("transfer")
    // debugPrint()

    this
  }

  /**
    * If this has less variables than other, return 1. 
    * @param other
    * @return
    */
  override def compare(other: this.type): Int = {
    val otherAsThis : ConstantPropagationAnalysis = typeCheck(other)
    (this.countEdges - otherAsThis.countEdges).sign
  }

  private def countEdges: Int = {
    var count: Int = 0

    functionLocalState.values.foreach(localState => 
      localState.values.foreach(constraint => {
        count += 1
    }))

    programGlobalState.values.foreach(constraint => {
      count += 1
    })
    
    count;
  }

  override def createLowest: this.type = {
    functionLocalState = new HashMap[String, HashMap[Expr, Option[Label]]]()
    programGlobalState = new HashMap[MemLoad, Option[Label]]()
    previousStmt = null
    this
  }

  /*
  This might not actually work....
  */
  override def equals(other: this.type): Boolean = {
    var otherAsThis: ConstantPropagationAnalysis = typeCheck(other);
    
    if (!functionLocalState.equals(otherAsThis.functionLocalState) || !programGlobalState.equals(otherAsThis.programGlobalState)) {
      return false
    }
    
    return true
  }

  /**
   * todo: create private set operations
    * Null == Top element
    * None == Bottom element
    * @param other
    * @return
    */
  override def join(other: this.type) = {
    val otherAsThis : ConstantPropagationAnalysis = typeCheck(other)

    // println("join")
    // println("before:")
    // println("this:")
    // debugPrint()
    // println("other:")
    // otherAsThis.debugPrint()

    // iterate over all functions for THIS, and if OTHER contains function, set all variables that contain different constraints in
    // THIS to null
    functionLocalState.foreach(localState => {
      val function = localState._1    // function name
      val locals = localState._2      // map expr -> label

      if (otherAsThis.functionLocalState.contains(function)) locals.foreach(local => {
        val variable = local._1
        val constraint = local._2

        otherAsThis.functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).getOrElse(variable, "Any") match {
          case label : Option[Label] if !label.isEmpty => {
            if (constraint != label) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).update(variable, null)
          }
          case "Any" =>
          case None => functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).update(variable, null)
          case _ => 
        }
      })
    })

    // if there exists a function state or variable in OTHER not in THIS, add to THIS
    otherAsThis.functionLocalState.foreach(localState => {
      val function = localState._1    // function name
      val locals = localState._2      // map expr -> label

      if (!functionLocalState.contains(function)) functionLocalState.update(function, otherAsThis.functionLocalState.getOrElse(function, null))
      else locals.foreach(local => {
        val variable = local._1
        val constraint = local._2

        if (!functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).contains(variable)) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).update(variable, constraint)
      })
    })

    programGlobalState.foreach(globalConstraint => {
      val global = globalConstraint._1
      val constraint = globalConstraint._2

      otherAsThis.programGlobalState.getOrElse(global, "Any") match {
        case label : Option[Label] if !label.isEmpty => if (constraint.get != label.get) programGlobalState.update(global, None)
        case "Any" =>
        case None => programGlobalState.update(global, None)
        case _ =>
      }
    })

    otherAsThis.programGlobalState.foreach(globalConstraint => {
      val global = globalConstraint._1
      val constraint = globalConstraint._2

      if (!programGlobalState.contains(global)) programGlobalState.update(global, constraint)
    })

    // println("after:")
    // debugPrint()

    this
  }

  /**
    * TODO: union of remove + pending
    * @param other
    * @return
    */
  override def meet(other: this.type) = {
    val otherAsThis : ConstantPropagationAnalysis = typeCheck(other)

    functionLocalState.foreach(localState => {
      val function = localState._1    // function name
      val locals = localState._2      // map expr -> label

      if (!otherAsThis.functionLocalState.contains(function)) functionLocalState.remove(function)
      else locals.foreach(local => {
        val variable = local._1
        val constraint = local._2

        otherAsThis.functionLocalState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).getOrElse(variable, "Any") match {
          case label : Option[Label] if !label.isEmpty => {
            if (constraint == null) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).update(variable, label) 
            else if (label != constraint) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).remove(variable)
          }
          case "Any" =>  functionLocalState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).remove(variable)
          case None => 
          case _ =>
        }
      })
    })

    programGlobalState.foreach(globalConstraint => {
      val global = globalConstraint._1
      val constraint = globalConstraint._2

      otherAsThis.programGlobalState.getOrElse(global, "Any") match {
        // constant
        case label : Option[Label] if !label.isEmpty => if (constraint == null) programGlobalState.update(global, label) else if (label != constraint) programGlobalState.remove(global)
        // bottom element
        case "Any" => programGlobalState.remove(global)
        // top element
        case None => 
        case _ => 
      }
    })

    println("meet")
    debugPrint()

    this
  }

  def debugPrint() = {
    println(functionLocalState)
    println(programGlobalState)
    println(previousStmt)
  }

  override def applyChanges(preState: State, information: Map[Stmt, this.type]): State = {
    // println(functionLocalState)
    // println(programGlobalState)
    // println(previousStmt)

    // information.foreach(analysis => analysis._2.debugPrint())

    information.foreach(analysis => {
      val stmt = analysis._1
      val state = analysis._2

      println("New statement")
      println(stmt)
      // state.debugPrint()
      val func = findStmtFunc(stmt)

      var simpleStmt = stmt
      println("Folds:")
      state.functionLocalState
        .getOrElse(func.header.funcName, throw new Exception(s"CP Analysis: Function unknown: ${func.header.funcName}"))
        .foreach(constraint => {
          val dependentExp = constraint._1
          val dependentInst = constraint._2

          if (dependentInst != None) {
            val newExpr = func.findStmtFromLabel(dependentInst.get).getOrElse(throw new Exception("CP Analysis: Cannot find statement."))
            println(newExpr)

            // if (assignStmt.lhs.isInstanceOf[Register] && assignStmt.lhs.asInstanceOf[Register].name.equals("#31")) {
            //   println(newStmt)
            // }

            simpleStmt match {
              case assignStmt : Assign => simpleStmt = simpleStmt.asInstanceOf[Assign].fold(dependentExp, newExpr.asInstanceOf[Assign].getRhs)
              case cJump : CJmpStmt => simpleStmt = simpleStmt.asInstanceOf[CJmpStmt].fold(dependentExp, newExpr.asInstanceOf[Assign].getRhs)
              case _ =>
            }

            // if (assignStmt.lhs.isInstanceOf[Register] && assignStmt.lhs.asInstanceOf[Register].name.equals("#31")) {
            //   println(newStmt)
            // }
          }
        })
      
      state.programGlobalState
        .foreach(constraint => {
          val dependentExp = constraint._1
          val dependentInst = constraint._2

          if (dependentInst != None) {
            val newExpr = func.findStmtFromLabel(dependentInst.get).getOrElse(throw new Exception("CP Analysis: Cannot find statement."))
            println(newExpr)

            // if (assignStmt.lhs.isInstanceOf[Register] && assignStmt.lhs.asInstanceOf[Register].name.equals("#31")) {
            //   println(newStmt)
            // }

            simpleStmt match {
              case assignStmt : Assign => simpleStmt = simpleStmt.asInstanceOf[Assign].fold(dependentExp, newExpr.asInstanceOf[Assign].getRhs)
              case cJump : CJmpStmt => simpleStmt = simpleStmt.asInstanceOf[CJmpStmt].fold(dependentExp, newExpr.asInstanceOf[Assign].getRhs)
              case _ =>
            }

            // if (assignStmt.lhs.isInstanceOf[Register] && assignStmt.lhs.asInstanceOf[Register].name.equals("#31")) {
            //   println(newStmt)
            // }
          }
        })

      func.replaceLine(stmt, simpleStmt)
    })

    preState
  }

  // find the function that contains the statement (yes its disgusting i know)
  private def findStmtFunc(stmt: Stmt) = state.functions.find(func => 
        {!func.labelToBlock.values.find(block => 
          !block.lines.find(line => 
            line.label == stmt.label).isEmpty).isEmpty})
            .getOrElse(throw new Exception("CP Analysis: Statement does not belong to a function."))

}