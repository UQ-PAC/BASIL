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
  var functionLocalState : HashMap[String, HashMap[Expr, Option[RegisterAssign]]] = new HashMap[String, HashMap[Expr, Option[RegisterAssign]]]()

  // the state of the program heap i.e. global variables
  var programGlobalState : HashMap[Label, Option[MemAssign]] = new HashMap[Label, Option[MemAssign]]()

  var previousStmt : Stmt = null
  
  /**
    * Handle each type of statement.
    * 
    * @param stmt
    */
  override def transfer(stmt: Stmt) = {
    val prevStmtFunc = if (previousStmt != null) findStmtFunc(previousStmt) else null
    val currStmtFunc = findStmtFunc(stmt)

    if (!functionLocalState.contains(currStmtFunc.header.funcName)) functionLocalState.update(currStmtFunc.header.funcName, new HashMap[Expr, Option[RegisterAssign]]())

    // println("New Stmt")
    // println(previousStmt)
    if (previousStmt != null && previousStmt.isInstanceOf[Assign]) {
      var newPrevStmt : Assign = previousStmt.asInstanceOf[Assign]
      functionLocalState.get(prevStmtFunc.header.funcName).get.foreach(constraint => {
        val variable = constraint._1
        val assignment = constraint._2

        if (assignment != None) newPrevStmt = newPrevStmt.fold(variable, assignment.get.rhs)
      })

      programGlobalState.foreach(constraint => {
        val label = constraint._1
        val memAssign = constraint._2

        if (memAssign != None) newPrevStmt = newPrevStmt.fold(memAssign.get.lhs.asInstanceOf[Expr], memAssign.get.rhs.asInstanceOf[Expr])
      })

      newPrevStmt match {
        case regAssignStmt : RegisterAssign => if (!regAssignStmt.isFramePointer && !regAssignStmt.isLinkRegister && !regAssignStmt.isStackPointer) functionLocalState.get(prevStmtFunc.header.funcName).get.update(newPrevStmt.lhs, Some(regAssignStmt))
        case memAssignStmt : MemAssign => programGlobalState.update(newPrevStmt.label, Some(memAssignStmt))
        case _ =>
      }
    }

    previousStmt = stmt

    // println("transfer")
    // debugPrint()

    this
  }

  // private def foldLocals: Expr = {}

  // private def foldGlobals: Expr = {}

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
    functionLocalState = new HashMap[String, HashMap[Expr, Option[RegisterAssign]]]()
    programGlobalState = new HashMap[Label, Option[MemAssign]]()
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
          case stmt : Option[RegisterAssign] if !stmt.isEmpty => {
            if (constraint.get != stmt.get) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).update(variable, None)
          }
          case "Any" =>
          case None => functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).update(variable, None)
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
        case stmt : Option[RegisterAssign] if !stmt.isEmpty => if (constraint.get != stmt.get) programGlobalState.update(global, None)
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
          case stmt : Option[RegisterAssign] if !stmt.isEmpty => {
            if (constraint == None) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).update(variable, stmt) 
            else if (stmt.get != constraint.get) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).remove(variable)
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
        case stmt : Option[MemAssign] if !stmt.isEmpty => if (constraint == None) programGlobalState.update(global, stmt) else if (stmt.get != constraint.get) programGlobalState.remove(global)
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
    // information.foreach(analysis => {
    //   val stmt = analysis._1
    //   val state = analysis._2
    //
    //   println(stmt.label.pc + " : " + stmt)
    //   println(state.functionLocalState)
    // })

    information.foreach(analysis => {
      val stmt = analysis._1
      val state = analysis._2

      val func = findStmtFunc(stmt)

      stmt match {
        case regAssign : RegisterAssign => if (state.functionLocalState.get(func.header.funcName).get.get(regAssign.lhs) != None) func.replaceLine(stmt, state.functionLocalState.get(func.header.funcName).get.get(regAssign.lhs).get.get)
        case memAssign : MemAssign => if (state.programGlobalState.get(stmt.label) != None) func.replaceLine(stmt, state.programGlobalState.get(stmt.label).get.get)
        // case cJump: CJmpStmt => func.replaceLine(stmt, cJump.fold())
        case _ => 
      }
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