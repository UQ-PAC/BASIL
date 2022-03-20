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
import scala.annotation.varargs

/*
TODO:
replace map foreach w/ map match partial func
*/
class ConstantPropagationAnalysis(state: State, locals : HashMap[String, HashMap[Expr, Option[Assign]]],
  globals : HashMap[Label, Option[MemAssign]], previous : Stmt) extends AnalysisPoint {

  // the state of all the local variables of a function
  var localState : HashMap[String, HashMap[Expr, Option[Assign]]] = locals

  // the state of the program heap i.e. global variables
  var globalState : HashMap[Label, Option[MemAssign]] = globals

  def this(state: State) = this(state, new HashMap[String, HashMap[Expr, Option[Assign]]], new HashMap[Label, Option[MemAssign]], null)
  
  /**
    * Handle each type of statement.
    * 
    * @param stmt
    */
  override def transfer(stmt: Stmt) = {
    // println("transfer")
    var functionLocalState : HashMap[String, HashMap[Expr, Option[Assign]]] = new HashMap[String, HashMap[Expr, Option[Assign]]]
    localState.foreach(function => {
      functionLocalState(function._1) = function._2.clone
    })
    var programGlobalState = globals.clone

    // val prevStmtFunc = if (previousStmt != null) findStmtFunc(previousStmt) else null
    val currStmtFunc = findStmtFunc(stmt)

    if (!functionLocalState.contains(currStmtFunc.header.funcName)) functionLocalState.update(currStmtFunc.header.funcName, new HashMap[Expr, Option[Assign]]())

    // println("New Stmt")
    // println(previousStmt)
    if (stmt.isInstanceOf[Assign]) {
    // if (previousStmt != null && previousStmt.isInstanceOf[Assign]) {
      var newPrevStmt : Assign = stmt.asInstanceOf[Assign]
      functionLocalState.get(currStmtFunc.header.funcName).get.foreach(constraint => {
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
        case regAssignStmt : RegisterAssign => if (!regAssignStmt.isFramePointer && !regAssignStmt.isLinkRegister && !regAssignStmt.isStackPointer) functionLocalState.get(currStmtFunc.header.funcName).get.update(newPrevStmt.lhs, Some(regAssignStmt))
        case memAssignStmt : MemAssign if !memAssignStmt.lhs.onStack => programGlobalState.update(newPrevStmt.label, Some(memAssignStmt))
        case memAssignStmt : MemAssign if memAssignStmt.lhs.onStack => functionLocalState.get(currStmtFunc.header.funcName).get.update(newPrevStmt.lhs, Some(memAssignStmt))
        case _ =>
      }
    }

    // println(s"\n${stmt.label.pc} : ${stmt}")
    // println(localState)

    // println("transfer")
    // debugPrint()

    ConstantPropagationAnalysis(state, functionLocalState, programGlobalState, stmt).asInstanceOf[this.type]
  }

  // private def foldLocals: Expr = {}

  // private def foldGlobals: Expr = {}

  /**
    * If this has less variables than other, return 1. 
    * @param other
    * @return
    */
  override def compare(other: this.type): Int = {
    (this.countEdges - other.countEdges).sign
  }

  private def countEdges: Int = {
    var count: Int = 0

    localState.values.foreach(localState => 
      localState.values.foreach(constraint => {
        count += 1
    }))

    globalState.values.foreach(constraint => {
      count += 1
    })
    
    count;
  }

  override def createLowest: this.type = {
    ConstantPropagationAnalysis(state, new HashMap[String, HashMap[Expr, Option[Assign]]](), new HashMap[Label, Option[MemAssign]](), null).asInstanceOf[this.type]
  }

  /*
  This might not actually work....
  */
  override def equals(other: this.type): Boolean = {
    println("in CP equals")

    if (other == null) {
      println("\nh1")
      return false
    }

    localState.foreach(function => {
      val funcName = function._1
      val localMap = function._2

      if (!other.localState.contains(funcName)) { println("\nh2"); return false }

      localMap.foreach(local => {
        val variable = local._1
        val value = local._2

        if (!other.localState.get(funcName).get.contains(variable)) { println("\nh3"); return false }

        (value, other.localState.get(funcName).get.get(variable).get) match {
          case (Some(a), Some(b)) => if (a != b) { return false }
          case (None, None) =>
          case _ => { println("\nh4"); return false }
        }
      })
    })

    other.localState.foreach(function => {
      val funcName = function._1
      val localMap = function._2

      if (!localState.contains(funcName)) { println("\nh5"); return false }

      localMap.foreach(local => {
        val variable = local._1
        val value = local._2

        if (!localState.get(funcName).get.contains(variable)) { println("\nh6"); return false }
      })
    })

    println("locals equal")

    globalState.foreach(global => {
      val label = global._1
      val value = global._2

      if (!other.globalState.contains(label)) { println("\nh7"); return false }

      (value, other.globalState.get(label).get) match {
        case (Some(a), Some(b)) => if (a != b) { println("\nh8"); return false }
        case (None, None) =>
        case _ => { println("\nh9"); return false }
      }
    })

    other.globalState.foreach(global => {
      val label = global._1
      val value = global._2

      if (!globalState.contains(label)) { println("\nh10"); return false }
    })

    println("globals equal")

    return true
  }

  /**
   * Constant propagation is a must-analysis, meaning to derive the exit state of a block a meet operation must be performed on parents.
   * 
   */
  override def combine(other: this.type): this.type = {
    meet(other);
  }

  /**
   * todo: create private set operations
    * Null == Top element
    * None == Bottom element
    * @param other
    * @return
    */
  override def join(other: this.type) = {
    var functionLocalState : HashMap[String, HashMap[Expr, Option[Assign]]] = new HashMap[String, HashMap[Expr, Option[Assign]]]
    localState.foreach(function => {
      functionLocalState(function._1) = function._2.clone
    })
    var programGlobalState = globals.clone

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

      if (other.localState.contains(function)) locals.foreach(local => {
        val variable = local._1
        val constraint = local._2

        other.localState.getOrElse(function, throw new Exception("CP Analysis: join error.")).getOrElse(variable, "Any") match {
          // due to Scala type erasure, encoding the concrete parameter type will be useless and only produce pesky warnings
          case stmt : Option[_] if !stmt.isEmpty => {
            if (constraint.get != stmt.get) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).update(variable, None)
          }
          case "Any" =>
          case None => functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).update(variable, None)
          case _ => 
        }
      })
    })

    // if there exists a function state or variable in OTHER not in THIS, add to THIS
    other.localState.foreach(localState => {
      val function = localState._1    // function name
      val locals = localState._2      // map expr -> label

      if (!functionLocalState.contains(function)) functionLocalState.update(function, other.localState.getOrElse(function, null))
      else locals.foreach(local => {
        val variable = local._1
        val constraint = local._2

        if (!functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).contains(variable)) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: join error.")).update(variable, constraint)
      })
    })

    programGlobalState.foreach(globalConstraint => {
      val global = globalConstraint._1
      val constraint = globalConstraint._2

      other.globalState.getOrElse(global, "Any") match {
        case stmt : Option[_] if !stmt.isEmpty => if (constraint.get != stmt.get) programGlobalState.update(global, None)
        case "Any" =>
        case None => programGlobalState.update(global, None)
        case _ =>
      }
    })

    other.globalState.foreach(globalConstraint => {
      val global = globalConstraint._1
      val constraint = globalConstraint._2

      if (!programGlobalState.contains(global)) programGlobalState.update(global, constraint)
    })

    // println("after:")
    // debugPrint()

    ConstantPropagationAnalysis(state, functionLocalState, programGlobalState, previous).asInstanceOf[this.type]
  }

  /**
    * TODO: union of remove + pending
    * @param other
    * @return
    */
  override def meet(other: this.type) = {
    var functionLocalState : HashMap[String, HashMap[Expr, Option[Assign]]] = new HashMap[String, HashMap[Expr, Option[Assign]]]
    localState.foreach(function => {
      functionLocalState(function._1) = function._2.clone
    })
    var programGlobalState = globals.clone

    // println("\nMeet info:")
    // this.debugPrint()
    // other.debugPrint()

    // println("Local state:")
    functionLocalState.foreach(localState => {
      val function = localState._1    // function name
      val locals = localState._2      // map expr -> label

      if (!other.localState.contains(function)) functionLocalState.remove(function)
      else locals.foreach(local => {
        val variable = local._1
        val constraint = local._2

        other.localState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).getOrElse(variable, "Any") match {
          case stmt : Option[_] if !stmt.isEmpty => {
            if (constraint == None) functionLocalState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).update(variable, Some(stmt.get.asInstanceOf[Assign])) 
            else if (stmt.get != constraint.get) { functionLocalState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).remove(variable); println("Removed: " + stmt.get); }
          }
          case "Any" =>  functionLocalState.getOrElse(function, throw new Exception("CP Analysis: meet error.")).remove(variable); println("Removed: " + variable);
          case None => 
          case _ => 
        }
      })
    })

    // println("Global state:")
    programGlobalState.foreach(globalConstraint => {
      val global = globalConstraint._1
      val constraint = globalConstraint._2

      other.globalState.getOrElse(global, "Any") match {
        // constant
        case stmt : Option[_] if !stmt.isEmpty => if (constraint == None) programGlobalState.update(global, Some(stmt.get.asInstanceOf[MemAssign])) else if (stmt.get != constraint.get) programGlobalState.remove(global); println("Removed: " + stmt.get);
        // bottom element
        case "Any" => programGlobalState.remove(global); println("Removed: " + global);
        // top element
        case None => 
        case _ => 
      }
    })

    // debugPrint()

    ConstantPropagationAnalysis(state, functionLocalState, programGlobalState, previous).asInstanceOf[this.type]
  }

  def debugPrint() = {
    println(localState)
    println(globalState)
    println(previous)
  }

  override def applyChanges(preState: State, information: Map[Stmt, this.type]): State = {
    println("applying changes\n")
    // information.foreach(analysis => {
    //   val stmt = analysis._1
    //   val state = analysis._2
    
    //   println(stmt.label.pc + " : " + stmt)
    //   println(state.localState)
    // })
    var prevStmt : Stmt = null

    information.foreach(analysis => {
      val stmt = analysis._1
      val state = analysis._2
      prevStmt = stmt

      // println("\n" + stmt.label.pc + " : " + stmt)
      if (prevStmt != null) {
        println("\n" + prevStmt.label.pc + " : " + prevStmt)
        // println(state.previous)
        println(state.localState.toString + "\n")
        val func = findStmtFunc(stmt)

        prevStmt match {
          case regAssign : RegisterAssign => if (state.localState.get(func.header.funcName).get.get(regAssign.lhs) != None) func.replaceLine(prevStmt, state.localState.get(func.header.funcName).get.get(regAssign.lhs).get.get)
          case memAssign : MemAssign if !memAssign.lhs.onStack => if (state.globalState.get(prevStmt.label) != None) func.replaceLine(prevStmt, state.globalState.get(prevStmt.label).get.get)
          case stackAssign : MemAssign if stackAssign.lhs.onStack => if (state.localState.get(func.header.funcName).get.get(stackAssign.lhs) != None) func.replaceLine(prevStmt, state.localState.get(func.header.funcName).get.get(stackAssign.lhs).get.get)
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
          case _ => 
        }
      }
      
      // prevStmt = stmt
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