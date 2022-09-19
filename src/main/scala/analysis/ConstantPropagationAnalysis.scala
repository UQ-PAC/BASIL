//package analysis
//
//object ConstantPropagationAnalysis:
//
//  object Intraprocedural:
//
//    /** Intraprocedural analysis that uses the simple fixpoint solver.
//      */
//    class SimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
//        extends IntraprocValueAnalysisSimpleSolver(cfg, ConstantPropagationLattice)

/*
package analysis

import astnodes._
import vcgen.{FunctionState, State}

case class ConstantPropagationAnalysis(state: State,
                                       // the state of all the local variables of a function
                                       localState: Map[String, Map[Expr, MaybeNonConstantAssign]],
                                       // the state of the program heap i.e. global variables
                                       globalState: Map[String, MaybeNonConstantMemAssign],
                                       // true for folding + expression simplification, false for just heap address resolution
                                       simplification: Boolean)
  extends AnalysisPoint[ConstantPropagationAnalysis] {

  /**
 * Performs a transfer on the entry analysis point state and returns the exit state.
 *
 */
  override def transfer(stmt: Statement): ConstantPropagationAnalysis = stmt match {
      case assign: AssignStatement =>
        val funcName = state.findStatementFunction(stmt).name
        var newPrevStmt = assign
        val localStateWithFunc = if (!localState.contains(funcName)) {
          localState + (funcName -> Map())
        } else {
          localState
        }

        // fold statement with all local and global variables
        for ((variable, assignment) <- localStateWithFunc(funcName)) {
          assignment match {
            case a: AssignStatement => newPrevStmt = newPrevStmt.simplify(variable, a.rhs)
            case _ =>
          }
        }
        for ((_, memAssign) <- globalState) {
          memAssign match {
            case a: MemAssign => newPrevStmt = newPrevStmt.simplify(a.lhs, a.rhs)
            case _ =>
          }
        }

        val updateGlobalState = newPrevStmt match {
          case memAssignStmt: MemAssign if !memAssignStmt.lhs.onStack =>
            globalState + (newPrevStmt.pc -> memAssignStmt)
          case _ => globalState
        }

        val updateLocalState = newPrevStmt match {
          case regAssignStmt: LocalAssign
            if !regAssignStmt.isFramePointer && !regAssignStmt.isLinkRegister && !regAssignStmt.isStackPointer =>
            localStateWithFunc + (funcName -> (localStateWithFunc(funcName) + (newPrevStmt.lhs -> regAssignStmt)))
          case memAssignStmt: MemAssign if memAssignStmt.lhs.onStack =>
            localStateWithFunc + (funcName -> (localStateWithFunc(funcName) + (newPrevStmt.lhs -> memAssignStmt)))
          case _ => localStateWithFunc
        }
        copy (localState = updateLocalState, globalState = updateGlobalState)
      case _ => this
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
      for (_ <- l.values) {
        count += 1
      }
    }

    for (_ <- globalState.values) {
      count += 1
    }
    count
  }

  /**
 * Creates an analysis point at the bottom of the lattice.
 *
 */
  override def createLowest: ConstantPropagationAnalysis = ConstantPropagationAnalysis(state, simplification)

  /**
 * Determines whether two ConstantPropagation analysis points are equal.
 *
 */
  override def equals(other: ConstantPropagationAnalysis): Boolean = {
    // need to include state, simplification here ???
    if (other.localState == this.localState && other.globalState == this.globalState) {
      true
    } else {
      false
    }
    /*
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
 */
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
    if (state != other.state) {
      throw new Exception("trying to join unrelated analyses")
    }

    val localStateOut = ((localState.keySet union other.localState.keySet) map {
      function => (localState.get(function), other.localState.get(function)) match {
        case (Some(t), Some(o)) =>
          val mapUpdate = ((t.keySet union o.keySet) map {
            variable => (t.get(variable), o.get(variable)) match {
              case (Some(a), Some(b)) if a == b => variable -> a
              case (Some(a), None) => variable -> a
              case (None, Some(b)) => variable -> b
              case _ => variable -> NonConstantAssign() // case where a != b
            }
          }).toMap
          function -> mapUpdate
        case (None, Some(o)) => function -> o
        case (Some(t), None) => function -> t
        case _ => function -> Map() // shouldn't happen
      }
    }).toMap

    val globalStateOut = ((globalState.keySet union other.globalState.keySet) map {
      global => (globalState.get(global), other.globalState.get(global)) match {
        case (Some(t), Some(o)) if t == o => global -> t
        case (Some(t), None) => global -> t
        case (None, Some(o)) => global -> o
        case _ => global -> NonConstantMemAssign() // case where t != o
      }
    }).toMap

    ConstantPropagationAnalysis(state, localStateOut, globalStateOut, simplification)
  }

  /**
 * Performs a meet on two ConstantPropagation analysis points and returns a new ConstantPropagation analysis point.
 *
 */
  override def meet(other: ConstantPropagationAnalysis): ConstantPropagationAnalysis = {
    val localStateOut = ((localState.keySet intersect other.localState.keySet) map {
      function => function -> ((localState(function).keySet intersect other.localState(function).keySet) collect {
        case variable if localState(function)(variable) == other.localState(function)(variable) =>
          variable -> localState(function)(variable)
      }).toMap
    }).toMap

    val globalStateOut = ((globalState.keySet intersect other.globalState.keySet) collect {
      case global if globalState(global) == other.globalState(global) => global -> globalState(global)
    }).toMap

    ConstantPropagationAnalysis(state, localStateOut, globalStateOut, simplification)
  }

  def debugPrint(): Unit = {
    println(localState)
    println(globalState)
  }

  override def applyChange(stmt: Statement): Statement = {
    // this is still bad but should replace it with propagating through later
    val func = state.findStatementFunction(stmt)
    if (simplification) {
      stmt match {
        case regAssign: LocalAssign =>
          localState(func.name).get(regAssign.lhs) match {
            case Some(s: AssignStatement) => s
            case _ => stmt
          }
        case memAssign: MemAssign =>
          if (memAssign.lhs.onStack) {
            localState(func.name).get(memAssign.lhs) match {
              case Some(s: AssignStatement) => s
              case _ => stmt
            }
          } else {
            globalState.get(stmt.pc) match {
              case Some(s: MemAssign) => s
              case _ => stmt
            }
          }
        case _ => stmt
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
        case memAssign: MemAssign if !memAssign.lhs.onStack =>
          globalState.get(stmt.pc) match {
            case Some(s: MemAssign) => s
            case _ => stmt
          }
        case _ => stmt
      }
    }
  }
}

object ConstantPropagationAnalysis {
  def apply(state: State, simplify: Boolean): ConstantPropagationAnalysis =
    ConstantPropagationAnalysis(state, Map(), Map(), simplify)
}
 */
