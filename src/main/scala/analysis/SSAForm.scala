package analysis

import analysis.*
import analysis.solvers.{Term, UnionFindSolver}
import ir.{SignExtend, *}

import scala.collection.mutable

class SSAForm(cfg: ProgramCfg, ANRResult: Map[CfgNode, Set[Variable]], RNAResult: Map[CfgNode, Set[Variable]]) {

  val varMaxTracker = new mutable.HashMap[String, Int]()
  val procBasedAssignments = new mutable.HashMap[(CfgFunctionEntryNode, String), Int]()
  val unionFindSolver: UnionFindSolver[StTerm] = new UnionFindSolver()

  def varToStTerm(vari: VariableWrapper): Term[StTerm] = IdentifierVariable(vari)

  def termToVar(term: Term[StTerm]): VariableWrapper = term match {
    case IdentifierVariable(vari) => vari
    case _ => throw new RuntimeException("Not a variable")
  }

  def getMax(varName: String): Int =
    val ret = varMaxTracker.getOrElse(varName, 1) // starts from 1 because 0 is reserved for unassigned yet
    varMaxTracker(varName) = ret + 1
    ret

  // TODO: NOTE: if we unify(t1, t2) then t2 will be the new representative, as such we use this to unify
  //  procedure call variables with the procedure parameters (which are the representatives)

  def SSAStyle(n: CfgNode, funcEntry: CfgFunctionEntryNode, params: Option[Set[Variable]] = None): Unit = {
    n match {
      case cmd: CfgCommandNode => {
        cmd.data match {
          case stmt: Statement => {
            println(stmt)
            stmt match {
              case localAssign: LocalAssign => {
                localAssign.rhs.variables.foreach(v => {
                  transformVariable(v, funcEntry, params)
                })
                val maxVal = getMax(localAssign.lhs.name)
                procBasedAssignments((funcEntry, localAssign.lhs.name)) = maxVal
                localAssign.lhs.ssa_id = maxVal
                unionFindSolver.find(varToStTerm(RegisterVariableWrapper(localAssign.lhs)))
              }
              case memoryAssign: MemoryAssign => {
                memoryAssign.lhs.variables.foreach(v => {
                  transformVariable(v, funcEntry, params)
                })
                memoryAssign.rhs.variables.foreach(v => {
                  transformVariable(v, funcEntry, params)
                })
              }
              case assume: Assume =>
                assume.body.variables.foreach(v => {
                  transformVariable(v, funcEntry, params)
                }) // not required for analyses
              case assert: Assert => {
                assert.body.variables.foreach(v => {
                  transformVariable(v, funcEntry, params)
                })
              } // not required for analyses
              case _ => throw new RuntimeException("No SSA form for " + stmt.getClass + " yet")
            }
          }
          case jump: Jump => {
            jump match {
              case indirectCall: IndirectCall => {
                indirectCall.target.variables.foreach(v => {
                  transformVariable(v, funcEntry, params)
                })
              }
              case directCall: DirectCall => {
                // TODO: FIXME
                // Here it should detect the variables used as arguments and unify them with the callee procedure parameters
                // Must use ANR and RNA to do this

                // Step 1: Overlap the ANR and RNA results to get the variables that are used as arguments
                val ANRVars = ANRResult(n)
                val RNAVars = RNAResult(n)
                val args = ANRVars.intersect(RNAVars).map({ v => termToVar(unionFindSolver.find(varToStTerm(RegisterVariableWrapper(v)))).variable})
                val callee = directCall.target

                // Step 2: Unify the arguments with the callee procedure parameters
                applySSA(cfg.funEntries.collectFirst({case s if s.data == callee => s}).get, Some(args))
              }
              case _ => {}
            }
          }
        }
      }
      case _ => {}
    }
  }

  def transformVariable(v: Variable, proc: CfgFunctionEntryNode, params: Option[Set[Variable]]): Unit = {
    if (params.isDefined) {
      val collected = params.collect({ case s if s == v => s })
      if (collected.nonEmpty) {
        unionFindSolver.unify(varToStTerm(RegisterVariableWrapper(collected.get.head)), varToStTerm(RegisterVariableWrapper(v)))
        v.ssa_id = termToVar(unionFindSolver.find(varToStTerm(RegisterVariableWrapper(v)))).variable.ssa_id
      }
    }
    if (v.ssa_id == 0) {
      v.ssa_id = procBasedAssignments.getOrElseUpdate((proc, v.name), getMax(v.name))
      unionFindSolver.find(varToStTerm(RegisterVariableWrapper(v)))
    } else {
      val current = v
      val incoming = v match {
        case r: Register => r.copy()
        case l: LocalVar => l.copy()
      }
      incoming.ssa_id = procBasedAssignments.getOrElseUpdate((proc, v.name), getMax(v.name)) // this should be the new var
      unionFindSolver.unify(varToStTerm(RegisterVariableWrapper(current)), varToStTerm(RegisterVariableWrapper(incoming)))
      v.ssa_id = termToVar(unionFindSolver.find(varToStTerm(RegisterVariableWrapper(v)))).variable.ssa_id
    }
  }

  def recursiveIterator(node: CfgNode, funcEntry: CfgFunctionEntryNode, params: Option[Set[Variable]] = None): Unit = {
    SSAStyle(node, funcEntry, params)
    for (succ <- node.succIntra) {
      recursiveIterator(succ, funcEntry, params)
    }
  }

  def applySSA(funcEntry: CfgFunctionEntryNode = cfg.startNode, params: Option[Set[Variable]] = None): Unit = {
      for (succ <- funcEntry.succIntra) {
        recursiveIterator(succ, funcEntry, params)
      }
    println(
      """
        |---------------------
        |SSA Form
        |---------------------
        |""".stripMargin)
    println(unionFindSolver.solution())
  }
}