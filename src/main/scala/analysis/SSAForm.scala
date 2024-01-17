package analysis

import analysis.*
import analysis.solvers.{Term, UnionFindSolver}
import ir.{SignExtend, *}

import scala.collection.mutable

object SSAForm {

  val varMaxTracker = new mutable.HashMap[String, Int]()
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

  def applySSA(program: Program, ANRResult: Map[CfgNode, Set[Variable]], RNAResult: Map[CfgNode, Set[Variable]]): Unit = {
    val blockBasedMappings = new mutable.HashMap[(Block, String), Int]()

    def transformVariable(v: Variable, block: Block): Unit = {
      if (v.ssa_id == 0) {
        v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), getMax(v.name))
        unionFindSolver.find(varToStTerm(RegisterVariableWrapper(v)))
      } else {
        val current = v
        val incoming = v match {
          case r: Register => r.copy()
          case l: LocalVar => l.copy()
        }
        incoming.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), getMax(v.name)) // this should be the new var
        unionFindSolver.unify(varToStTerm(RegisterVariableWrapper(current)), varToStTerm(RegisterVariableWrapper(incoming)))
        v.ssa_id = termToVar(unionFindSolver.find(varToStTerm(RegisterVariableWrapper(v)))).variable.ssa_id
      }
    }

    for (proc <- program.procedures) {
      for (block <- proc.blocks) {
        for (stmt <- block.statements) {
          println(stmt)
          stmt match {
            case localAssign: LocalAssign => {
                localAssign.rhs.variables.foreach(v => {
                    transformVariable(v, block)
                })
                val maxVal = getMax(localAssign.lhs.name)
                blockBasedMappings((block, localAssign.lhs.name)) = maxVal
                localAssign.lhs.ssa_id = maxVal
                unionFindSolver.find(varToStTerm(RegisterVariableWrapper(localAssign.lhs)))
            }
            case memoryAssign: MemoryAssign => {
                memoryAssign.lhs.variables.foreach(v => {
                  transformVariable(v, block)
                })
                memoryAssign.rhs.variables.foreach(v => {
                  transformVariable(v, block)
                })
            }
            case assume: Assume =>
              assume.body.variables.foreach(v => {
                  transformVariable(v, block)
                }) // not required for analyses
            case assert: Assert => {
                assert.body.variables.foreach(v => {
                  transformVariable(v, block)
                })
            } // not required for analyses
            case _ => throw new RuntimeException("No SSA form for " + stmt.getClass + " yet")
          }
        }
        block.jump match {
          case indirectCall: IndirectCall => {
              indirectCall.target.variables.foreach(v => {
                transformVariable(v, block)
              })
          }
          case directCall: DirectCall => {
              // TODO: FIXME
              // Here it should detect the variables used as arguments and unify them with the callee procedure parameters
              // Must use ANR and RNA to do this
              
              // Step 1: Overlap the ANR and RNA results to get the variables that are used as arguments
          }
          case _ => {}
        }
      }
    }
  }
}