package analysis

import analysis.*
import ir.{SignExtend, *}

import scala.collection.mutable

/** Set-Based SSA
 * - Each variable has a set of versions
 * - New assignments create new versions and replaces any new versions
 */
object SSAForm {

  val varMaxTracker = new mutable.HashMap[String, Int]()
  val blockBasedMappings = new mutable.HashMap[(Block, String), mutable.Set[Int]]().withDefault(_ => mutable.Set())
  val context = new mutable.HashMap[(Procedure, String), mutable.Set[Int]]().withDefault(_ => mutable.Set())
  def getMax(varName: String): Int =
    val ret = varMaxTracker.getOrElse(varName, 0)
    varMaxTracker(varName) = ret + 1
    ret

  def transformVariables(vars: Set[Variable], block: Block, proc: Procedure): Unit = {
    vars.foreach(v => {
      v.ssa_id = blockBasedMappings.getOrElseUpdate(
        (block, v.name),
        context.getOrElseUpdate((proc, v.name), mutable.Set(getMax(v.name)))
      )
    })
  }

  def applySSA(program: Program): Unit = {
    for (proc <- program.procedures) {
      for (block <- proc.blocks) {
        for (stmt <- block.statements) {
          println(stmt)
          stmt match {
            case localAssign: LocalAssign => {
              transformVariables(localAssign.rhs.variables, block, proc)
              val maxVal = varMaxTracker.getOrElseUpdate(localAssign.lhs.name, 0)
              blockBasedMappings((block, localAssign.lhs.name)) = mutable.Set(maxVal)

              localAssign.lhs.ssa_id = blockBasedMappings((block, localAssign.lhs.name))
              varMaxTracker(localAssign.lhs.name) = blockBasedMappings((block, localAssign.lhs.name)).max + 1
            }
            case memoryAssign: MemoryAssign => {
              transformVariables(memoryAssign.lhs.variables, block, proc)
              transformVariables(memoryAssign.rhs.variables, block, proc)
            }
            case assume: Assume => {
              transformVariables(assume.body.variables, block, proc)
            } // no required for analyses
            case assert: Assert => {
              transformVariables(assert.body.variables, block, proc)
            } // no required for analyses
            case _ => throw new RuntimeException("No SSA form for " + stmt.getClass + " yet")
          }
        }
        block.jump match {
          case directCall: DirectCall => {
            // TODO: transfers the whole context but it could be using ANR and RNA to transfer only the relevant context
            varMaxTracker.keys.foreach(varr => {
              //context((directCall.target, varr)) = context((directCall.target, varr)) ++ blockBasedMappings(block, varr)
              context.getOrElseUpdate((directCall.target, varr), mutable.Set()) ++= blockBasedMappings((block, varr))
            })
            println(context)
          }
          case indirectCall: IndirectCall => {
            transformVariables(indirectCall.target.variables, block, proc)
          }
          case goTo: GoTo => {
            goTo.targets.foreach(b => {
              varMaxTracker.keys.foreach(varr => {
                blockBasedMappings((b, varr)) = blockBasedMappings(b, varr) ++ blockBasedMappings(block, varr)
              })
            })
          }
          case _ => {}
        }
      }
    }
  }
}
