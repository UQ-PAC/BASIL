package analysis

import analysis.*
import ir.{SignExtend, *}

import scala.collection.mutable

object SSAForm {

  val varMaxTracker = new mutable.HashMap[String, Int]()
  def getMax(varName: String): Int =
    val ret = varMaxTracker.getOrElse(varName, 0)
    varMaxTracker(varName) = ret + 1
    ret

  def applySSA(program: Program): Unit = {
    val blockBasedMappings = new mutable.HashMap[(Block, String), Int]()
    for (proc <- program.procedures) {
      for (block <- proc.blocks) {
        for (stmt <- block.statements) {
          println(stmt)
          stmt match {
            case localAssign: LocalAssign => {
                localAssign.rhs.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), getMax(v.name))
                })
                val maxVal = getMax(localAssign.lhs.name)
                blockBasedMappings((block, localAssign.lhs.name)) = maxVal
                localAssign.lhs.ssa_id = maxVal
            }
            case memoryAssign: MemoryAssign => {
                memoryAssign.lhs.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), getMax(v.name))
                })
                memoryAssign.rhs.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), getMax(v.name))
                })
            }
            case assume: Assume => {
                assume.body.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), getMax(v.name))
                })
            } // no required for analyses
            case assert: Assert => {
                assert.body.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), getMax(v.name))
                })
            } // no required for analyses
            case _ => throw new RuntimeException("No SSA form for " + stmt.getClass + " yet")
          }
        }
        block.jump match {
          case indirectCall: IndirectCall => {
              indirectCall.target.variables.foreach(v => {
                v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), getMax(v.name))
              })
          }
          case _ => {}
        }
      }
    }
  }
}