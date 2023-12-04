package analysis

import analysis.*
import ir.{SignExtend, *}

import scala.collection.mutable

object SSAForm {

//  def extractVariables(expr: Expr, vars: Set[Variable]): Set[Variable] = {
//    expr match {
//      case signExtend: SignExtend => extractVariables(signExtend.body, vars)
//      case v: Variable => vars + v
//      case extract: Extract => extractVariables(extract.body, vars)
//      case memoryStore: MemoryStore => extractVariables(memoryStore.index, vars) ++ extractVariables(memoryStore.value, vars)
//      case zeroExtend: ZeroExtend => extractVariables(zeroExtend.body, vars)
//      case unaryExpr: UnaryExpr => extractVariables(unaryExpr.arg, vars)
//      case repeat: Repeat => extractVariables(repeat.body, vars)
//      case memoryLoad: MemoryLoad => extractVariables(memoryLoad.index, vars)
//      case binaryExpr: BinaryExpr => extractVariables(binaryExpr.arg1, vars) ++ extractVariables(binaryExpr.arg2, vars)
//      case _ => vars
//    }
//  }

  def applySSA(program: Program, invasive: Boolean = false): Unit = {
    for (proc <- program.procedures) {
      val variableMapping = new mutable.HashMap[String, Int]().withDefault(_ => 0)
      for (block <- proc.blocks) {
        for (stmt <- block.statements) {
          println(stmt)
          stmt match {
            case localAssign: LocalAssign => {
              if (invasive) {
                localAssign.rhs.variables.foreach(v => {
                  v.name = v.name + "_" + variableMapping(v.name)
                })
                variableMapping(localAssign.lhs.name) += 1
                localAssign.lhs.name = localAssign.lhs.name + "_" + variableMapping(localAssign.lhs.name)
              } else {
                localAssign.rhs.variables.foreach(v => {
                  v.ssa_id = variableMapping(v.name)
                })
                variableMapping(localAssign.lhs.name) += 1
                localAssign.lhs.ssa_id = variableMapping(localAssign.lhs.name)
              }
            }
            case memoryAssign: MemoryAssign => {
              if (invasive) {
                memoryAssign.lhs.variables.foreach(v => {
                  v.name = v.name + "_" + variableMapping(v.name)
                })
                memoryAssign.rhs.variables.foreach(v => {
                  v.name = v.name + "_" + variableMapping(v.name)
                })
              } else {
                memoryAssign.lhs.variables.foreach(v => {
                  v.ssa_id = variableMapping(v.name)
                })
                memoryAssign.rhs.variables.foreach(v => {
                  v.ssa_id = variableMapping(v.name)
                })
              }
            }
            case assume: Assume => {
              if (invasive) {
                assume.body.variables.foreach(v => {
                  v.name = v.name + "_" + variableMapping(v.name)
                })
              } else {
                assume.body.variables.foreach(v => {
                  v.ssa_id = variableMapping(v.name)
                })
              }
            } // no required for analyses
            case assume: Assert => {
              if (invasive) {
                assume.body.variables.foreach(v => {
                  v.name = v.name + "_" + variableMapping(v.name)
                })
              } else {
                assume.body.variables.foreach(v => {
                  v.ssa_id = variableMapping(v.name)
                })
              }
            } // no required for analyses
            case _ => throw new RuntimeException("No SSA form for " + stmt.getClass + " yet")
          }
        }
        block.jump match {
          case indirectCall: IndirectCall => {
            if (invasive) {
                indirectCall.target.variables.foreach(v => {
                    v.name = v.name + "_" + variableMapping(v.name)
                })
                } else {
                indirectCall.target.variables.foreach(v => {
                    v.ssa_id = variableMapping(v.name)
                })
            }
          }
          case _ => {}
        }
      }
    }
  }
}
