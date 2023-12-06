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

//  def applySSA(program: Program, invasive: Boolean = false): Unit = {
//    val variableMapping = new mutable.HashMap[String, Int]().withDefault(_ => 0)
//    for (proc <- program.procedures) {
//      val blockBasedMappings = new mutable.HashMap[Block, mutable.Map[String, Int]]()
//      variableMapping.keys.foreach(key => variableMapping.update(key, variableMapping(key) + 1))
//      for (block <- proc.blocks) {
//        blockBasedMappings.update(block, new mutable.HashMap[String, Int]().withDefault(_ => 0))
//        for (stmt <- block.statements) {
//          println(stmt)
//          stmt match {
//            case localAssign: LocalAssign => {
//              if (invasive) {
//                localAssign.rhs.variables.foreach(v => {
//                  v.name = v.name + "_" + variableMapping(v.name)
//                })
//                variableMapping(localAssign.lhs.name) += 1
//                localAssign.lhs.name = localAssign.lhs.name + "_" + variableMapping(localAssign.lhs.name)
//              } else {
//                localAssign.rhs.variables.foreach(v => {
//                  v.ssa_id = variableMapping(v.name)
//                })
//                variableMapping(localAssign.lhs.name) += 1
//                localAssign.lhs.ssa_id = variableMapping(localAssign.lhs.name)
//              }
//            }
//            case memoryAssign: MemoryAssign => {
//              if (invasive) {
//                memoryAssign.lhs.variables.foreach(v => {
//                  v.name = v.name + "_" + variableMapping(v.name)
//                })
//                memoryAssign.rhs.variables.foreach(v => {
//                  v.name = v.name + "_" + variableMapping(v.name)
//                })
//              } else {
//                memoryAssign.lhs.variables.foreach(v => {
//                  v.ssa_id = variableMapping(v.name)
//                })
//                memoryAssign.rhs.variables.foreach(v => {
//                  v.ssa_id = variableMapping(v.name)
//                })
//              }
//            }
//            case assume: Assume => {
//              if (invasive) {
//                assume.body.variables.foreach(v => {
//                  v.name = v.name + "_" + variableMapping(v.name)
//                })
//              } else {
//                assume.body.variables.foreach(v => {
//                  v.ssa_id = variableMapping(v.name)
//                })
//              }
//            } // no required for analyses
//            case assume: Assert => {
//              if (invasive) {
//                assume.body.variables.foreach(v => {
//                  v.name = v.name + "_" + variableMapping(v.name)
//                })
//              } else {
//                assume.body.variables.foreach(v => {
//                  v.ssa_id = variableMapping(v.name)
//                })
//              }
//            } // no required for analyses
//            case _ => throw new RuntimeException("No SSA form for " + stmt.getClass + " yet")
//          }
//        }
//        block.jump match {
//          case indirectCall: IndirectCall => {
//            if (invasive) {
//                indirectCall.target.variables.foreach(v => {
//                    v.name = v.name + "_" + variableMapping(v.name)
//                })
//                } else {
//                indirectCall.target.variables.foreach(v => {
//                    v.ssa_id = variableMapping(v.name)
//                })
//            }
//          }
//          case _ => {}
//        }
//      }
//    }
//  }

  def applySSA(program: Program): Unit = {
    val varMaxTracker = new mutable.HashMap[String, Int]()
    val blockBasedMappings = new mutable.HashMap[(Block, String), mutable.Set[Int]]().withDefault(_ => mutable.Set())
    val context = new mutable.HashMap[(Procedure, String), mutable.Set[Int]]().withDefault(_ => mutable.Set())
    for (proc <- program.procedures) {
      for (block <- proc.blocks) {
        for (stmt <- block.statements) {
          println(stmt)
          stmt match {
            case localAssign: LocalAssign => {
                localAssign.rhs.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), context.getOrElseUpdate((proc, v.name), mutable.Set()))
                })
                val maxVal = varMaxTracker.getOrElseUpdate(localAssign.lhs.name, 0)
                blockBasedMappings((block, localAssign.lhs.name)) = mutable.Set(maxVal)

                localAssign.lhs.ssa_id = blockBasedMappings((block, localAssign.lhs.name))
                varMaxTracker(localAssign.lhs.name) = blockBasedMappings((block, localAssign.lhs.name)).max + 1
                println("test")
            }
            case memoryAssign: MemoryAssign => {
                memoryAssign.lhs.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), context.getOrElseUpdate((proc, v.name), mutable.Set()))
                })
                memoryAssign.rhs.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), context.getOrElseUpdate((proc, v.name), mutable.Set()))
                })
            }
            case assume: Assume => {
                assume.body.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), context.getOrElseUpdate((proc, v.name), mutable.Set()))
                })
            } // no required for analyses
            case assume: Assert => {
                assume.body.variables.foreach(v => {
                  v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), context.getOrElseUpdate((proc, v.name), mutable.Set()))
                })
            } // no required for analyses
            case _ => throw new RuntimeException("No SSA form for " + stmt.getClass + " yet")
          }
        }
        block.jump match {
          case directCall: DirectCall => {
            varMaxTracker.keys.foreach(varr => {
              //context((directCall.target, varr)) = context((directCall.target, varr)) ++ blockBasedMappings(block, varr)
              context.getOrElseUpdate((directCall.target, varr), mutable.Set()) ++= blockBasedMappings((block, varr))
            })
            println(context)
          }
          case indirectCall: IndirectCall => {
              indirectCall.target.variables.foreach(v => {
                v.ssa_id = blockBasedMappings.getOrElseUpdate((block, v.name), context.getOrElseUpdate((proc, v.name), mutable.Set()))
              })
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
