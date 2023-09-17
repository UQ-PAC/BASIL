package analysis

import analysis.solvers.*
import ir.*

import scala.collection.mutable
import scala.collection.mutable.{HashMap, ListBuffer}

//Static Single Assignment (SSA) form
//Takes a program and normalises it based on that from

class SSA(cfg: Cfg) {
    val variableTracker: mutable.Map[String, Int] = mutable.HashMap()
    val visited: mutable.Set[CfgNode] = mutable.HashSet()

    def analyze(): Unit =
    // generate the constraints by traversing the AST and solve them on-the-fly

        for (entry <- cfg.entries) {
          entry match {
            case _: CfgFunctionEntryNode =>
              visit(entry, ())
            case _ =>
          }
        }
      post_SSA(cfg)

    def checkAssignment(name: String): Int = {
        if (variableTracker.contains(name)) {
            variableTracker(name) += 1
        } else {
            variableTracker(name) = 1
        }
        variableTracker(name)
    }

    def checkUse(name: String): Int = {
        if (!variableTracker.contains(name)) {
            variableTracker(name) = 1
        }
        variableTracker(name)
    }

    def visit(node: CfgNode, arg: Unit): Unit = {
        if (visited.contains(node)) {
            return
        }
        visited.add(node)
        node match {
          case statementNode: CfgCommandNode =>
            statementNode.data match {
              case localAssign: LocalAssign =>
                checkExpr(localAssign.rhs)
                //localAssign.lhs = LocalVar(s"${localAssign.lhs.name}_${checkAssignment(localAssign.lhs.name)}", localAssign.lhs.size)
                localAssign.lhs.ssa_id = checkAssignment(localAssign.lhs.toString)
                //localAssign.rhs.ssa_id = checkUse(localAssign.rhs.toString)

              case memAssign: MemoryAssign =>
                checkExpr(memAssign.rhs.value)
                //                memAssign.rhs = Store(memAssign.rhs.memory, s"${memAssign.rhs.index}_${checkAssignment(memAssign.rhs.index.toString)}", value, memAssign.rhs.endian, memAssign.rhs.size)
                //memAssign.rhs.index.ssa_id = checkAssignment(memAssign.rhs.index.toString)
                checkExpr(memAssign.rhs.index)
                //memAssign.rhs.value.ssa_id = checkUse(memAssign.rhs.value.toString)

              case _ =>
            }
          case _ =>
        }
        for (child <- node.succ) {
          visit(child, ())
        }
    }

    def checkExpr(expr: Expr): Unit = {
      //print(s"Checking expression: ${expr}\n")
      expr match {
        case binExpr: BinaryExpr =>
          checkExpr(binExpr.arg1)
          checkExpr(binExpr.arg2)
        case unExpr: UnaryExpr =>
          checkExpr(unExpr.arg)
        case zeroExtend: ZeroExtend =>
          checkExpr(zeroExtend.body)
        case memoryLoad: MemoryLoad =>
          checkExpr(memoryLoad.index)
          checkExpr(memoryLoad.mem)
        case variable: Variable =>
          variable.ssa_id = checkUse(variable.toString)
        case extract: Extract =>
          checkExpr(extract.body)
        case signExtend: SignExtend =>
          checkExpr(signExtend.body)
        case memory: Memory =>
        case bitVecLiteral: BitVecLiteral =>
        case _ => println(s"WARNING: Unhandled expression type in SSA: ${expr.getClass}")
      }
    }

//    def replacer(node: Object): Unit = {
//        node match {
//            case localAssign: LocalAssign =>
//                val newVar = LocalVar(s"${localAssign.lhs.name}_${checkAssignment(localAssign.lhs.name)}", localAssign.lhs.size)
//                val newExpr = localAssign.rhs match {
//                    case expr: Expr =>
//                        expr match {
//                            case binExpr: BinOp =>
//                                val newLeft = binExpr.lhs match {
//                                    case local: LocalVar => LocalVar(s"${local.name}_${checkUse(local.name)}", local.size)
//                                    case _ => binExpr.lhs
//                                }
//                                val newRight = binExpr.rhs match {
//                                    case local: LocalVar => LocalVar(s"${local.name}_${checkUse(local.name)}", local.size)
//                                    case _ => binExpr.rhs
//                                }
//                                BinOp(newLeft, binExpr.op, newRight)
//                            case unExpr: UnOp =>
//                              val newRight = unExpr.expr match {
//                                case local: LocalVar => LocalVar(s"${local.name}_${checkUse(local.name)}", local.size)
//                                case _ => unExpr.expr
//                              }
//                        }
//                    case _ => localAssign.rhs
//                }
//                LocalAssign(newVar, newExpr, localAssign.line, localAssign.instruction)
//            case memAssign: MemAssign =>
//            case _ => node
//        }
//        visitChildren(node, ())
//
//      def exprChecker(node: Object): Expr = {
//        node match {
//            case binExpr: BinOp =>
//                BinOp(binExpr.operator, exprChecker(binExpr.lhs), exprChecker(binExpr.rhs))
//            case unExpr: UnOp =>
//                UnOp(unExpr.operator, exprChecker(unExpr.exp))
//            case store: Store =>
//                Store(exprChecker(store.memory))
//        }
//      }
//    }


//    def visitChildren(node: Object, arg: Unit): Unit = {
//        node match {
//            case cfg: Cfg =>
//              cfg.nodes.foreach(visit(_, ()))
//
//            case node: CfgNode =>
//              node match {
//                case stmtNode: CfgCommandNode =>
//                  visit(stmtNode.data, ())
//                case _ =>
//              }
//
//            case program: Program =>
//                program.procedures.foreach(visit(_, ()))
//
//            case function: Procedure =>
//                function.blocks.foreach(visit(_, ()))
//
//            case block: Block =>
//                block.statements.foreach(visit(_, ()))
//                block.jumps.foreach(visit(_, ()))
//
//            case _ => // ignore other kinds of nodes
//
//        }
//    }

    def post_SSA(cfg: Cfg): Unit = {
      cfg.nodes.foreach {
        case statementNode: CfgCommandNode =>
          resolve_phi(statementNode, cfg)
        case _ =>
      }
    }

    def get_incoming_edges(cfg: Cfg, node: CfgNode): ListBuffer[Edge] = {
      val incomingEdges: ListBuffer[Edge] = ListBuffer()
      for (e <- cfg.getEdges.toList) {
        if (e.getTo.equals(node)) {
          incomingEdges.addOne(e)
        }
      }
      incomingEdges
    }

    def resolve_phi(node: CfgNode, cfg: Cfg): Unit = {

      def add_map(map: mutable.Map[Expr, ListBuffer[Expr]], key: Expr, value: Expr): Unit = {
        if (map.contains(key)) {
          map(key) += value
        } else {
          map(key) = ListBuffer(value)
        }
      }

      val incomingEdges = get_incoming_edges(cfg, node)
      //val incomingMatches: ListBuffer[Statement] = ListBuffer()
      var incomingMatches: mutable.Map[Expr, ListBuffer[Expr]] = mutable.Map[Expr, ListBuffer[Expr]]()
      val incomingMatches2: mutable.Map[Expr, ListBuffer[Expr]] = mutable.Map[Expr, ListBuffer[Expr]]()
      for (e <- incomingEdges) {
        node match {
          case stmtNode: CfgCommandNode =>
            stmtNode.data match
              case memAssign: MemoryAssign =>
                memAssign.rhs.value.variables.foreach(l => {
                  e.getFrom match {
                    case fromStmtNode: CfgCommandNode =>
                      fromStmtNode.data match
                        case memAssign2: MemoryAssign =>
                          if (memAssign2.rhs.index.variables.contains(l)) {
                            memAssign2.rhs.index.variables.foreach(l2 => {
                              if (l2.equals(l)) {
                                add_map(incomingMatches, l, l2)
                                add_map(incomingMatches2, memAssign.rhs.value, memAssign2.rhs.index)
                              }
                            })
                          }
                        case localAssign: LocalAssign =>
                          if (localAssign.lhs.variables.contains(l)) {
                            localAssign.lhs.variables.foreach(l2 => {
                              if (l2.equals(l)) {
                                add_map(incomingMatches, l, l2)
                                add_map(incomingMatches2, memAssign.rhs.value, localAssign.lhs)
                              }
                            })
                          }
                        case _ =>
                    case _ =>
                  }
                })
              case localAssign: LocalAssign =>
                localAssign.rhs.variables.foreach(l => {
                  e.getFrom match {
                    case fromStmtNode: CfgCommandNode =>
                      fromStmtNode.data match
                        case memAssign: MemoryAssign =>
                          if (memAssign.rhs.index.variables.contains(l)) {
                            memAssign.rhs.index.variables.foreach(l2 => {
                              if (l2.equals(l)) {
                                add_map(incomingMatches, l, l2)
                                add_map(incomingMatches2, localAssign.rhs, memAssign.rhs.index)
                              }
                            })
                          }
                        case localAssign2: LocalAssign =>
                          if (localAssign2.lhs.variables.contains(l)) {
                            localAssign2.lhs.variables.foreach(l2 => {
                              if (l2.equals(l)) {
                                add_map(incomingMatches, l, l2)
                                add_map(incomingMatches2, localAssign.rhs, localAssign2.lhs)
                              }
                            })
                          }
                        case _ =>
                    case _ =>
                  }
                })
              case _ =>
          case _ =>
        }
      }


//      // for whole expression
//      for (e <- incomingEdges) {
//        node match {
//          case stmtNode: CfgCommandNode =>
//            stmtNode.data match
//              case memAssign: MemAssign =>
//                val l = memAssign.rhs.value
//                  e.getFrom() match {
//                    case fromStmtNode: CfgCommandNode =>
//                      fromStmtNode.data match
//                        case memAssign2: MemAssign =>
//                            val l2 = memAssign2.rhs.index
//                            if (l2.equals(l)) {
//                              add_map(incomingMatches, l, l2)
//                            }
//                        case localAssign: LocalAssign =>
//                            val l2 = localAssign.lhs
//                            if (l2.equals(l)) {
//                              add_map(incomingMatches, l, l2)
//                            }
//                        case _ =>
//                    case _ =>
//                  }
//              case localAssign: LocalAssign =>
//                val l = localAssign.rhs
//                  e.getFrom() match {
//                    case fromStmtNode: CfgCommandNode =>
//                      fromStmtNode.data match
//                        case memAssign: MemAssign =>
//                            val l2 = memAssign.rhs.index
//                            if (l2.equals(l)) {
//                              add_map(incomingMatches, l, l2)
//                            }
//                        case localAssign2: LocalAssign =>
//                            val l2 = localAssign2.lhs
//                            if (l2.equals(l)) {
//                              add_map(incomingMatches, l, l2)
//                            }
//                        case _ =>
//                    case _ =>
//                  }
//              case _ =>
//          case _ =>
//        }
//      }

      incomingMatches = incomingMatches.concat(incomingMatches2)

      // Flow corrector: replace phi with incoming edges
      // TODO: needs to be fixed cause it just selects the highest value of the incoming edges
      incomingMatches.foreach((key, value) =>
        if (value.length == 1) {
            // replace key with value
            key.ssa_id = value.head.ssa_id
            } else if (value.length > 1) {
            // replace key with phi
            value.foreach(v =>
              if (v.ssa_id > key.ssa_id) {
                key.ssa_id = v.ssa_id
              }
            )
            } else {
            // no incoming edges
        }
      )

//      incomingMatches2.foreach((key, value) =>
//        print(s"key: ($key, ${key.ssa_id}), value: ")
//        value.foreach(v => print(s"($v,${v.ssa_id})"))
//        println()
//      )


        //TODO: print this if required
//      incomingMatches.foreach((key, value) =>
//        print(s"key: ($key, ${key.ssa_id}), value: ")
//        value.foreach(v => print(s"($v,${v.ssa_id})"))
//        println()
//      )
    }


//    def resolve_phi_rec(node: CfgNode, cfg: Cfg): mutable.Map[Expr, ListBuffer[Expr]] = {
//
//      def add_map(map: mutable.Map[Expr, ListBuffer[Expr]], key: Expr, value: Expr): Unit = {
//        if (map.contains(key)) {
//          map(key) += value
//        } else {
//          map(key) = ListBuffer(value)
//        }
//      }
//
//      val incomingEdges = get_incoming_edges(cfg, node)
//      val incomingMatches: mutable.Map[Expr, ListBuffer[Expr]] = mutable.Map[Expr, ListBuffer[Expr]]()
//      val incomingMatches2: mutable.Map[Expr, ListBuffer[Expr]] = mutable.Map[Expr, ListBuffer[Expr]]()
//
//      for (e <- incomingEdges) {
//        node match {
//          case stmtNode: CfgCommandNode =>
//            stmtNode.data match
//              case memAssign: MemAssign =>
//                memAssign.rhs.value.locals.foreach(l => {
//                  e.getFrom() match {
//                    case fromStmtNode: CfgCommandNode =>
//                      fromStmtNode.data match
//                        case memAssign2: MemAssign =>
//                          if (memAssign2.rhs.index.locals.contains(l)) {
//                            memAssign2.rhs.index.locals.foreach(l2 => {
//                              if (l2.equals(l)) {
//                                add_map(incomingMatches, l, l2)
//                                add_map(incomingMatches2, memAssign.rhs.value, memAssign2.rhs.index)
//                              }
//                            })
//                          }
//                        case localAssign: LocalAssign =>
//                          if (localAssign.lhs.locals.contains(l)) {
//                            localAssign.lhs.locals.foreach(l2 => {
//                              if (l2.equals(l)) {
//                                add_map(incomingMatches, l, l2)
//                                add_map(incomingMatches2, memAssign.rhs.value, localAssign.lhs)
//                              }
//                            })
//                          }
//                        case _ =>
//                    case _ =>
//                  }
//                })
//              case localAssign: LocalAssign =>
//                localAssign.rhs.locals.foreach(l => {
//                  e.getFrom() match {
//                    case fromStmtNode: CfgCommandNode =>
//                      fromStmtNode.data match
//                        case memAssign: MemAssign =>
//                          if (memAssign.rhs.index.locals.contains(l)) {
//                            memAssign.rhs.index.locals.foreach(l2 => {
//                              if (l2.equals(l)) {
//                                add_map(incomingMatches, l, l2)
//                                add_map(incomingMatches2, localAssign.rhs, memAssign.rhs.index)
//                              }
//                            })
//                          }
//                        case localAssign2: LocalAssign =>
//                          if (localAssign2.lhs.locals.contains(l)) {
//                            localAssign2.lhs.locals.foreach(l2 => {
//                              if (l2.equals(l)) {
//                                add_map(incomingMatches, l, l2)
//                                add_map(incomingMatches2, localAssign.rhs, localAssign2.lhs)
//                              }
//                            })
//                          }
//                        case _ =>
//                    case _ =>
//                  }
//                })
//              case _ =>
//          case _ =>
//        }
//      }
//      incomingMatches
//    }
}