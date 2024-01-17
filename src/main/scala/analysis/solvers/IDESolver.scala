package analysis.solvers

import analysis.{CfgCallReturnNode, CfgFunctionEntryNode, CfgIDECache, CfgJumpNode, CfgNode, CfgProcedureReturnNode, EdgeFunction, EdgeFunctionLattice, IDEAnalysis, Lambda, Lattice, MapLattice, ProgramCfg}
import ir.Procedure

import scala.collection.mutable



abstract class IDESolver[D, T, L <: Lattice[T]](val cfg: ProgramCfg)
  extends IDEAnalysis[D, T, L]
{
  class Phase1(val cfg: ProgramCfg) extends WorklistFixPointFunctions[(CfgNode, DL , DL), EdgeFunction[T], EdgeFunctionLattice[T, valuelattice.type]]
  {

    val lattice = new MapLattice(edgelattice)

    var x: Map[(CfgNode, DL , DL), EdgeFunction[T]] = _

    val first: Set[(CfgNode, DL, DL)] = Set((cfg.startNode, Right(Lambda()), Right(Lambda())))

    val cache = CfgIDECache()
    cache.cacheCfg(cfg)

    private val callJumpCache = mutable.Map[(CfgFunctionEntryNode, DL, CfgJumpNode), mutable.Map[DL, EdgeFunction[T]]]()
    private val exitJumpCache = mutable.Map[(CfgFunctionEntryNode, DL), mutable.Set[DL]]()


    private def storeCallJump(funentry: CfgFunctionEntryNode, d1: DL, call: CfgJumpNode, e: EdgeFunction[T], d3: DL): Unit = {
      val m = callJumpCache.getOrElseUpdate((funentry, d1, call), mutable.Map[DL, EdgeFunction[T]]())
      m += d3 -> m.getOrElse(d3, edgelattice.bottom).joinWith(e)
    }

    private def storeExitJump(funentry: CfgFunctionEntryNode, d1: DL, d2: DL): Unit =
      exitJumpCache.getOrElseUpdate((funentry, d1), mutable.Set[DL]()) += d2

    import edgelattice.{IdEdge}

    def init: EdgeFunction[T] = IdEdge()

    private def returnflow(d1: DL, d2: DL, funexit: CfgProcedureReturnNode, aftercall: CfgCallReturnNode): Unit = {
      callJumpCache.getOrElseUpdate((cache.entryExitMap(funexit), d1, cache.callReturnMap(aftercall)), mutable.Map[DL, EdgeFunction[T]]()).foreach {
        case (d3, e12) => // d3 is now an item at the caller function entry, and e12 is the composed edge to d1 at the callee entry
          val e3 = x(funexit, d1, d2) // summary edge from d1 to d2 at the callee function
          val e123 = e3.composeWith(e12)
          edgesExitToAfterCall(funexit, aftercall)(d2).foreach {
            case (d4, e4) => // d4 is now an item at the aftercall node, and e4 is the edge from the function exit to the aftercall node
              val e = e4.composeWith(e123) // e is now the composed edge from e3 at the caller entry to d4 at the aftercall node
              propagate(e, (aftercall, d3, d4))
          }
      }
    }

    def process(n: (CfgNode, DL, DL)) = {
      n match {
        case (node, d1, d2) =>
          val e1 = x(n)
          node match {
            case call: CfgJumpNode =>
              if cache.callReturnMap.forwardMap.contains(call) then
                val entry = cache.callees(call)
                edgesCallToEntry(call, entry)(d2).foreach {
                  case (d3, e2) =>
                    // propagate to function entry
                    propagate(IdEdge(), (entry, d3, d3))
                    // cache the composed edge from the entry of the caller to the entry of the callee
                    storeCallJump(entry, d3, call, e2.composeWith(e1), d1)
                    // propagate existing return flow to the after-call node
                    exitJumpCache.getOrElseUpdate((entry, d3), mutable.Set[DL]()).foreach { d4 =>
                      returnflow(d3, d4, cache.entryExitMap(entry), cache.callReturnMap(call))
                    }
                }

                edgesCallToAfterCall(call, cache.callReturnMap(call))(d2).foreach {
                  case (d3, e2) =>
                    propagate(e2.composeWith(e1), (cache.callReturnMap(call), d1, d3))
                }

              else
                edgesOther(node)(d2).foreach {
                  case (d3, e2) =>
                    val e3 = e2.composeWith(e1)
                    node.succInter.foreach { m =>
                      propagate(e3, (m, d1, d3))
                    }
                }

            case exit: CfgProcedureReturnNode =>
              try
                cache.afterCall(exit).foreach(afterCall =>
                  returnflow(d1, d2, exit, afterCall)
                  // TODO
                  if (!exit.succInter.contains(afterCall)) {
                    cfg.addInterprocCallEdge(exit, afterCall)
                  }

                )
                storeExitJump(cache.entryExitMap(exit), d1, d2)
              catch
                case _: NoSuchElementException =>
                case e => throw e

              edgesOther(node)(d2).foreach {
                case (d3, e2) =>
                  val e3 = e2.composeWith(e1)
                  node.succInter.foreach { m =>
                    propagate(e3, (m, d1, d3))
                  }
              }

            case _ =>
              edgesOther(node)(d2).foreach {
                case (d3, e2) =>
                  val e3 = e2.composeWith(e1)
                  node.succInter.foreach { m =>
                    propagate(e3, (m, d1, d3))
                  }
              }
          }
      }
    }

    def summaries(): mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]] = {
      val res = mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]]()
      x.foreach {
        case ((n, d1, d2), e) =>
          n match {
            case exit: CfgProcedureReturnNode =>
              val proc = cache.entryExitMap(exit).data
              val m1 = res.getOrElseUpdate(proc, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]().withDefaultValue(mutable.Map[DL, EdgeFunction[T]]()))
              val m2 = m1.getOrElseUpdate(d1, mutable.Map[DL, EdgeFunction[T]]())
              m2 += d2 -> e
            case _ => // ignore other node kinds
          }
      }
      res
    }
  }

  class Phase2(val cfg: ProgramCfg, val phase1: Phase1) extends WorklistFixPointFunctions[(CfgNode, DL), T, valuelattice.type]:
    val lattice: MapLattice[(CfgNode, DL), T, valuelattice.type] = new MapLattice(valuelattice)
    var x: Map[(CfgNode, DL), T] = _
    val first: Set[(CfgNode, DL)] = Set((cfg.startNode, Right(Lambda())))
    lazy val summaries: mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]] = phase1.summaries()

    def init: T = lattice.sublattice.top

    def process(n: (CfgNode, DL)): Unit = {
      val xnd = x(n)
      n match {
        case(node, d) =>
          node match {
            // function call nodes
            case call: CfgJumpNode =>
              if phase1.cache.callReturnMap.forwardMap.contains(call) then
                val afterCallNode = phase1.cache.callReturnMap(call)
                val entry = phase1.cache.callees(call)
                edgesCallToEntry(call, entry)(d).foreach {
                  case (d2, e) =>
                    propagate(e(xnd), (entry, d2))
                    summaries(entry.data)(d2).foreach {
                      case (d3, e2) =>
                        edgesExitToAfterCall(phase1.cache.entryExitMap(entry), afterCallNode)(d3).foreach {
                          case (d4, e3) =>
                            propagate(e3(e2(e(xnd))), (afterCallNode, d4))
                        }
                    }
                }

                edgesCallToAfterCall(call, afterCallNode)(d).foreach {
                  case (d2, e) =>
                    propagate(e(xnd), (afterCallNode, d2))
                }
              else
                edgesOther(node)(d).foreach{
                  case (d2, e) =>
                    node.succInter.foreach(m => propagate(e(xnd), (m, d2)))
                }

            case _ =>
              edgesOther(node)(d).foreach{
                case (d2, e) =>
                  node.succInter.foreach(m => propagate(e(xnd), (m, d2)))
              }
          }
      }
    }

    val restructuredlattice: MapLattice[CfgNode, Map[D, T], MapLattice[D, T, valuelattice.type]] = new MapLattice(new MapLattice(valuelattice))

    def restructure(y: lattice.Element): restructuredlattice.Element =
      y.foldLeft(Map[CfgNode, Map[D, valuelattice.Element]]()) {
        case (acc, ((n, dl), e)) =>
          dl match {
            case Left(d) => acc + (n -> (acc.getOrElse(n, Map[D, valuelattice.Element]()) + (d -> e)))
            case _ => acc
          }
      }

  def analyze(): Map[CfgNode, Map[D, valuelattice.Element]] = {
    val phase1 = new Phase1(cfg)
    phase1.analyze()
    val phase2 = new Phase2(cfg, phase1)
    phase2.restructure(phase2.analyze())
  }
}
