package analysis.solvers

import analysis.{CfgCallReturnNode, CfgFunctionEntryNode, CfgFunctionExitNode, CfgIDECache, CfgJumpNode, CfgNode, CfgProcedureReturnNode, EdgeFunction, EdgeFunctionLattice, IDEAnalysis, Lambda, Lattice, MapLattice, ProgramCfg}
import ir.Procedure

import scala.collection.mutable

/**
 * (A variant of) the IDE analysis algorithm.
 * Adapted from Tip
 * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/IDESolver.scala
 */
abstract class IDESolver[D, T, L <: Lattice[T]](val cfg: ProgramCfg, val cache: CfgIDECache)
  extends IDEAnalysis[D, T, L]
{

  /**
   * Phase 1 of the IDE algorithm.
   * Computes Path functions and Summary functions
   * The original version of the algorithm uses summary edges from call nodes to after-call nodes
   * instead of `callJumpCache` and `exitJumpCache`.
   */
  class Phase1(val cfg: ProgramCfg) extends WorklistFixPointFunctions[(CfgNode, DL , DL), EdgeFunction[T], EdgeFunctionLattice[T, valuelattice.type]]
  {

    val lattice = new MapLattice(edgelattice)

    // Current Lattice Element
    var x: Map[(CfgNode, DL , DL), EdgeFunction[T]] = _

    val first: Set[(CfgNode, DL, DL)] = Set((cfg.startNode, Right(Lambda()), Right(Lambda())))


    /**
     * callJumpCache(funentry, d1, call)(d3) returns the composition of the edges (call.funentry, d3) -> (call, *) -> (funentry, d1).
     * Allows faster lookup than scanning through the current lattice element.
     */
    private val callJumpCache = mutable.Map[(CfgFunctionEntryNode, DL, CfgJumpNode), mutable.Map[DL, EdgeFunction[T]]]()


    /**
     * exitJumpCache(funentry, d1) contains d2 if there is a non-bottom edge (funentry, d1) -> (funentry.exit, d2).
     * Allows faster lookup than scanning through the current lattice element.
     */
    private val exitJumpCache = mutable.Map[(CfgFunctionEntryNode, DL), mutable.Set[DL]]()


    private def storeCallJump(funentry: CfgFunctionEntryNode, d1: DL, call: CfgJumpNode, e: EdgeFunction[T], d3: DL): Unit = {
      val m = callJumpCache.getOrElseUpdate((funentry, d1, call), mutable.Map[DL, EdgeFunction[T]]())
      m += d3 -> m.getOrElse(d3, edgelattice.bottom).joinWith(e)
    }

    private def storeExitJump(funentry: CfgFunctionEntryNode, d1: DL, d2: DL): Unit =
      exitJumpCache.getOrElseUpdate((funentry, d1), mutable.Set[DL]()) += d2

    import edgelattice.{IdEdge}

    def init: EdgeFunction[T] = IdEdge()

    private def returnflow(d1: DL, d2: DL, funexit: CfgFunctionExitNode, aftercall: CfgCallReturnNode): Unit = {
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

            case exit: CfgFunctionExitNode =>
              try
                cache.afterCall(exit).foreach(afterCall => returnflow(d1, d2, exit, afterCall))
                storeExitJump(cache.entryExitMap(exit), d1, d2)
              catch
                case _: NoSuchElementException =>
                case e => throw e

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
            case exit: CfgFunctionExitNode =>
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

  /**
   * Phase 2 of the IDE algorithm.
   * Performs a forward dataflow analysis using the decomposed lattice and the micro-transformers.
   * The original RHS version of IDE uses jump functions for all nodes, not only at exits, but the analysis result and complexity is the same.
   */
  class Phase2(val cfg: ProgramCfg, val phase1: Phase1) extends WorklistFixPointFunctions[(CfgNode, DL), T, valuelattice.type]:
    val lattice: MapLattice[(CfgNode, DL), T, valuelattice.type] = new MapLattice(valuelattice)
    var x: Map[(CfgNode, DL), T] = _
    val first: Set[(CfgNode, DL)] = Set((cfg.startNode, Right(Lambda())))

    /**
     * Function summaries from phase 1.
     * Built when first invoked.
     */
    lazy val summaries: mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]] = phase1.summaries()

    def init: T = lattice.sublattice.top

    def process(n: (CfgNode, DL)): Unit = {
      val xnd = x(n)
      n match {
        case(node, d) =>
          node match {
            // function call nodes
            case call: CfgJumpNode =>
              if cache.callReturnMap.forwardMap.contains(call) then
                val afterCallNode = cache.callReturnMap(call)
                val entry = cache.callees(call)
                edgesCallToEntry(call, entry)(d).foreach {
                  case (d2, e) =>
                    propagate(e(xnd), (entry, d2))
                    summaries(entry.data)(d2).foreach {
                      case (d3, e2) =>
                        edgesExitToAfterCall(cache.entryExitMap(entry), afterCallNode)(d3).foreach {
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

            case _: CfgFunctionExitNode =>
            case _ =>
              edgesOther(node)(d).foreach{
                case (d2, e) =>
                  node.succInter.foreach(m => propagate(e(xnd), (m, d2)))
              }
          }
      }
    }

    val restructuredlattice: MapLattice[CfgNode, Map[D, T], MapLattice[D, T, valuelattice.type]] = new MapLattice(new MapLattice(valuelattice))

    /**
     * Restructures the analysis output to match `restructuredlattice`.
     */
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
