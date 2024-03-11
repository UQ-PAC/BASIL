package analysis.solvers

import analysis.{BackwardIDEAnalysis, Dependencies, EdgeFunction, EdgeFunctionLattice, ForwardIDEAnalysis, IDEAnalysis, IRInterproceduralBackwardDependencies, IRInterproceduralForwardDependencies, Lambda, Lattice, MapLattice}
import ir.{CFGPosition, Command, DirectCall, GoTo, IRWalk, IndirectCall, InterProcIRCursor, Procedure, Program, Return, end, isAfterCall}
import util.Logger

import scala.collection.immutable.Map
import scala.collection.mutable

/**
 * (A variant of) the IDE analysis algorithm.
 * Adapted from Tip
 * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/IDESolver.scala
 */
abstract class IDESolver[Entrance <: Procedure | Return, Exit <: Procedure | Return, CallNode <: DirectCall | GoTo, AfterCall <: DirectCall | GoTo, D, T, L <: Lattice[T]](val program: Program, val startNode: CFGPosition)
  extends IDEAnalysis[Entrance, Exit, CallNode, AfterCall, D, T, L], Dependencies[CFGPosition] {

  protected def entryToExit(entry: Entrance): Exit
  protected def exitToEntry(exit: Exit): Entrance
  protected def callToAfterCall(call: CallNode): AfterCall
  protected def afterCallToCall(ret: AfterCall): CallNode
  protected def getCallee(call: CallNode): Entrance
  protected def isCall(call: CFGPosition): Boolean
  protected def isExit(exit: CFGPosition): Boolean
  protected def getAfterCalls(exit: Exit): Set[AfterCall]

  /**
   * Phase 1 of the IDE algorithm.
   * Computes Path functions and Summary functions
   * The original version of the algorithm uses summary edges from call nodes to after-call nodes
   * instead of `callJumpCache` and `exitJumpCache`.
   */
  private class Phase1(val program: Program) extends InitializingPushDownWorklistFixpointSolver[(CFGPosition, DL, DL), EdgeFunction[T], EdgeFunctionLattice[T, L]] {

    val lattice: MapLattice[(CFGPosition, DL, DL), EdgeFunction[T], EdgeFunctionLattice[T, L]] = MapLattice(edgelattice)
    val first: Set[(CFGPosition, DL, DL)] = Set((startNode, Right(Lambda()), Right(Lambda())))

    /**
     * callJumpCache(funentry, d1, call)(d3) returns the composition of the edges (call.funentry, d3) -> (call, *) -> (funentry, d1).
     * Allows faster lookup than scanning through the current lattice element.
     */
     private val callJumpCache = mutable.Map[(Entrance, DL, CallNode), mutable.Map[DL, EdgeFunction[T]]]()

    /**
     * exitJumpCache(funentry, d1) contains d2 if there is a non-bottom edge (funentry, d1) -> (funentry.exit, d2).
     * Allows faster lookup than scanning through the current lattice element.
     */
    private val exitJumpCache = mutable.Map[(Entrance, DL), mutable.Set[DL]]()

    private def storeCallJump(funentry: Entrance, d1: DL, call: CallNode, e: EdgeFunction[T], d3: DL): Unit = {
      val m = callJumpCache.getOrElseUpdate((funentry, d1, call), mutable.Map[DL, EdgeFunction[T]]())
      m += d3 -> m.getOrElse(d3, edgelattice.bottom).joinWith(e)
    }

    private def storeExitJump(funentry: Entrance, d1: DL, d2: DL): Unit =
      exitJumpCache.getOrElseUpdate((funentry, d1), mutable.Set[DL]()) += d2


    import edgelattice.IdEdge

    def init: EdgeFunction[T] = IdEdge()

    private def returnflow(d1: DL, d2: DL, funexit: Exit, aftercall: AfterCall): Unit = {
      val entry = exitToEntry(funexit)
      val call = afterCallToCall(aftercall)
      callJumpCache.getOrElseUpdate((entry, d1, call), mutable.Map[DL, EdgeFunction[T]]()).foreach { (d3, e12) =>
        // d3 is now an item at the caller function entry, and e12 is the composed edge to d1 at the callee entry
        val e3 = x(funexit, d1, d2) // summary edge from d1 to d2 at the callee function
        val e123 = e3.composeWith(e12)
        edgesExitToAfterCall(funexit, aftercall)(d2).foreach { (d4, e4) =>
          // d4 is now an item at the aftercall node, and e4 is the edge from the function exit to the aftercall node
          val e = e4.composeWith(e123) // e is now the composed edge from e3 at the caller entry to d4 at the aftercall node
          propagate(e, (aftercall, d3, d4))
        }
      }
    }


    def process(n: (CFGPosition, DL, DL)): Unit = {
      val (position, d1, d2) = n
      val e1 = x(n)
      if (isCall(position)) { // at a call node
        val call: CallNode = position.asInstanceOf[CallNode]
        val entry: Entrance = getCallee(call)
        val ret: AfterCall = callToAfterCall(call)

        edgesCallToEntry(call, entry)(d2).foreach { (d3, e2) =>
          // propagate to function entry
          propagate(IdEdge(), (entry, d3, d3))
          // cache the composed edge from the entry of the caller to the entry of the callee
          storeCallJump(entry, d3, call, e2.composeWith(e1), d1)
          // propagate existing return flow to the after-call node
          exitJumpCache.getOrElseUpdate((entry, d3), mutable.Set[DL]()).foreach { d4 =>
            returnflow(d3, d4, entryToExit(entry), ret)
          }
        }

        edgesCallToAfterCall(call, ret)(d2).foreach { (d3, e2) =>
          propagate(e2.composeWith(e1), (ret, d1, d3))
        }
      } else if (isExit(position)) {
        // at an exit node
        val exit: Exit = position.asInstanceOf[Exit]
        getAfterCalls(exit).foreach(afterCall => returnflow(d1, d2, exit, afterCall))
        storeExitJump(exitToEntry(exit), d1, d2)
      } else {
        edgesOther(position)(d2).foreach { (d3, e2) =>
          val e3 = e2.composeWith(e1)
          outdep(position).foreach { m =>
            propagate(e3, (m, d1, d3))
          }
        }
      }
    }

    def summaries(): mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]] = {
      val res = mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]]()
      x.foreach { case ((n, d1, d2), e) =>
        if (isExit(n)) {
          val exit: Exit = n.asInstanceOf[Exit]
          val proc = IRWalk.procedure(exit)
          val m1 = res.getOrElseUpdate(proc, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]().withDefaultValue(mutable.Map[DL, EdgeFunction[T]]()))
          val m2 = m1.getOrElseUpdate(d1, mutable.Map[DL, EdgeFunction[T]]())
          m2 += d2 -> e
        }
      }
      Logger.debug(s"Function summaries:\n${res.map {
        (f, s) => s"  function $f:\n${s.map {
          (d1, m) => s"${m.map {
            (d2, e) => s"    ($d1,$d2): $e"
          }.mkString("\n")}"
        }.mkString("\n")}"
      }.mkString("\n")} ")
      res
    }
  }

  /**
   * Phase 2 of the IDE algorithm.
   * Performs a forward dataflow analysis using the decomposed lattice and the micro-transformers.
   * The original RHS version of IDE uses jump functions for all nodes, not only at exits, but the analysis result and complexity is the same.
   */
  private class Phase2(val program: Program, val phase1: Phase1) extends InitializingPushDownWorklistFixpointSolver[(CFGPosition, DL), T, L]:
    val lattice: MapLattice[(CFGPosition, DL), T, L] = MapLattice(valuelattice)
    val first: Set[(CFGPosition, DL)] = Set((startNode, Right(Lambda())))

    /**
     * Function summaries from phase 1.
     * Built when first invoked.
     */
    lazy val summaries: mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]] = phase1.summaries()

    def init: T = lattice.sublattice.top

    def process(n: (CFGPosition, DL)): Unit = {
      val xnd = x(n)
      val (position, d) = n
      if (isCall(position)) { // at a call node
        val call: CallNode = position.asInstanceOf[CallNode]
        val entry: Entrance = getCallee(call)
        val ret: AfterCall = callToAfterCall(call)

        edgesCallToEntry(call, entry)(d).foreach { (d2, e) =>
          propagate(e(xnd), (entry, d2))
          summaries(IRWalk.procedure(entry))(d2).foreach { (d3, e2) =>
            edgesExitToAfterCall(entryToExit(entry), ret)(d3).foreach { (d4, e3) =>
              propagate(e3(e2(e(xnd))), (ret, d4))
            }
          }
        }

        edgesCallToAfterCall(call, ret)(d).foreach { (d2, e) =>
          propagate(e(xnd), (ret, d2))
        }
      } else if (isExit(position)) { // at exit node
        // ignore, return flow is handled at the call nodes
      } else {
        edgesOther(position)(d).foreach { (d2, e) =>
          outdep(position).foreach(m => propagate(e(xnd), (m, d2)))
        }
      }
    }

    val restructuredlattice: MapLattice[CFGPosition, Map[D, T], MapLattice[D, T, L]] = MapLattice(MapLattice(valuelattice))

    /**
     * Restructures the analysis output to match `restructuredlattice`.
     */
    def restructure(y: lattice.Element): restructuredlattice.Element =
      y.foldLeft(Map[CFGPosition, Map[D, valuelattice.Element]]()) { case (acc, ((n, dl), e)) =>
        dl match {
          case Left(d) => acc + (n -> (acc.getOrElse(n, Map[D, valuelattice.Element]()) + (d -> e)))
          case _ => acc
        }
      }

  def analyze(): Map[CFGPosition, Map[D, T]] = {
    val phase1 = Phase1(program)
    phase1.analyze()
    val phase2 = Phase2(program, phase1)
    phase2.restructure(phase2.analyze())
  }
}


abstract class ForwardIDESolver[D, T, L <: Lattice[T]](program: Program)
  extends IDESolver[Procedure, Return, DirectCall, GoTo, D, T, L](program, program.mainProcedure),
    ForwardIDEAnalysis[D, T, L], IRInterproceduralForwardDependencies {

  protected def entryToExit(entry: Procedure): Return = entry.returnBlock.jump.asInstanceOf[Return]

  protected def exitToEntry(exit: IndirectCall): Procedure = IRWalk.procedure(exit)

  protected def callToAfterCall(call: DirectCall): GoTo = call.parent.fallthrough.get

  protected def afterCallToCall(ret: GoTo): DirectCall = ret.parent.jump.asInstanceOf[DirectCall]

  protected def getCallee(call: DirectCall): Procedure = call.target

  protected def isCall(call: CFGPosition): Boolean =
    call match
      case directCall: DirectCall => directCall.target.hasImplementation
      case _ => false

  protected def isExit(exit: CFGPosition): Boolean =
    exit match
      // only looking at functions with statements
      case command: Command => IRWalk.procedure(command).end == command
      case _ => false

  protected def getAfterCalls(exit: IndirectCall): Set[GoTo] =
    InterProcIRCursor.succ(exit).foreach(s => assert(s.isInstanceOf[GoTo]))
    InterProcIRCursor.succ(exit).filter(_.isInstanceOf[GoTo]).map(_.asInstanceOf[GoTo])

}


abstract class BackwardIDESolver[D, T, L <: Lattice[T]](program: Program)
  extends IDESolver[Return, Procedure, GoTo, DirectCall, D, T, L](program, program.mainProcedure.end),
    BackwardIDEAnalysis[D, T, L], IRInterproceduralBackwardDependencies {

  protected def entryToExit(entry: Return): Procedure = IRWalk.procedure(entry)

  protected def exitToEntry(exit: Procedure): Return = exit.end.asInstanceOf[Return]

  protected def callToAfterCall(call: GoTo): DirectCall = call.parent.jump.asInstanceOf[DirectCall]

  protected def afterCallToCall(ret: DirectCall): GoTo = ret.parent.fallthrough.get

  protected def getCallee(call: GoTo): Return = callToAfterCall(call).target.end.asInstanceOf[Return]

  protected def isCall(call: CFGPosition): Boolean =
    call match
      case goto: GoTo if goto.isAfterCall =>
        goto.parent.jump match
          case directCall: DirectCall => directCall.target.hasImplementation
          case _ => false
      case _ => false

  protected def isExit(exit: CFGPosition): Boolean =
    exit match
      case procedure: Procedure => procedure.hasImplementation
      case _ => false

  protected def getAfterCalls(exit: Procedure): Set[DirectCall] = InterProcIRCursor.pred(exit).filter(_.isInstanceOf[DirectCall]).map(_.asInstanceOf[DirectCall])
}
