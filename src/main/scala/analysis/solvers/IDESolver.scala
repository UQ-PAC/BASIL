package analysis.solvers

import analysis.{
  BackwardIDEAnalysis,
  Dependencies,
  EdgeFunction,
  EdgeFunctionLattice,
  ForwardIDEAnalysis,
  IDEAnalysis,
  IRInterproceduralBackwardDependencies,
  IRInterproceduralForwardDependencies,
  Lambda,
  Lattice,
  MapLattice
}
import ir.{
  CFGPosition,
  Command,
  DirectCall,
  IRWalk,
  InterProcIRCursor,
  Jump,
  Procedure,
  Program,
  Return,
  Statement,
  Unreachable
}
import util.Logger

import scala.collection.immutable.Map
import scala.collection.mutable

/** (A variant of) the IDE analysis algorithm. Adapted from Tip
  * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/IDESolver.scala
  */
abstract class IDESolver[
  E <: Procedure | Command,
  EE <: Procedure | Command,
  C <: Command,
  R <: Command,
  D,
  T,
  L <: Lattice[T]
](val program: Program, val startNode: CFGPosition)
    extends IDEAnalysis[E, EE, C, R, D, T, L],
      Dependencies[CFGPosition] {

  protected def entryToExit(entry: E): EE
  protected def exitToEntry(exit: EE): E
  protected def callToReturn(call: C): R
  protected def returnToCall(ret: R): C
  protected def getCallee(call: C): E
  protected def isCall(call: CFGPosition): Boolean
  protected def isExit(exit: CFGPosition): Boolean
  protected def getAfterCalls(exit: EE): Set[R]

  def phase2Init: T = valuelattice.top
  def start: CFGPosition = startNode

  /** Phase 1 of the IDE algorithm. Computes Path functions and Summary functions The original version of the algorithm
    * uses summary edges from call nodes to after-call nodes instead of `callJumpCache` and `exitJumpCache`.
    */
  private class Phase1
      extends InitializingPushDownWorklistFixpointSolver[
        (CFGPosition, DL, DL),
        EdgeFunction[T],
        EdgeFunctionLattice[T, L]
      ] {

    val lattice: MapLattice[(CFGPosition, DL, DL), EdgeFunction[T], EdgeFunctionLattice[T, L]] = MapLattice(edgelattice)
    val first: Set[(CFGPosition, DL, DL)] = Set((start, Right(Lambda()), Right(Lambda())))

    /** callJumpCache(funentry, d1, call)(d3) returns the composition of the edges (call.funentry, d3) -> (call, *) ->
      * (funentry, d1). Allows faster lookup than scanning through the current lattice element.
      */
    private val callJumpCache = mutable.Map[(E, DL, C), mutable.Map[DL, EdgeFunction[T]]]()

    /** exitJumpCache(funentry, d1) contains d2 if there is a non-bottom edge (funentry, d1) -> (funentry.exit, d2).
      * Allows faster lookup than scanning through the current lattice element.
      */
    private val exitJumpCache = mutable.Map[(E, DL), mutable.Set[DL]]()

    private def storeCallJump(funentry: E, d1: DL, call: C, e: EdgeFunction[T], d3: DL): Unit = {
      val m = callJumpCache.getOrElseUpdate((funentry, d1, call), mutable.Map[DL, EdgeFunction[T]]())
      m += d3 -> m.getOrElse(d3, edgelattice.bottom).joinWith(e)
    }

    private def storeExitJump(funentry: E, d1: DL, d2: DL): Unit =
      exitJumpCache.getOrElseUpdate((funentry, d1), mutable.Set[DL]()) += d2

    import edgelattice.IdEdge

    def init: EdgeFunction[T] = IdEdge()

    private def returnflow(d1: DL, d2: DL, funexit: EE, aftercall: R): Unit = {
      val entry = exitToEntry(funexit)
      val call = returnToCall(aftercall)
      callJumpCache.getOrElseUpdate((entry, d1, call), mutable.Map[DL, EdgeFunction[T]]()).foreach { (d3, e12) =>
        // d3 is now an item at the caller function entry, and e12 is the composed edge to d1 at the callee entry
        val e3 = x(funexit, d1, d2) // summary edge from d1 to d2 at the callee function
        val e123 = e3.composeWith(e12)
        edgesExitToAfterCall(funexit, aftercall)(d2).foreach { (d4, e4) =>
          // d4 is now an item at the aftercall node, and e4 is the edge from the function exit to the aftercall node
          val e =
            e4.composeWith(e123) // e is now the composed edge from e3 at the caller entry to d4 at the aftercall node
          propagate(e, (aftercall, d3, d4))
        }
      }
    }

    def process(n: (CFGPosition, DL, DL)): Unit = {
      val (position, d1, d2) = n
      val e1 = x(n)
      if (isCall(position)) { // at a call node
        val call: C = position.asInstanceOf[C]
        val entry: E = getCallee(call)
        val ret: R = callToReturn(call)

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
        val exit: EE = position.asInstanceOf[EE]
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
      this.analyze()

      val res = mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]]()
      x.foreach { case ((n, d1, d2), e) =>
        if (isExit(n)) {
          val exit: EE = n.asInstanceOf[EE]
          val proc = IRWalk.procedure(exit)
          val m1 = res.getOrElseUpdate(
            proc,
            mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]().withDefaultValue(mutable.Map[DL, EdgeFunction[T]]())
          )
          val m2 = m1.getOrElseUpdate(d1, mutable.Map[DL, EdgeFunction[T]]())
          m2 += d2 -> e
        }
      }
      Logger.debug(s"Function summaries:\n${res
          .map { (f, s) =>
            s"  function $f:\n${s
                .map { (d1, m) =>
                  s"${m
                      .map { (d2, e) =>
                        s"    ($d1,$d2): $e"
                      }
                      .mkString("\n")}"
                }
                .mkString("\n")}"
          }
          .mkString("\n")} ")
      res
    }
  }

  /** Phase 2 of the IDE algorithm. Performs a forward dataflow analysis using the decomposed lattice and the
    * micro-transformers. The original RHS version of IDE uses jump functions for all nodes, not only at exits, but the
    * analysis result and complexity is the same.
    */
  private class Phase2(val phase1: Phase1) extends InitializingPushDownWorklistFixpointSolver[(CFGPosition, DL), T, L] {
    val lattice: MapLattice[(CFGPosition, DL), T, L] = MapLattice(valuelattice)
    val first: Set[(CFGPosition, DL)] = Set((start, Right(Lambda())))

    /** Function summaries from phase 1. Built when first invoked.
      */
    lazy val summaries: mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]] = phase1.summaries()

    def init: T = phase2Init

    def process(n: (CFGPosition, DL)): Unit = {
      val xnd = x(n)
      val (position, d) = n
      if (isCall(position)) { // at a call node
        val call: C = position.asInstanceOf[C]
        val entry: E = getCallee(call)
        val ret: R = callToReturn(call)

        edgesCallToEntry(call, entry)(d).foreach { (d2, e) =>
          propagate(e(xnd), (entry, d2))
          summaries.get(IRWalk.procedure(entry)).foreach {
            _(d2).foreach { (d3, e2) =>
              edgesExitToAfterCall(entryToExit(entry), ret)(d3).foreach { (d4, e3) =>
                propagate(e3(e2(e(xnd))), (ret, d4))
              }
            }
          }
        }

        edgesCallToAfterCall(call, ret)(d).foreach { (d2, e) =>
          propagate(e(xnd), (ret, d2))
        }
      } else if (isExit(position)) { // at exit node
        // ignore, return flow is handled at the call nodes
        // temporary fix, propagate results into the out params of a return
        edgesOther(position)(d).foreach { (d2, e) =>
          propagate(e(xnd), (position, d2))
        }
      } else {
        edgesOther(position)(d).foreach { (d2, e) =>
          outdep(position).foreach(m => propagate(e(xnd), (m, d2)))
        }
      }
    }

    val restructuredlattice: MapLattice[CFGPosition, Map[D, T], MapLattice[D, T, L]] = MapLattice(
      MapLattice(valuelattice)
    )

    /** Restructures the analysis output to match `restructuredlattice`.
      */
    def restructure(y: lattice.Element): restructuredlattice.Element = {
      y.foldLeft(Map[CFGPosition, Map[D, valuelattice.Element]]()) { case (acc, ((n, dl), e)) =>
        dl match {
          case Left(d) => acc + (n -> (acc.getOrElse(n, Map[D, valuelattice.Element]()) + (d -> e)))
          case _ => acc
        }
      }
    }
  }

  def analyze(): Map[CFGPosition, Map[D, T]] = {
    val phase1 = Phase1()
    val phase2 = Phase2(phase1)
    phase2.restructure(phase2.analyze())
  }
}

abstract class ForwardIDESolver[D, T, L <: Lattice[T]](program: Program, entry: Option[Procedure] = None)
    extends IDESolver[Procedure, Return, DirectCall, Command, D, T, L](program, entry.getOrElse(program.mainProcedure)),
      ForwardIDEAnalysis[D, T, L],
      IRInterproceduralForwardDependencies {

  protected def entryToExit(entry: Procedure): Return = IRWalk.lastInProc(entry).get.asInstanceOf[Return]

  protected def exitToEntry(exit: Return): Procedure = IRWalk.procedure(exit)

  protected def callToReturn(call: DirectCall): Command = call.successor

  protected def returnToCall(ret: Command): DirectCall = ret match {
    case ret: Statement => ret.parent.statements.getPrev(ret).asInstanceOf[DirectCall]
    case _: Jump => ret.parent.statements.last.asInstanceOf[DirectCall]
  }

  protected def getCallee(call: DirectCall): Procedure = {
    require(isCall(call))
    call.target
  }

  protected def isCall(call: CFGPosition): Boolean = {
    call match {
      case directCall: DirectCall
          if !directCall.successor.isInstanceOf[
            Unreachable
          ] && directCall.target.returnBlock.isDefined && directCall.target.entryBlock.isDefined =>
        true
      case _ => false
    }
  }

  protected def isExit(exit: CFGPosition): Boolean = {
    exit match {
      // only looking at functions with statements
      case _: Return => true
      case _ => false
    }
  }

  protected def getAfterCalls(exit: Return): Set[Command] =
    InterProcIRCursor.succ(exit).filter(_.isInstanceOf[Command]).map(_.asInstanceOf[Command])
}

abstract class BackwardIDESolver[D, T, L <: Lattice[T]](program: Program, entry: Option[Procedure] = None)
    extends IDESolver[Return, Procedure, Command, DirectCall, D, T, L](
      program, {
        val e = entry.getOrElse(program.mainProcedure)
        IRWalk.lastInProc(e).getOrElse(e)
      }
    ),
      BackwardIDEAnalysis[D, T, L],
      IRInterproceduralBackwardDependencies {

  protected def entryToExit(entry: Return): Procedure = IRWalk.procedure(entry)

  protected def exitToEntry(exit: Procedure): Return = exit.returnBlock.get.jump.asInstanceOf[Return]

  protected def callToReturn(call: Command): DirectCall = {
    IRWalk.prevCommandInBlock(call) match {
      case Some(x: DirectCall) => x
      case p => throw Exception(s"Not a return/aftercall node $call  .... prev = $p")
    }
  }

  protected def returnToCall(ret: DirectCall): Command = ret.successor

  protected def getCallee(call: Command): Return = {
    require(isCall(call))
    val procCalled = callToReturn(call).target
    procCalled.returnBlock
      .getOrElse(throw Exception(s"No return node for procedure ${procCalled}"))
      .jump
      .asInstanceOf[Return]
  }

  protected def isCall(call: CFGPosition): Boolean = {
    call match {
      case _: Unreachable => false /* don't process non-returning calls */
      case c: Command =>
        val call = IRWalk.prevCommandInBlock(c)
        call match {
          case Some(d: DirectCall) if d.target.returnBlock.isDefined => true
          case _ => false
        }
      case _ => false
    }
  }

  protected def isExit(exit: CFGPosition): Boolean = {
    exit match {
      case procedure: Procedure => procedure.blocks.nonEmpty
      case _ => false
    }
  }

  protected def getAfterCalls(exit: Procedure): Set[DirectCall] = exit.incomingCalls().toSet
}
