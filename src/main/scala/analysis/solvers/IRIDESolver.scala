package analysis.solvers

import analysis.{BackwardIDEAnalysis, Dependencies, EdgeFunction, EdgeFunctionLattice, ForwardIDEAnalysis, IRIDEAnalysis, IRIDECache, IRInterproceduralBackwardDependencies, IRInterproceduralForwardDependencies, Lambda, Lattice, MapLattice}
import ir.{Block, CFGPosition, Command, DirectCall, InterProcIRCursor, IntraProcIRCursor, Jump, Procedure, Program}
import util.Logger

import scala.collection.immutable.Map
import scala.collection.mutable

/**
 * (A variant of) the IDE analysis algorithm.
 * Adapted from Tip
 * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/IDESolver.scala
 */
abstract class IRIDESolver[E <: (Procedure | Command), EE <: (Procedure | Command), C <: (DirectCall | Block), R <: (DirectCall | Block), D, T, L <: Lattice[T]](val program: Program, val cache: IRIDECache, val startNode: CFGPosition)
  extends IRIDEAnalysis[E, EE, C, R, D, T, L], Dependencies[CFGPosition] {

  protected def entryToExit(entry: E) : EE
  protected def exitToEntry(exit: EE) : E
  protected def callToReturn(call: C) : R
  protected def returnToCall(ret: R) : C
  protected def getCallee(call: C) : E
  protected def isCall(call: CFGPosition) : Boolean
  protected def isExit(exit: CFGPosition) : Boolean
  protected def getAfterCalls(exit: EE) : Set[R]
  protected def getExitProc(exit: EE) : Procedure
  protected def getEntryProc(entry: E) : Procedure

  /**
   * Phase 1 of the IDE algorithm.
   * Computes Path functions and Summary functions
   * The original version of the algorithm uses summary edges from call nodes to after-call nodes
   * instead of `callJumpCache` and `exitJumpCache`.
   */
  class Phase1(val program: Program) extends WorklistFixPointFunctions[(CFGPosition, DL, DL), EdgeFunction[T], EdgeFunctionLattice[T, valuelattice.type]] {

    val lattice: MapLattice[(CFGPosition, DL, DL), EdgeFunction[T], EdgeFunctionLattice[T, valuelattice.type]] = new MapLattice(edgelattice)
    var x: Map[(CFGPosition, DL, DL), EdgeFunction[T]] = _
    val first: Set[(CFGPosition, DL, DL)] = Set((startNode, Right(Lambda()), Right(Lambda())))

    /**
     * callJumpCache(funentry, d1, call)(d3) returns the composition of the edges (call.funentry, d3) -> (call, *) -> (funentry, d1).
     * Allows faster lookup than scanning through the current lattice element.
     */
     private val callJumpCache = mutable.Map[(E, DL, C), mutable.Map[DL, EdgeFunction[T]]]()

    /**
     * exitJumpCache(funentry, d1) contains d2 if there is a non-bottom edge (funentry, d1) -> (funentry.exit, d2).
     * Allows faster lookup than scanning through the current lattice element.
     */
    private val exitJumpCache = mutable.Map[(E, DL), mutable.Set[DL]]()

    private def storeCallJump(funentry: E, d1: DL, call: C, e: EdgeFunction[T], d3: DL): Unit = {
      val m = callJumpCache.getOrElseUpdate((funentry, d1, call), mutable.Map[DL, EdgeFunction[T]]())
      m += d3 -> m.getOrElse(d3, edgelattice.bottom).joinWith(e)
    }

    private def storeExitJump(funentry: E, d1: DL, d2: DL): Unit =
      exitJumpCache.getOrElseUpdate((funentry, d1), mutable.Set[DL]()) += d2


    import edgelattice.{IdEdge}

    def init: EdgeFunction[T] = IdEdge()

    private def returnflow(d1: DL, d2: DL, funexit: EE, aftercall: R): Unit = {
      val entry = exitToEntry(funexit)
      val call = returnToCall(aftercall)
      callJumpCache.getOrElseUpdate((entry, d1, call), mutable.Map[DL, EdgeFunction[T]]()).foreach {
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


    def process(n: (CFGPosition, DL, DL)): Unit = {
      n match {
        case (position, d1, d2) =>
          val e1 = x(n)
          position match
            case call: C if isCall(call) => // at a call node
              val entry: E = getCallee(call)
              val ret: R = callToReturn(call)

              edgesCallToEntry(call, entry)(d2).foreach {
                case (d3, e2) =>
                  // propagate to function entry
                  propagate(IdEdge(), (entry, d3, d3))
                  // cache the composed edge from the entry of the caller to the entry of the callee
                  storeCallJump(entry, d3, call, e2.composeWith(e1), d1)
                  // propagate existing return flow to the after-call node
                  exitJumpCache.getOrElseUpdate((entry, d3), mutable.Set[DL]()).foreach { d4 =>
                    returnflow(d3, d4, entryToExit(entry), ret)
                  }
              }

              edgesCallToAfterCall(call, ret)(d2).foreach {
                case (d3, e2) =>
                  propagate(e2.composeWith(e1), (ret, d1, d3))
              }
            case exit: EE if isExit(exit) => // at an exit node
              getAfterCalls(exit).foreach(afterCall => returnflow(d1, d2, exit, afterCall))
              storeExitJump(exitToEntry(exit), d1, d2)

            case _ =>
              edgesOther(position)(d2).foreach {
                case (d3, e2) =>
                  val e3 = e2.composeWith(e1)
                  outdep(position).foreach { m =>
                    propagate(e3, (m, d1, d3))
                  }
              }
      }
    }

    def summaries(): mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]] = {
      val res = mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]]()
      x.foreach {
        case ((n, d1, d2), e) =>
          n match {
            case exit: EE if isExit(exit) =>
              val proc = getExitProc(exit)
              val m1 = res.getOrElseUpdate(proc, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]().withDefaultValue(mutable.Map[DL, EdgeFunction[T]]()))
              val m2 = m1.getOrElseUpdate(d1, mutable.Map[DL, EdgeFunction[T]]())
              m2 += d2 -> e
            case _ => // ignore other node kinds
          }
      }
      Logger.info(s"Function summaries:\n${
        res.map {
          case (f, s) => s"  function $f:\n${s.map { case (d1, m) => s"${m.map { case (d2, e) => s"    ($d1,$d2): $e" }.mkString("\n")}" }.mkString("\n")}"
        }.mkString("\n")
      } ")
      res
    }
  }

  /**
   * Phase 2 of the IDE algorithm.
   * Performs a forward dataflow analysis using the decomposed lattice and the micro-transformers.
   * The original RHS version of IDE uses jump functions for all nodes, not only at exits, but the analysis result and complexity is the same.
   */
  class Phase2(val program: Program , val phase1: Phase1) extends WorklistFixPointFunctions[(CFGPosition, DL), T, valuelattice.type]:
    val lattice: MapLattice[(CFGPosition, DL), T, valuelattice.type] = new MapLattice(valuelattice)
    var x: Map[(CFGPosition, DL), T] = _
    val first: Set[(CFGPosition, DL)] = Set((startNode, Right(Lambda())))

    /**
     * Function summaries from phase 1.
     * Built when first invoked.
     */
    lazy val summaries: mutable.Map[Procedure, mutable.Map[DL, mutable.Map[DL, EdgeFunction[T]]]] = phase1.summaries()

    def init: T = lattice.sublattice.top

    def process(n: (CFGPosition, DL)): Unit = {
      val xnd = x(n)
      n match {
        case (position, d) =>
          position match {
            // function call nodes
            case call: C if isCall(call) => // at a call node
              val entry: E = getCallee(call)
              val ret: R = callToReturn(call)

              edgesCallToEntry(call, entry)(d).foreach {
                case (d2, e) =>
                  propagate(e(xnd), (entry, d2))
                  summaries(getEntryProc(entry))(d2).foreach {
                    case (d3, e2) =>
                      edgesExitToAfterCall(entryToExit(entry), ret)(d3).foreach {
                        case (d4, e3) =>
                          propagate(e3(e2(e(xnd))), (ret, d4))
                      }
                  }
              }

              edgesCallToAfterCall(call, ret)(d).foreach {
                case (d2, e) =>
                  propagate(e(xnd), (ret, d2))
              }

            // Function exit
            case exit: EE if isExit(exit) => // ignore, return flow is handled at the call nodes

            case _ =>
              edgesOther(position)(d).foreach {
                case (d2, e) =>
                  outdep(position).foreach(m => propagate(e(xnd), (m, d2)))
              }
          }
      }
    }

    val restructuredlattice: MapLattice[CFGPosition, Map[D, T], MapLattice[D, T, valuelattice.type]] = new MapLattice(new MapLattice(valuelattice))

    /**
     * Restructures the analysis output to match `restructuredlattice`.
     */
    def restructure(y: lattice.Element): restructuredlattice.Element =
      y.foldLeft(Map[CFGPosition, Map[D, valuelattice.Element]]()) {
        case (acc, ((n, dl), e)) =>
          dl match {
            case Left(d) => acc + (n -> (acc.getOrElse(n, Map[D, valuelattice.Element]()) + (d -> e)))
            case _ => acc
          }
      }

  def analyze(): Map[CFGPosition, Map[D, valuelattice.Element]] = {
    val phase1 = new Phase1(program)
    phase1.analyze()
    val phase2 = new Phase2(program, phase1)
    phase2.restructure(phase2.analyze())
  }
}


abstract class ForwardIDESolver[D, T, L <: Lattice[T]](program: Program, cache: IRIDECache)
  extends IRIDESolver[Procedure, Command, DirectCall, Block, D, T, L](program, cache, program.mainProcedure),
    ForwardIDEAnalysis[D, T, L], IRInterproceduralForwardDependencies {

  protected def entryToExit(entry: Procedure): Command = cache.entryExitMap(entry)

  protected def exitToEntry(exit: Command): Procedure = cache.entryExitMap(exit)

  protected def callToReturn(call: DirectCall): Block = cache.callReturnMap(call)

  protected def returnToCall(ret: Block): DirectCall = cache.callReturnMap(ret)

  protected def getCallee(call: DirectCall): Procedure = cache.callees(call)

  protected def isCall(call: CFGPosition): Boolean =
    call match
      case directCall: DirectCall => cache.callReturnMap.forwardMap.contains(directCall)
      case _ => false

  protected def isExit(exit: CFGPosition): Boolean =
    exit match
      case command: Command => cache.afterCall.contains(command)
      case _ => false

  protected def getAfterCalls(exit: Command): Set[Block] = cache.afterCall(exit)

  protected def getExitProc(exit: Command): Procedure = exit.parent.parent

  protected def getEntryProc(entry: Procedure): Procedure = entry
}


abstract class BackwardIDESolver[D, T, L <: Lattice[T]](program: Program, cache: IRIDECache)
  extends IRIDESolver[Command, Procedure, Block, DirectCall, D, T, L](program, cache, cache.entryExitMap(program.mainProcedure)),
    BackwardIDEAnalysis[D, T, L], IRInterproceduralBackwardDependencies {

  protected def entryToExit(entry: Command): Procedure = cache.entryExitMap(entry)

  protected def exitToEntry(exit: Procedure): Command = cache.entryExitMap(exit)

  protected def callToReturn(call: Block): DirectCall = cache.callReturnMap(call)

  protected def returnToCall(ret: DirectCall): Block = cache.callReturnMap(ret)

  protected def getCallee(call: Block): Command = cache.retExit(call)

  protected def isCall(call: CFGPosition): Boolean =
    call match
      case block: Block => cache.callReturnMap.backwardMap.contains(block)
      case _ => false

  protected def isExit(exit: CFGPosition): Boolean =
    exit match
      case procedure: Procedure => cache.callers.contains(procedure)
      case _ => false

  protected def getAfterCalls(exit: Procedure): Set[DirectCall] = cache.callers(exit)

  protected def getExitProc(exit: Procedure): Procedure = exit

  protected def getEntryProc(entry: Command): Procedure = entry.parent.parent
}
