package ir.transforms

import ir.*
import util.Logger
import util.assertion.*

import scala.collection.mutable

/**********************************************************************************
 * Block-level abstract interpreter framework.
 *********************************************************************************/

/** The abstract domain used for an analysis.
  *
  * @tparam Location:
  *   a code location (e.g. [[Block]])
  * @tparam Code
  *   : A program command transfer is defined for (e.g. [[Command]])
  * @tparam L
  *   : The lattice value type for the analysis
  */
trait GenericAbstractDomain[Location, Code, L] {
  def join(a: L, b: L, pos: Location): L
  def widen(a: L, b: L, pos: Location): L = join(a, b, pos) /* not used */
  def narrow(a: L, b: L): L = a
  def transfer(a: L, b: Code): L
  def init(b: Location): L = bot

  def isFixed(prev: L, next: L): Boolean = (prev == next)

  def top: L
  def bot: L
}

/** An abstract domain for a block-level analysis. Use with [[worklistSolver]].
  */
trait AbstractDomain[L] extends GenericAbstractDomain[Block, Command, L] {
  def transferBlockFwd(a: L, b: Block): L = {
    transfer(b.statements.foldLeft(a)(transfer), b.jump)
  }
  def transferBlockBwd(a: L, b: Block): L = {
    b.statements.toList.reverse.foldLeft(transfer(a, b.jump))(transfer)
  }
}

trait PowerSetDomain[T] extends AbstractDomain[Set[T]] {
  def bot = Set()
  def top = ???
  def join(a: Set[T], b: Set[T], pos: Block) = a.union(b)
}

/** A solver which solve for a single global lattice value across a procedrure. This traverses blocks in
  * reverse-post-order.
  *
  * This expects blocks [[rpoOrder()]] to have been run on the blocks in the procedure.
  *
  * E.g. used for flow-insensitive analyses.
  *
  * @tparam L
  *   : the lattice value type
  * @tparam D
  *   : The analysis abstract domain defining operations over L
  *
  * @param initial:
  *   the initial set of blocks to add to the worklist
  * @param domain:
  *   the instance of D providing an implementation of the abstract domain
  */
def onePassForwardGlobalStateSolver[L, D <: AbstractDomain[L]](initial: Iterable[Block], domain: D) = {
  val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
  worklist.addAll(initial)
  var state = domain.bot

  while (worklist.nonEmpty) {
    val b: Block = worklist.dequeue
    val p = state

    for (l <- b.statements) {
      state = domain.transfer(state, l)
    }
    state = domain.transfer(state, b.jump)
  }
}

trait ProcAbstractDomain[L] extends GenericAbstractDomain[Procedure, Procedure, L]

class DomainWithFunctionSummaries[L, Summary](
  d: AbstractDomain[L],
  procSummary: Procedure => Summary,
  transferCallSummary: (L, Summary, DirectCall) => L
) extends AbstractDomain[L] {
  def join(a: L, b: L, pos: Block): L = d.join(a, b, pos)
  override def widen(a: L, b: L, pos: Block): L = d.widen(a, b, pos)
  override def narrow(a: L, b: L): L = d.narrow(a, b)
  override def init(b: Block): L = d.init(b)
  override def isFixed(prev: L, next: L): Boolean = d.isFixed(prev, next)
  def top: L = d.top
  def bot: L = d.bot

  def transfer(a: L, b: Command): L = b match {
    case call: DirectCall => {
      transferCallSummary(a, procSummary(call.target), call)
    }
    case o => d.transfer(a, b)
  }
}

trait ProcedureSummaryGenerator[L, LocalDomain] extends ProcAbstractDomain[L] {

  /** 
   *  Join the summary [[summaryForTarget]] for a call [[p]] into the local abstract state [[l]]
   */
  def localTransferCall(l: LocalDomain, summaryForTarget: L, p: DirectCall): LocalDomain

  /**
   * Return the new updated summary for a procedure based on the results of a dataflow analysis of that procedure.
   */
  def updateSummary(
    prevSummary: L,
    p: Procedure,
    resBefore: Map[Block, LocalDomain],
    resAfter: Map[Block, LocalDomain]
  ): L
}

/** Intraprocedural worklist solver.
  *
  * Traverses blocks in reverse-post-order so requires [[rpoOrder()]] to have been run on the procedure.
  *
  * @tparam L:
  *   the lattice value type
  * @tparam A:
  *   The abstract domain defining the analysis using lattice value [[L]]
  * @param domain:
  *   The abstract domain implementing the analysis
  */
class worklistSolver[L, A <: AbstractDomain[L]](domain: A) {

  /** Perform the analysis on a procedure and return the resulting lattice values for each block.
    *
    * @param p
    *   the procedure to analyse
    * @param backwards
    *   should the analysis be run backwards
    * @returns
    *   (block -> lattice value at start of block, block -> lattice value at end of block)
    */
  def solveProc(p: Procedure, backwards: Boolean = false): (Map[Block, L], Map[Block, L]) = {
    solve(p.blocks, backwards)
  }

  /** Apply the [[solveProc]] to every procedure in the program and flatten the result into one map.
    *
    * @param p
    *   the procedure to analyse
    * @returns
    *   (block -> lattice value at start of block, block -> lattice value at end of block)
    */
  def solveProgIntraProc(p: Program, backwards: Boolean = false): (Map[Block, L], Map[Block, L]) = {
    def foldfun(acc: (Map[Block, L], Map[Block, L]), l: (Map[Block, L], Map[Block, L])) = {
      val (accl, accr) = acc
      val (vl, vr) = l
      ((accl ++ vl), (accr ++ vr))
    }
    p.procedures.map(p => solve(p.blocks, backwards)).foldLeft(Map[Block, L](), Map[Block, L]())(foldfun)
  }

  def solve(initial: IterableOnce[Block], backwards: Boolean = false): (Map[Block, L], Map[Block, L]) = {
    val savedAfter: mutable.HashMap[Block, L] = mutable.HashMap()
    val savedBefore: mutable.HashMap[Block, L] = mutable.HashMap()
    val saveCount: mutable.HashMap[Block, Int] = mutable.HashMap()
    val worklist = {
      if (backwards) {
        mutable.PriorityQueue[Block]()(Ordering.by(b => -b.rpoOrder))
      } else {
        mutable.PriorityQueue[Block]()(Ordering.by(b => b.rpoOrder))
      }
    }
    worklist.addAll(initial)

    def successors(b: Block) = if backwards then b.prevBlocks else b.nextBlocks
    def predecessors(b: Block) = if backwards then b.nextBlocks else b.prevBlocks

    while (worklist.nonEmpty) {
      val b = worklist.dequeue

      while (
          worklist.nonEmpty && (if backwards then (worklist.head.rpoOrder <= b.rpoOrder)
                                else (worklist.head.rpoOrder >= b.rpoOrder))
        )
      do {
        // drop rest of blocks with same priority
        val m = worklist.dequeue()
        debugAssert(
          m == b,
          s"Different nodes with same priority ${m.rpoOrder} ${b.rpoOrder}, violates PriorityQueueWorklist assumption: $b and $m"
        )
      }

      val prev = savedAfter.get(b)
      val x = {
        predecessors(b).toList.flatMap(b => savedAfter.get(b).toList) match {
          case Nil => domain.init(b)
          case h :: Nil => h
          case h :: tl => tl.foldLeft(h)((acc, nb) => domain.join(acc, nb, b))
        }
      }
      savedBefore(b) = x
      val todo = List(b)

      val lastBlock = b // todo.last
      var nx = todo.foldLeft(x)((x, b) => {
        savedBefore(b) = x
        if (backwards) {
          val ojmp = domain.transfer(x, b.jump)
          savedAfter(b) = b.statements.toList.reverse.foldLeft(ojmp)(domain.transfer)
        } else {
          val stmts = b.statements.foldLeft(x)(domain.transfer)
          savedAfter(b) = domain.transfer(stmts, b.jump)
        }
        savedAfter(b)
      })
      savedAfter(lastBlock) = nx
      saveCount(lastBlock) = saveCount.get(lastBlock).getOrElse(0) + 1
      if (!prev.contains(nx)) then {
        if (saveCount(lastBlock) >= 1000) {
          Logger.warn(s"Large join count on block ${lastBlock.label}, no fix point? (-v for more info)")
          Logger.debug(lastBlock.label + "    ==> " + x)
          Logger.debug(lastBlock.label + "    <== " + nx)
        }
        worklist.addAll(successors(lastBlock))
      }
    }
    if backwards then (savedAfter.toMap, savedBefore.toMap) else (savedBefore.toMap, savedAfter.toMap)
  }
}

/** Perform an interprocedural analysis by running an intraprocedural analysis on each procedure, computing a summary
  * based on the result, and computing the fixed point of summaries over the call graph.
  *
  * Recommended to call [[Procedure.sortProceduresRPO]] before using this so that we traverse the call graph in
  * reverse-post-order.
  *
  * @tparam SummaryAbsVal
  *   The lattice value type representing summaries of a procedure
  * @tparam LocalAbsVal
  *   The lattice value type for the intraprocedural analysis of each block
  * @tparam A
  *   The abstract domain type implemmenting the intraprocedural analysis of each procedure
  * @param localDomain
  *   The abstract domain implementing the intraprocedural analysis of procedures
  * @param sg
  *   The ProcedureSummaryGenerator defining the analysis of a procedure using summaries.
  *
  * This internally creates a new abstract domain that overrides transfer of [[DirectCall]] in [[localDomain]] with the
  * transfer function defined in [[sg]] which utilises procedure summaries.
  */
class interprocSummaryFixpointSolver[SummaryAbsVal, LocalAbsVal, A <: AbstractDomain[LocalAbsVal]](
  localDomain: A,
  sg: ProcedureSummaryGenerator[SummaryAbsVal, LocalAbsVal]
) {

  def transferProcedure(
    a: SummaryAbsVal,
    b: Procedure,
    getSummary: Procedure => SummaryAbsVal,
    backwards: Boolean
  ): SummaryAbsVal = {
    val domain = DomainWithFunctionSummaries(localDomain, getSummary, sg.localTransferCall)
    val solver = worklistSolver(domain)
    val (beforeRes, afterRes) = solver.solveProc(b, backwards)
    sg.updateSummary(a, b, beforeRes, afterRes)
  }

  def solveProcsInterProc(procedures: Iterable[Procedure], backwards: Boolean = false) = {
    var old_summaries = Map[Procedure, SummaryAbsVal]()
    var summaries = Map[Procedure, SummaryAbsVal]()
    var first = true
    while (first || summaries != old_summaries) {
      first = false
      old_summaries = summaries

      for (p <- procedures) {
        def getSummary(p: Procedure) = old_summaries.get(p).getOrElse(sg.init(p))
        val r = transferProcedure(getSummary(p), p, getSummary, backwards)
        summaries = summaries.updated(p, r)
      }
    }
    summaries
  }

  def solveProgInterProc(p: Program, backwards: Boolean = false) = {
    solveProcsInterProc(p.procedures, backwards)
  }
}

/** Worklist solver which performs a fixed point over the call graph.
  *
  * @tparam L
  *   the lattice value type
  * @param transferProcedure
  *   defines the analysis of a single procedure (akin to a transfer function)
  * @param init
  *   function defining the initial lattice value for a procedure
  */
class BottomUpCallgraphWorklistSolver[L](transferProcedure: (Procedure => L, L, Procedure) => L, init: Procedure => L) {

  def solve(p: Program): Map[Procedure, L] = {
    var old_summaries = Map[Procedure, L]()
    var summaries = Map[Procedure, L]()

    p.sortProceduresRPO()
    val indexed = p.procedures.zipWithIndex
    val indexMap = indexed.toMap
    val worklist = mutable.PriorityQueue[(Procedure, Int)]()(Ordering.by(-_._2))
    worklist.addAll(indexed)

    while (worklist.nonEmpty) {
      val (p, _) = worklist.dequeue
      old_summaries = summaries

      def getSummary(p: Procedure) = old_summaries.get(p).getOrElse(init(p))
      val s = getSummary(p)
      val r = transferProcedure(getSummary, s, p)
      if (r != s) {
        summaries = summaries.updated(p, r)
        worklist.addAll(p.incomingCalls().map(p => (p.target, indexMap(p.target))))
        worklist.addAll(p.calls.map(p => (p, indexMap(p))))
      }
    }
    summaries
  }
}

class SCCCallgraphWorklistSolver[L](transferProcedure: (Procedure => L, L, Procedure) => L, init: Procedure => L) {

  def solve(p: Program) = {
    var old_summaries = Map[Procedure, L]()
    var summaries = Map[Procedure, L]()

    val scc = stronglyConnectedComponents(CallGraph, List(p.mainProcedure))

    for component <- scc do {
      val worklist = mutable.LinkedHashSet[Procedure]()

      worklist.addAll(component)

      while (worklist.nonEmpty) {
        val p = worklist.head
        worklist.remove(p)
        old_summaries = summaries

        def getSummary(p: Procedure) = old_summaries.get(p).getOrElse(init(p))
        val s = getSummary(p)
        val r = transferProcedure(getSummary, s, p)
        if (r != s) {
          summaries = summaries.updated(p, r)
          worklist.addAll(p.incomingCalls().map(_.target).filter(component.contains(_)))
          worklist.addAll(p.calls.filter(component.contains(_)))
        }
      }
    }
    summaries
  }
}
