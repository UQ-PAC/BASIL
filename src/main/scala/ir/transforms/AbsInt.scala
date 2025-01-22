package ir.transforms
import translating.serialiseIL

import util.Logger
import ir.eval.AlgebraicSimplifications
import ir.eval.AssumeConditionSimplifications
import ir.eval.simplifyExprFixpoint
import ir.cilvisitor.*
import ir.*
import scala.collection.mutable
import analysis._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global



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

class DomainWithFunctionSummaries[L, Summary](d: AbstractDomain[L], 
  procSummary: Procedure => Summary,
  transferCallSummary: (L, Summary, DirectCall) => L
  ) extends AbstractDomain[L]  {
    def join(a: L, b: L, pos: Block): L = d.join(a,b,pos)
    override def widen(a: L, b: L, pos: Block): L = d.widen(a,b, pos)
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
  def localTransferCall(l: LocalDomain, summaryForTarget: L, p: DirectCall) : LocalDomain
  def updateSummary(prevSummary: L, p: Procedure,  resBefore: Map[Block, LocalDomain], resAfter: Map[Block, LocalDomain]) : L
}



class worklistSolver[L, A <: AbstractDomain[L]](domain: A) {

  def solveProc(p: Procedure, backwards: Boolean = false) : (Map[Block, L], Map[Block, L]) = {
    solve(p.blocks, backwards)
  }

  def solveProgIntraProc(p: Program, backwards: Boolean = false): (Map[Block, L], Map[Block, L]) = {
    def foldfun(acc: (Map[Block, L], Map[Block, L]), l: (Map[Block, L], Map[Block, L])) = {
        val (accl, accr) = acc
        val (vl, vr) = l
        ((accl ++ vl), (accr ++ vr))
    }
    p.procedures.map(p => solve(p.blocks, backwards)).foldLeft(Map[Block, L](), Map[Block, L]())(foldfun)
  }

  def solve(
    initial: IterableOnce[Block],
    backwards: Boolean = false
  ): (Map[Block, L], Map[Block, L]) = {
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
      ) do {
        // drop rest of blocks with same priority
        val m = worklist.dequeue()
        assert(
          m == b,
          s"Different nodes with same priority ${m.rpoOrder} ${b.rpoOrder}, violates PriorityQueueWorklist assumption: $b and $m"
        )
      }

      val prev = savedAfter.get(b)
      val x = {
        predecessors(b).toList.flatMap(b => savedAfter.get(b).toList) match {
          case Nil      => domain.init(b)
          case h :: Nil => h
          case h :: tl  => tl.foldLeft(h)((acc, nb) => domain.join(acc, nb, b))
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
        if (saveCount(lastBlock) >= 50) {
          Logger.warn(s"Large join count on block ${lastBlock.label}, no fix point? (-v for mor info)")
          Logger.debug(lastBlock.label + "    ==> " + x)
          Logger.debug(lastBlock.label + "    <== " + nx)
        }
        worklist.addAll(successors(lastBlock))
      }
    }
    if backwards then (savedAfter.toMap, savedBefore.toMap) else (savedBefore.toMap, savedAfter.toMap)
  }
}



class interprocSummaryFixpointSolver[SummaryAbsVal, LocalAbsVal, 
  A <: AbstractDomain[LocalAbsVal]](localDomain : A, 
    sg: ProcedureSummaryGenerator[SummaryAbsVal, LocalAbsVal]) {

  def transferProcedure(a: SummaryAbsVal, b: Procedure, getSummary: Procedure => SummaryAbsVal, backwards: Boolean): SummaryAbsVal = {
    val domain = DomainWithFunctionSummaries(localDomain, getSummary, sg.localTransferCall)
    val solver = worklistSolver(domain)
    val (beforeRes, afterRes) = solver.solveProc(b, backwards)
    sg.updateSummary(a, b, beforeRes, afterRes)
  }

  def solveProgInterProc(p: Program, backwards : Boolean = false) = {
    var old_summaries = Map[Procedure, SummaryAbsVal]()
    var summaries = Map[Procedure, SummaryAbsVal]()
    var first = true
    while (first || summaries != old_summaries) {
      first = false
      old_summaries = summaries

      for (p <- p.procedures) {
        def getSummary(p: Procedure) = old_summaries.get(p).getOrElse(sg.init(p))
        val r = transferProcedure(getSummary(p), p, getSummary, backwards)
        summaries = summaries.updated(p, r)
      }
    }
    summaries
  }
}
