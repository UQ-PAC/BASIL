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


trait AbstractDomain[L] {
  def join(a: L, b: L, pos: Block): L
  def widen(a: L, b: L, pos: Block): L = join(a, b, pos) /* not used */
  def narrow(a: L, b: L): L = a
  def transfer(a: L, b: Command): L
  def init(b: Block): L = bot

  def isFixed(prev: L, next: L) : Boolean = (prev == next)

  def transferBlockFwd(a: L, b: Block): L = {
    transfer(b.statements.foldLeft(a)(transfer), b.jump)
  }
  def transferBlockBwd(a: L, b: Block): L = {
    b.statements.toList.reverse.foldLeft(transfer(a, b.jump))(transfer)
  }

  def top: L
  def bot: L
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

class worklistSolver[L, A <: AbstractDomain[L]](domain: A) {

  def solveProc(p: Procedure, backwards: Boolean = false) = {
    solve(p.blocks, Set(), Set(), backwards)
  }

  def solveProg(
      p: Program,
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): Map[Procedure, Map[Block, L]] = {
    val initDom = p.procedures.map(p => (p, p.blocks))

    val work = initDom.map(d => {
      (
        d._1,
        Future {
          val t = util.PerformanceTimer(s"solve ${d._1.name}")
          Logger.info(s"begin ${t.timerName}")
          val r = solve(d._2, Set(), Set())
          t.checkPoint("finished")
          r
        }
      )
    })
    work
      .map((prog, x) =>
        try {
          (prog, Await.result(x, 10000.millis)._2)
        } catch {
          case t: Exception => {
            Logger.error(s"${prog.name} : $t")
            (prog, Map())
          }
        }
      )
      .toMap
    // Await.result(Future.sequence(work), Duration.Inf).toMap
  }

  def solve(
      initial: IterableOnce[Block],
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block], // set of conditions
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
