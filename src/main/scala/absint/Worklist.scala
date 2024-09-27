package absint
import ir.*
import util.PerformanceTimer
import analysis.*
import scala.collection.mutable
import scala.concurrent.{Future, blocking, ExecutionContext, Await}
import scala.concurrent.duration.Duration
given ExecutionContext = ExecutionContext.global

def reversePostOrder(startBlock: Block): Unit = {
  var count = 0
  val seen = mutable.HashSet[Block]()
  val vcs = mutable.HashMap[Block, Int]()

  def walk(b: Block): Unit = {
    seen += b
    for (s <- b.nextBlocks) {
      if (!seen.contains(s)) {
        walk(s)
      }
    }
    b.rpoOrder = count
    count += 1
  }

  walk(startBlock)
}

def applyRPO(p: Program) = {
  for (proc <- p.procedures) {
    proc.entryBlock.map(eb => reversePostOrder(eb))
  }
}



class worklistSolver[L, A <: AbstractDomain[L]](domain: A) {



  def parSolveProg(
      p: Program,
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): Map[Procedure, L] = {
    val initDom = p.procedures.map(p =>
      (p, p.blocks.filter(x => (x.nextBlocks.size > 1 || x.prevBlocks.size > 1) && x.rpoOrder != -1))
    )

    Await.result(Future.sequence(initDom.map(d => Future {
      blocking {
        (d._1, solve(d._2, Set(), Set(), Some(s"procedure : ${d._1.name} (size ${(d._1.blocks.map(_.statements.size)).foldLeft(0)((a,b) => a + b)})")))
      }
    })), Duration.Inf).toMap
  }

  def solveProg(
      p: Program,
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): Map[Procedure, L] = {
    val initDom = p.procedures.map(p =>
      (p, p.blocks.filter(x => (x.nextBlocks.size > 1 || x.prevBlocks.size > 1) && x.rpoOrder != -1))
    )

    initDom.map(d => (d._1, solve(d._2, Set(), Set()))).toMap
  }

  def solve(
      initial: IterableOnce[Block],
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block], // set of conditions
      timerMsg: Option[String] = None
  ): L = {
    val timer = timerMsg.map(PerformanceTimer(_))
    val saved: mutable.HashMap[Block, L] = mutable.HashMap()
    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(b => b.rpoOrder))
    worklist.addAll(initial)

    var x = domain.bot
    while (worklist.nonEmpty) {
      val b = worklist.dequeue

      while (worklist.nonEmpty && (worklist.head.rpoOrder >= b.rpoOrder)) do {
        // drop rest of blocks with same priority
        val m = worklist.dequeue()
        assert(
          m == b,
          s"Different nodes with same priority ${m.rpoOrder} ${b.rpoOrder}, violates PriorityQueueWorklist assumption: $b and $m"
        )
      }

      def bs(b: Block): List[Block] = {
        if (b.nextBlocks.size == 1) {
          val n = b.nextBlocks.head
          b :: bs(n)
        } else {
          List(b)
        }
      }

      x = b.prevBlocks.flatMap(ib => saved.get(ib).toList).foldLeft(x)(domain.join)
      val todo = bs(b)
      val lastBlock = todo.last

      def xf_block(x: L, b: Block) = b.statements.foldLeft(x)(domain.transfer)
      var nx = todo.foldLeft(x)(xf_block)
      saved(lastBlock) = nx
      if (nx != x) then {
        worklist.addAll(lastBlock.nextBlocks)
      }
      x = nx
    }
    timer.foreach(_.checkPoint("finished solve"))
    x
  }
}
