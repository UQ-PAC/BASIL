package ir.transforms

import util.Logger
import ir.cilvisitor.*
import ir.*
import scala.collection.mutable
import analysis._

trait AbstractDomain[L] {
  def join(a: L, b: L): L
  def widen(a: L, b: L): L = join(a, b)
  def narrow(a: L, b: L): L = a
  def transfer(a: L, b: Statement): L

  def top: L
  def bot: L
}

def saturateVariableDependencies(procedure: Procedure): Map[Variable, Set[Variable]] = {
  // assuming dsa single-assigned variables are those which do not depend on themselves; variables which do not depend on themselves
  //    x1 := y1 + z1
  //    x2 := x1
  //    z2 := 21
  // x1 -> {y1, z1}
  // x2 -> {x1, y1, z1,x2}
  var deps = Map[Variable, Set[Variable]]().withDefaultValue(Set())

  // add base facts
  for (s <- procedure) {
    s match {
      case Assign(x, y, _) => {
        deps = deps.updated(x, deps(x) ++ y.variables)
      }
      case d: DirectCall => {
        val rhs = d.actualParams.toSet.flatMap(_._2.variables)
        for (o <- d.outParams.map(_._2)) {
          deps = deps.updated(o, deps(o) ++ rhs)
        }
      }
      case _ => {}
    }
  }

  // saturate
  var ndeps = deps
  while ({
    deps = ndeps
    for (fact <- deps) {
      val (lhs, rhs) = fact
      for (v <- rhs) {
        ndeps = ndeps.updated(lhs, ndeps(lhs) ++ ndeps(v))
      }
    }
    ndeps != deps
    }) {;}

  ndeps
}

def cyclicVariables(p: Procedure) : Set[Variable] = {
  val deps = saturateVariableDependencies(p)
  deps.filter(v => v._2.contains(v._1)).map(_._1).toSet
}

def getRedundantAssignments(procedure: Procedure): Set[Assign] = {

  /** Get all assign statements which define a variable never used, assuming ssa form and proc parameters
   *  so that interprocedural check is not required.
    */

  enum VS:
    case Bot
    case Assigned(definition: Set[Assign])
    case Read

  def joinVS(a: VS, b: VS) = {
    (a, b) match {
      case (VS.Bot, o)                        => o
      case (o, VS.Bot)                        => o
      case (VS.Read, _)                       => VS.Read
      case (_, VS.Read)                       => VS.Read
      case (VS.Assigned(d1), VS.Assigned(d2)) => VS.Assigned(d1 ++ d2)
    }
  }

  val assignedNotRead = mutable.Map[Variable, VS]().withDefaultValue(VS.Bot)

  for (c <- procedure) {
    c match {
      case a: Assign => {
        assignedNotRead(a.lhs) = joinVS(assignedNotRead(a.lhs), VS.Assigned(Set(a)))
        a.rhs.variables.foreach(v => {
          assignedNotRead(v) = VS.Read
        })
      }
      case m: MemoryAssign => {
        m.index.variables.foreach(v => {
          assignedNotRead(v) = VS.Read
        })
        m.value.variables.foreach(v => {
          assignedNotRead(v) = VS.Read
        })
      }
      case m: IndirectCall => {
        assignedNotRead(m.target) = VS.Read
      }
      case m: Assert => {
        m.body.variables.foreach(v => {
          assignedNotRead(v) = VS.Read
        })
      }
      case m: Assume => {
        for (v <- m.body.variables) {
          assignedNotRead(v) = VS.Read
        }
      }
      case c: DirectCall => {
        c.actualParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = VS.Read
          })

      }
      case p: Return => {
        p.outParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = VS.Read
          })
      }
      case p: GoTo        => ()
      case p: NOP         => ()
      case p: Unreachable => ()
      case p: Procedure   => ()
      case b: Block       => ()
    }
  }

  val r = assignedNotRead
    .collect { case (v, VS.Assigned(d)) =>
      d
    }
    .toSet
    .flatten
  r
}

class CleanupAssignments() extends CILVisitor {
  var redundantAssignments = Set[Assign]()

  def isRedundant(a: Assign) = {
    redundantAssignments.contains(a)
  }

  override def vproc(p: Procedure) = {
    redundantAssignments = getRedundantAssignments(p)
    DoChildren()
  }

  override def vstmt(s: Statement) = s match {
    case a: Assign if isRedundant(a) => ChangeTo(List())
    case _                           => SkipChildren()
  }

}

def doCopyPropTransform(p: Program) = {
  var runs = 0
  var rerun = true
  applyRPO(p)
  while (runs < 5) {
    val nonConstVars = p.procedures.map(p => p -> cyclicVariables(p)).toMap
    val d = ConstCopyProp(nonConstVars)

    runs += 1
    val solver = worklistSolver(d)
    val result = solver.solveProg(p, Set(), Set())

    while (rerun) {
      rerun = false
      for ((p, xf) <- result) {
        val vis = Simplify(xf.withDefaultValue(CCP(Map(), Map()))) 
        visit_proc(vis, p)
        rerun = rerun || vis.madeAnyChange
      }
    }
  }

  // cleanup
  visit_prog(CleanupAssignments(), p)
  val toremove = p.collect {
    case b: Block if b.statements.size == 0 && b.prevBlocks.size == 1 && b.nextBlocks.size == 1 => b
  }
  for (b <- toremove) {
    val p = b.prevBlocks.head
    val n = b.nextBlocks.head
    p.replaceJump((GoTo(n)))
    b.parent.removeBlocks(b)
  }

}

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

  def solveProg(
      p: Program,
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): Map[Procedure, Map[Block, L]] = {
    val initDom = p.procedures.map(p =>
      (p, p.blocks)
    )

    initDom.map(d => (d._1, solve(d._2, Set(), Set()))).toMap
  }

  def solve(
      initial: IterableOnce[Block],
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ):  Map[Block, L] = {
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
      saved(b) = x
      val todo = bs(b)
      val lastBlock = todo.last

      def xf_block(x: L, b: Block) = {
        saved(b) = b.statements.foldLeft(x)(domain.transfer)
        saved(b)
      }
      var nx = todo.foldLeft(x)((x, b) => {
        saved(b) = xf_block(x, b)
        saved(b)
      })
      saved(lastBlock) = nx
      if (nx != x) then {
        worklist.addAll(lastBlock.nextBlocks)
      }
      x = nx
    }
    saved.toMap
  }
}

case class CopyProp(expr: Expr, deps: Set[Variable])

case class CCP(
    constants: Map[Variable, Literal],
    // variable -> expr * dependencies
    val exprs: Map[Variable, CopyProp]
)

class ConstCopyProp(cyclicVariables: Map[Procedure, Set[Variable]]) extends AbstractDomain[CCP] {
  private final val callClobbers = (0 to 30).map("R" + _).map(c => Register(c, 64))

  def top: CCP = CCP(Map(), Map())
  def bot: CCP = CCP(Map(), Map())

  override def join(l: CCP, r: CCP): CCP = {
    val const = l.constants.keySet.intersect(r.constants.keySet)
    val exprs = l.exprs.keySet.intersect(r.exprs.keySet)
    CCP(
      const.collect {
        case k if (l.constants(k) == r.constants(k)) => k -> l.constants(k)
      }.toMap,
      exprs.collect {
        case k if (l.exprs(k) == r.exprs(k)) => k -> l.exprs(k)
      }.toMap
    )
  }

  override def transfer(c: CCP, s: Statement): CCP = {
    s match {
      case m: MemoryAssign => {
        // c.copy(exprs = c.exprs.filterNot((k, v) => v.expr.loads.nonEmpty))
        c
      }
      case Assign(l, r, lb) => {
        var p = c
        val evaled = eval.partialEvalExpr(
            eval.simplifyExprFixpoint(r),
            v => p.constants.get(v)
          )
        val rhsDeps = evaled.variables

        p = evaled match {
          case lit: Literal => p.copy(constants = p.constants.updated(l, lit), exprs = p.exprs.removed(l))
          case e: Expr if e.loads.isEmpty && !e.variables.contains(l) && !(cyclicVariables(s.parent.parent).contains(l)) =>
            p.copy(constants = p.constants.removed(l), exprs = p.exprs.updated(l, CopyProp(e, e.variables)))
          case _ => p.copy(constants = p.constants.removed(l), exprs = p.exprs.removed(l))
        }

        // remove candidates whose value changes due to this update
        p = p.copy(exprs = p.exprs.filterNot((k, v) => v.deps.exists(c => c.name == l.name)))

        p
      }
      case x: DirectCall => {
        val lhs = x.outParams.map(_._2)
        lhs.foldLeft(c)((c, l) =>
          c.copy(constants = c.constants.removed(l), exprs = c.exprs.filterNot((k, v) => v.deps.contains(l)))
        )
      }
      case x: IndirectCall => {
        val toClob = callClobbers
        toClob.foldLeft(c)((c, l) =>
          c.copy(constants = c.constants.removed(l), exprs = c.exprs.filterNot((k, v) => v.deps.contains(l)))
        )
      }
      case _ => c
    }
  }
}


class Simplify(
    val res: Map[Block, CCP],
    val initialBlock: Block = null,
) extends CILVisitor {

  var madeAnyChange = false
  var block : Block =  initialBlock

  def simp(pe: Expr)(ne: Expr) = {
    val simped = eval.partialEvalExpr(ir.eval.simplifyExprFixpoint(ne), x => None)
    if (pe != simped) {
      madeAnyChange = true
    }

    simped
  }

  override def vexpr(e: Expr) = {
    e match {
      case v: Variable if res(block).constants.contains(v) => {
        madeAnyChange = true
        ChangeDoChildrenPost(
          res(block).constants(v),
          simp(e)
        )
      }
      case v: Variable if res(block).exprs.contains(v) => {
        val repl = res(block).exprs(v).expr
        madeAnyChange = true
        ChangeDoChildrenPost(
          repl,
          simp(e)
        )
      }
      case e => ChangeDoChildrenPost(e, simp(e))
    }
  }

  override def vblock(b: Block) = {
    block = b
    DoChildren()
  }
}
