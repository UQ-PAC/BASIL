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

class Product[L, R, AL <: AbstractDomain[L], AR <: AbstractDomain[R]](val l: AL, val r: AR)
    extends AbstractDomain[(L, R)] {
  override def join(a: (L, R), b: (L, R)): (L, R) = (l.join(a._1, b._1), r.join(a._2, b._2))
  override def widen(a: (L, R), b: (L, R)): (L, R) = (l.widen(a._1, b._1), r.widen(a._2, b._2))
  override def narrow(a: (L, R), b: (L, R)): (L, R) = (l.narrow(a._1, b._1), r.narrow(a._2, b._2))
  override def transfer(a: (L, R), b: Statement): (L, R) = (l.transfer(a._1, b), r.transfer(a._2, b))

  override def top = (l.top, r.top)
  override def bot = (l.bot, r.bot)
}

enum MapWithDefault[+L] {
  case Top
  case Bot
  // current lattice value, memory of lattice values at each code point
  case V(value: L, memory: Map[Statement, L])
}

class MapDomain[L, D <: AbstractDomain[L]](val d: D) extends AbstractDomain[MapWithDefault[L]] {

  def top = MapWithDefault.Top
  def bot = MapWithDefault.Bot

  def toMap(v: MapWithDefault[L]): Map[Statement, L] = {
    v match {
      case MapWithDefault.Top     => Map().withDefaultValue(d.top)
      case MapWithDefault.Bot     => Map().withDefaultValue(d.bot)
      case MapWithDefault.V(_, m) => m.withDefaultValue(d.bot)
    }
  }

  def join(a: MapWithDefault[L], b: MapWithDefault[L]): MapWithDefault[L] = {
    (a, b) match {
      case (MapWithDefault.Top, MapWithDefault.Bot) => MapWithDefault.Top
      case (MapWithDefault.Bot, MapWithDefault.Top) => MapWithDefault.Top
      case (MapWithDefault.Top, _)                  => MapWithDefault.Top
      case (MapWithDefault.Bot, v)                  => v
      case (v, MapWithDefault.Bot)                  => v
      case (MapWithDefault.V(vl1, v1), MapWithDefault.V(vl2, v2)) => {
        MapWithDefault.V(
          d.join(vl1, vl2),
          v1.keys.foldLeft(v2)((m, a) => m + (a -> d.join(v1.get(a).getOrElse(d.bot), v2.get(a).getOrElse(d.bot))))
        )
      }
    }
  }

  def transfer(a: MapWithDefault[L], b: Statement): MapWithDefault[L] = {
    a match {
      case MapWithDefault.Top => MapWithDefault.Top
      case MapWithDefault.Bot => {
        val c = d.transfer(d.bot, b)
        MapWithDefault.V(c, Map(b -> c))
      }
      case MapWithDefault.V(vl, m) => {
        val c = d.transfer(vl, b)
        MapWithDefault.V(c, m.updated(b, c))
      }
    }
  }
}

def doCopyPropTransform(
    p: Program,
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
) = {
  val d = ConstCopyProp(reachingDefs)
  Logger.info("RPO")
  applyRPO(p)
  var rerun = true

  var runs = 0
  while (rerun && runs < 5) {
    Logger.info(s"Simp run $runs")
    runs += 1
    rerun = false
    val solver = workListSolver(d)
    val result = solver.solveProg(p, Set(), Set())

    for ((p, xf) <- result) {
      val vis = Simplify(xf, reachingDefs)
      visit_proc(vis, p)
      if (vis.madeAnyChange) {
        rerun = true
      }
    }
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

class workListSolver[L, A <: AbstractDomain[L]](domain: A) {

  def solveProg(
      p: Program,
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): Map[Procedure, L] = {
    val initDom = p.procedures.map(p => (p, p.blocks.filter(x => x.nextBlocks.size > 1 || x.prevBlocks.size > 1)))

    initDom.map(d => (d._1, solve(d._2, Set(), Set()))).toMap
  }

  def solve(
      initial: IterableOnce[Block],
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): L = {
    val saved: mutable.HashMap[Block, L] = mutable.HashMap()
    val workList = mutable.PriorityQueue[Block]()(Ordering.by(b => b.rpoOrder))
    workList.addAll(initial)

    var x = domain.bot
    while (workList.nonEmpty) {
      val b = workList.dequeue
      def bs(b: Block): List[Block] = {
        if (b.nextBlocks.size == 1) {
          val n = b.nextBlocks.head
          b :: bs(n)
        } else {
          List(b)
        }
      }

      x = b.prevBlocks.map(ib => saved.get(ib).getOrElse(domain.bot)).foldLeft(x)(domain.join)
      val todo = bs(b)
      val lastBlock = todo.last

      def xf_block(x: L, b: Block) = b.statements.foldLeft(x)(domain.transfer)
      var nx = todo.foldLeft(x)(xf_block)
      saved(lastBlock) = nx
      if (nx != x) then {
        workList.addAll(lastBlock.nextBlocks)
      }
      x = nx
    }
    x
  }
}

case class CopyProp(expr: Expr, deps: Set[RegisterWrapperEqualSets])

case class CCP(
    constants: Map[RegisterWrapperEqualSets, Literal],
    // variable -> expr * dependencies
    val exprs: Map[RegisterWrapperEqualSets, CopyProp]
)

class ConstCopyProp(val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])])
    extends AbstractDomain[CCP] {

  def top: CCP = CCP(Map(), Map())
  def bot: CCP = CCP(Map(), Map())

  override def join(l: CCP, r: CCP): CCP = {
    CCP(
      (l.constants ++ r.constants).removedAll(l.constants.keySet.intersect(r.constants.keySet)),
      (l.exprs ++ r.exprs).removedAll(l.exprs.keySet.intersect(r.exprs.keySet))
    )
  }

  override def transfer(c: CCP, s: Statement): CCP = {
    s match {
      case m: MemoryAssign => {
        c.copy(exprs = c.exprs.filterNot((k, v) => v.expr.loads.nonEmpty))
      }
      case Assign(lv, r, lb) => {
        val l = RegisterWrapperEqualSets(lv, getDefinition(lv, s, reachingDefs))

        var p = c
        val evaled = exprSimp(
          eval.partialEvalExpr(r, v => p.constants.get(RegisterWrapperEqualSets(v, getUse(v, s, reachingDefs))))
        )
        val rhsDeps = evaled.variables.map(v => RegisterWrapperEqualSets(v, getDefinition(v, s, reachingDefs)))

        p = evaled match {
          case lit: Literal => p.copy(constants = p.constants.updated(l, lit))
          case _: Expr => p.copy(constants = p.constants.removed(l), exprs = p.exprs.updated(l, CopyProp(evaled, rhsDeps)))
        }

        if (r.loads.nonEmpty) {
          // don't re-order loads
          p = p.copy(exprs = p.exprs.filter((k, v) => v.expr.loads.isEmpty))
        }
        // remove candidates whose value changes due to this update
        p = p.copy(exprs = p.exprs.filterNot((k, v) => v.deps.contains(l)))
        p = p.copy(exprs = p.exprs.filterNot((k, v) => v.deps.contains(k)))

        p
      }
      case _ => c
    }
  }
}

def exprSimp(e: Expr): Expr = {

  def simpOne(e: Expr): Expr = {
    e match {
      // normalise
      case BinaryExpr(op, x: Literal, y: Variable) => BinaryExpr(op, y, x)

      // identities
      case Extract(ed, 0, body) if (body.getType == BitVecType(ed))                        => exprSimp(body)
      case ZeroExtend(0, body)                                                             => exprSimp(body)
      case SignExtend(0, body)                                                             => exprSimp(body)
      case BinaryExpr(BVADD, body, BitVecLiteral(0, _))                                    => exprSimp(body)
      case BinaryExpr(BVMUL, body, BitVecLiteral(1, _))                                    => exprSimp(body)
      case Repeat(1, body)                                                                 => exprSimp(body)
      case Extract(ed, 0, ZeroExtend(extension, body)) if (body.getType == BitVecType(ed)) => exprSimp(body)
      case Extract(ed, 0, SignExtend(extension, body)) if (body.getType == BitVecType(ed)) => exprSimp(body)
      // case Extract(ed, 0, BinaryExpr(op, ZeroExtend(ex, hasVar), BitVecLiteral(v, sz)))    =>
      case BinaryExpr(BVXOR, l, r) if l == r =>
        e.getType match {
          case BitVecType(sz) => BitVecLiteral(0, sz)
        }
      // double negation
      case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body))     => exprSimp(body)
      case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body))     => exprSimp(body)
      case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => exprSimp(body)

      // this simplifies the slice register and then extend pattern on operations with bitvectors
      // to instead extend the bitvector to match the size of the variable
      case ZeroExtend(ed, BinaryExpr(op, Extract(sz1, 0, hasVar), BitVecLiteral(v, sz2)))
          if Set(BVADD, BVMUL, BVAND, BVOR).contains(op) =>
        BinaryExpr(op, hasVar, BitVecLiteral(v, ed + sz1))

      // constant folding
      // const + (x + const) -> (const + const) + x
      case BinaryExpr(BVADD, BinaryExpr(BVADD, body, l: BitVecLiteral), r: BitVecLiteral) =>
        BinaryExpr(BVADD, BinaryExpr(BVADD, l, r), exprSimp(body))
      case BinaryExpr(BVMUL, BinaryExpr(BVMUL, body, l: BitVecLiteral), r: BitVecLiteral) =>
        BinaryExpr(BVMUL, BinaryExpr(BVMUL, l, r), exprSimp(body))
      case BinaryExpr(BVOR, BinaryExpr(BVOR, body, l: BitVecLiteral), r: BitVecLiteral) =>
        BinaryExpr(BVOR, BinaryExpr(BVOR, l, r), exprSimp(body))
      case BinaryExpr(BVAND, BinaryExpr(BVAND, body, l: BitVecLiteral), r: BitVecLiteral) =>
        BinaryExpr(BVAND, BinaryExpr(BVAND, l, r), exprSimp(body))
      case r => r
    }
  }

  var pe = e
  var ne = simpOne(pe)
  while (ne != pe) {
    pe = ne
    ne = simpOne(pe)
  }
  ne
}

class Simplify(
    val res: CCP,
    val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
) extends CILVisitor {

  println(res)
  var madeAnyChange = false
  var statement: Statement = null

  override def vexpr(e: Expr) = {
    e match {
      case v: Variable if res.constants.contains(RegisterWrapperEqualSets(v, getUse(v, statement, reachingDefs))) => {
        madeAnyChange = true
        ChangeDoChildrenPost(
          res.constants(RegisterWrapperEqualSets(v, getUse(v, statement, reachingDefs))),
          exprSimp
        )
      }
      case v: Variable if res.exprs.contains(RegisterWrapperEqualSets(v, getUse(v, statement, reachingDefs))) => {
        val repl = res.exprs(RegisterWrapperEqualSets(v, getUse(v, statement, reachingDefs)))
        val u = RegisterWrapperEqualSets(v, getUse(v, statement, reachingDefs))
        println(s"COPYPROP $u repl=$repl")
        madeAnyChange = true
        ChangeDoChildrenPost(
          repl.expr,
          exprSimp
        )
      }
      case e => ChangeDoChildrenPost(e, exprSimp)
    }
  }

  override def vjump(j: Jump) = {
    SkipChildren()
  }

  override def vstmt(s: Statement) = {
    statement = s

    s match {
      case a @ Assign(l, r, lb) => {
        val state = res
        val nr = eval.partialEvalExpr(
          r,
          v => state.constants.get(RegisterWrapperEqualSets(v, getUse(v, statement, reachingDefs)))
        )
        if (nr != r) {
          madeAnyChange = true
        }
        a.rhs = nr
        DoChildren()
      }
      case _ => DoChildren()
    }
  }
}
