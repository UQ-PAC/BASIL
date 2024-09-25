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

def doCopyPropTransform(
    p: Program,
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
) = {

  var runs = 0
  var rerun = true
  while (runs < 5) {
    val rds2 = ReachingDefinitionsAnalysisSolver(p)
    val reachingDefs = rds2.analyze()
    val d = ConstCopyProp(reachingDefs)
    applyRPO(p)

    Logger.info(s"Simp run $runs")
    runs += 1
    val solver = worklistSolver(d)
    val result = solver.solveProg(p, Set(), Set())

    for ((p, xf) <- result) {
      val vis = Simplify(xf, reachingDefs)
      visit_proc(vis, p)
      rerun = vis.madeAnyChange
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

class worklistSolver[L, A <: AbstractDomain[L]](domain: A) {

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
      narrowpoints: Set[Block] // set of conditions
  ): L = {
    val saved: mutable.HashMap[Block, L] = mutable.HashMap()
    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(b => b.rpoOrder))
    worklist.addAll(initial)

    var x = domain.bot
    while (worklist.nonEmpty) {
      val b = worklist.dequeue

      if (b.label == "lmain_goto_l00000321") {
        println(s"$b\n  prevs : ${b.prevBlocks.map(c => saved.get(c))}")
      }

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
    x
  }
}

case class CopyProp(expr: Expr, deps: Set[RegisterVariableWrapper])

case class CCP(
    constants: Map[RegisterVariableWrapper, Literal],
    // variable -> expr * dependencies
    val exprs: Map[RegisterVariableWrapper, CopyProp]
)

class ConstCopyProp(val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])])
    extends AbstractDomain[CCP] {

  private final val callClobbers = (0 to 30).map("R" + _).map(c => Register(c, 64))

  def top: CCP = CCP(Map(), Map())
  def bot: CCP = CCP(Map(), Map())

  override def join(l: CCP, r: CCP): CCP = {
    CCP(
      (l.constants ++ r.constants).removedAll(l.constants.keySet.intersect(r.constants.keySet).filterNot(k => l.constants(k) == r.constants(k))),
      (l.exprs ++ r.exprs).removedAll(l.exprs.keySet.intersect(r.exprs.keySet).filterNot(k => l.exprs(k) == r.exprs(k)))
    )
  }

  override def transfer(c: CCP, s: Statement): CCP = {
    s match {
      case m: MemoryAssign => {
        c.copy(exprs = c.exprs.filterNot((k, v) => v.expr.loads.nonEmpty))
      }
      case Assign(lv, r, lb) => {
        val l = RegisterVariableWrapper(lv, getDefinition(lv, s, reachingDefs))

        var p = c
        val evaled = exprSimp(
          eval.partialEvalExpr(
            exprSimp(r),
            v => p.constants.get(RegisterVariableWrapper(v, getUse(v, s, reachingDefs)))
          )
        )
        val rhsDeps = evaled.variables.map(v => RegisterVariableWrapper(v, getUse(v, s, reachingDefs)))

        p = evaled match {
          case lit: Literal => p.copy(constants = p.constants.updated(l, lit))
          case _: Expr =>
            p.copy(constants = p.constants.removed(l), exprs = p.exprs.updated(l, CopyProp(evaled, rhsDeps)))
        }

        if (r.loads.nonEmpty) {
          // don't re-order loads
          p = p.copy(exprs = p.exprs.filter((k, v) => v.expr.loads.isEmpty))
        }
        // remove candidates whose value changes due to this update
        // without an SSA form in the output, we can't propagate assignments such that R0_1 := f(R0_0)
        //  or; only replace such that all uses are copyproped, the dead definition is removed
        p = p.copy(exprs = p.exprs.filterNot((k, v) => v.deps.exists(c => c.variable.name == l.variable.name)))

        p
      }
      case x: Call => {
        val toClob = callClobbers.map(v => RegisterVariableWrapper(v, getDefinition(v, s, reachingDefs)))
        toClob.foldLeft(c)((c, l) =>
          c.copy(constants = c.constants.removed(l), exprs = c.exprs.filterNot((k, v) => v.deps.contains(l)))
        )
      }
      case _ => c
    }
  }
}

def exprSimp(e: Expr): Expr = {

  val assocOps: Set[BinOp] =
    Set(BVADD, BVMUL, BVOR, BVAND, BVEQ, BoolAND, BoolEQ, BoolOR, BoolEQUIV, BoolEQ, IntADD, IntMUL, IntEQ)

  def simpOne(e: Expr): Expr = {
    e match {
      // normalise
      case BinaryExpr(op, x: Literal, y: Expr) if !y.isInstanceOf[Literal] && assocOps.contains(op) =>
        BinaryExpr(op, y, x)

      // identities
      case Extract(ed, 0, body) if (body.getType == BitVecType(ed))                        => exprSimp(body)
      case ZeroExtend(0, body)                                                             => exprSimp(body)
      case SignExtend(0, body)                                                             => exprSimp(body)
      case BinaryExpr(BVADD, body, BitVecLiteral(0, _))                                    => exprSimp(body)
      case BinaryExpr(BVMUL, body, BitVecLiteral(1, _))                                    => exprSimp(body)
      case Repeat(1, body)                                                                 => exprSimp(body)
      case Extract(ed, 0, ZeroExtend(extension, body)) if (body.getType == BitVecType(ed)) => exprSimp(body)
      case Extract(ed, 0, SignExtend(extension, body)) if (body.getType == BitVecType(ed)) => exprSimp(body)
      case BinaryExpr(BVXOR, l, r) if l == r =>
        e.getType match {
          case BitVecType(sz) => BitVecLiteral(0, sz)
        }
      // double negation
      case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body))     => exprSimp(body)
      case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body))     => exprSimp(body)
      case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => exprSimp(body)

      // compose slices
      case Extract(ed1, be1, Extract(ed2, be2, body)) => Extract(ed1 - be2, be1 + be2, exprSimp(body))

      // (comp (comp x y) 1) = (comp x y)
      case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(1, 1)) => exprSimp(body)
      case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)) => UnaryExpr(BVNOT, exprSimp(body))
      case BinaryExpr(
            BVEQ,
            BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)),
            BitVecLiteral(1, 1)
          ) =>
        BinaryExpr(BVEQ, exprSimp(body), BitVecLiteral(0, 1))

      // constant folding
      // const + (e + const) -> (const + const) + e
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

object SSARename:

  def concretiseSSA(p: Program, reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]) = {
    println("SSA conc ")
    val c = SSACollect(p, reachingDefs)
    c.initial()
    visit_prog(c, p)
    println(c.names)
    val rn = SSARename(p, c.names, reachingDefs)
    visit_prog(rn, p)
  }

  class SSARename(p: Program, renames: mutable.HashMap[Assign, Int], reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]) extends CILVisitor {
    var statement : Statement = null

    override def vvar(v: Variable) = {
      val assigns = getUse(v, statement, reachingDefs)
      assigns.headOption.map(doRename(v, _)) match {
        case Some(nv) => ChangeTo(nv)
        case None => SkipChildren()
      }
    }

    def doRename(v: Variable, definition: Assign): Variable = {
      renames.get(definition) match {
        case Some(idx) => {
          v match {
            case Register(n, sz) => Register(n + "_" + idx, sz)
            case LocalVar(n, t) => LocalVar(n + "_" + idx, t)
          }
        }
        case None => v
      }
    }

    override def vstmt(s: Statement) = {
      statement = s
      s match {
        case a @ Assign(l, r, _) => {
          a.lhs = doRename(l, a)
          DoChildren()
        }
        case _ => DoChildren()
      }
    }

  }

  class SSACollect(p: Program, reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]) extends CILVisitor {
    val names = mutable.HashMap[Assign, Int]()
    var count = 0
    var statement : Statement = null

    override def vvar(v: Variable) =  {
      val assigns = getUse(v, statement, reachingDefs)
      if (assigns.size > 1) {
        count += 1
        for (a <- assigns) {
          names(a) = count 
        }
      }
      DoChildren()
    }

    def initial() = {
      // give each definition a unique index
      for (c <- p) {
        count += 1
        c match {
          case a : Assign => names(a) = count
          case _ => ()
        }
      }
    }

    override def vstmt(s: Statement) = {
      statement = s
      DoChildren()
    }

  }

class Simplify(
    val res: CCP,
    val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
) extends CILVisitor {

  var madeAnyChange = false
  var statement: Statement = null

  def simp(pe: Expr)(ne: Expr) = {
    val simped = eval.partialEvalExpr(exprSimp(ne), x => None)
    if (pe != simped) {
      madeAnyChange = true
    }

    simped
  }

  override def vexpr(e: Expr) = {
    e match {
      case v: Variable if res.constants.contains(RegisterVariableWrapper(v, getUse(v, statement, reachingDefs))) => {
        madeAnyChange = true
        ChangeDoChildrenPost(
          res.constants(RegisterVariableWrapper(v, getUse(v, statement, reachingDefs))),
          simp(e)
        )
      }
      case v: Variable if res.exprs.contains(RegisterVariableWrapper(v, getUse(v, statement, reachingDefs))) => {
        val repl = res.exprs(RegisterVariableWrapper(v, getUse(v, statement, reachingDefs)))
        val u = RegisterVariableWrapper(v, getUse(v, statement, reachingDefs))
        madeAnyChange = true
        ChangeDoChildrenPost(
          repl.expr,
          simp(e)
        )
      }
      case e => ChangeDoChildrenPost(e, simp(e))
    }
  }

  override def vstmt(s: Statement) = {
    statement = s

    s match {
      case a @ Assign(l, r, lb) => {
        val state = res
        val nr = eval.partialEvalExpr(
          r,
          v => state.constants.get(RegisterVariableWrapper(v, getUse(v, statement, reachingDefs)))
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
