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

def removeSlices(p: Program): Unit = {
  p.procedures.foreach(removeSlices)
}

def removeSlices(p: Procedure): Unit = {

  /** if for each variable v there is some i:int such that (i) all its assignments have a ZeroExtend(i, x) and (ii) all
    * its uses have Extract(size(v) - i, 0, v) Then we replace v by a variable of bitvector size (size(v) - i)
    *
    * We check this flow-insensitively and recover precision using DSA form.
    */

  val assignments: Map[LocalVar, Iterable[Assign]] = p
    .collect { case a: Assign =>
      a
    }
    .groupBy(_.lhs)
    .collect { case (k: LocalVar, v) =>
      (k, v)
    }

  enum HighZeroBits:
    case Bits(n: Int) // (i) and (ii) hold; the n highest bits are redundant
    case False // property is false
    case Bot // don't know anything

  def size(e: Expr) = {
    e.getType match {
      case BitVecType(s) => Some(s)
      case _             => None
    }
  }

  case class LVTerm(v: LocalVar) extends analysis.solvers.Var[LVTerm]

  // unify variable uses across direct assignments
  val ufsolver = analysis.solvers.UnionFindSolver[LVTerm]()
  val unioned = assignments.foreach {
    case (lv, Assign(lhs: LocalVar, rhs: LocalVar, _)) => ufsolver.unify(LVTerm(lhs), LVTerm(rhs))
    case _                                             => ()
  }

  val unifiedAssignments = ufsolver
    .unifications()
    .map { case (v: LVTerm, rvs) =>
      v.v -> (rvs.map { case LVTerm(rv) =>
        rv
      }).toSet
    }
    .map((repr: LocalVar, elems: Set[LocalVar]) =>
      repr -> elems.flatMap(assignments(_).filter(_ match {
        // filter out the direct assignments we used to build the unif class
        case Assign(lhs: LocalVar, rhs: LocalVar, _) if elems.contains(lhs) && elems.contains(rhs) => false
        case _                                                                                     => true
      }))
    )

  // try and find a single extension size for all rhs of assignments to all variables in the assigned equality class
  val varHighZeroBits: Map[LocalVar, HighZeroBits] = assignments.map((v, assigns) =>
    // note: this overapproximates on x := y when x and y may both be smaller than their declared size
    val allRHSExtended = assigns.foldLeft(HighZeroBits.Bot: HighZeroBits)((e, assign) =>
      (e, assign.rhs) match {
        case (HighZeroBits.Bot, ZeroExtend(i, lhs))                   => HighZeroBits.Bits(i)
        case (b @ HighZeroBits.Bits(ei), ZeroExtend(i, _)) if i == ei => b
        case (b @ HighZeroBits.Bits(ei), ZeroExtend(i, _)) if i != ei => HighZeroBits.False
        case (HighZeroBits.False, _)                                  => HighZeroBits.False
        case (_, other)                                               => HighZeroBits.False
      }
    )
    (v, allRHSExtended)
  )

  val varsWithExtend: Map[LocalVar, HighZeroBits] = assignments
    .map((lhs, _) => {
      // map all lhs to the result for their representative
      val rep = ufsolver.find(LVTerm(lhs)) match {
        case LVTerm(r) => r
      }
      lhs -> varHighZeroBits.get(rep)
    })
    .collect { case (l, Some(x)) /* remove anything we have no information on */ =>
      (l, x)
    }

  class CheckUsesHaveExtend() extends CILVisitor {
    val result: mutable.HashMap[LocalVar, HighZeroBits] =
      mutable.HashMap[LocalVar, HighZeroBits]()

    override def vexpr(v: Expr) = {
      v match {
        case Extract(i, 0, v: LocalVar)
            if size(v).isDefined && result.get(v).contains(HighZeroBits.Bits(size(v).get - i)) =>
          SkipChildren()
        case v: LocalVar => {
          result.remove(v)
          SkipChildren()
        }
        case _ => DoChildren()
      }
    }

    def apply(assignHighZeroBits: Map[LocalVar, HighZeroBits])(p: Procedure): Map[LocalVar, HighZeroBits] = {
      result.clear()
      result.addAll(assignHighZeroBits)
      visit_proc(this, p)
      result.toMap
    }
  }

  val toSmallen = CheckUsesHaveExtend()(varsWithExtend)(p).collect { case (v, HighZeroBits.Bits(x)) =>
    v -> x
  }.toMap

  class ReplaceAlwaysSlicedVars(varHighZeroBits: Map[LocalVar, Int]) extends CILVisitor {

    override def vexpr(v: Expr) = {
      v match {
        case Extract(i, 0, v: LocalVar) if size(v).isDefined && varHighZeroBits.contains(v) => {
          ChangeTo(LocalVar(v.name, BitVecType(size(v).get - varHighZeroBits(v))))
        }
        case _ => DoChildren()
      }
    }

    override def vstmt(s: Statement) = {
      s match {
        case a @ Assign(lhs: LocalVar, ZeroExtend(sz, rhs), _)
            if size(lhs).isDefined && varHighZeroBits.contains(lhs) => {
          assert(varHighZeroBits(lhs) == sz)
          a.lhs = LocalVar(lhs.name, BitVecType(size(lhs).get - varHighZeroBits(lhs)))
          a.rhs = rhs
          DoChildren()
        }
        case _ => DoChildren()
      }
    }
  }

  visit_proc(ReplaceAlwaysSlicedVars(toSmallen), p)

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
  }) { ; }

  ndeps
}

def cyclicVariables(p: Procedure): Set[Variable] = {
  val deps = saturateVariableDependencies(p)
  deps.filter(v => v._2.contains(v._1)).map(_._1).toSet
}

def getRedundantAssignments(procedure: Procedure): Set[Assign] = {

  /** Get all assign statements which define a variable never used, assuming ssa form and proc parameters so that
    * interprocedural check is not required.
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
    val initDom = p.procedures.map(p => (p, p.blocks))

    initDom.map(d => (d._1, solve(d._2, Set(), Set()))).toMap
  }

  def solve(
      initial: IterableOnce[Block],
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): Map[Block, L] = {
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
          case e: Expr
              if e.loads.isEmpty && !e.variables.contains(l) && !(cyclicVariables(s.parent.parent).contains(l)) =>
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
    val initialBlock: Block = null
) extends CILVisitor {

  var madeAnyChange = false
  var block: Block = initialBlock

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
