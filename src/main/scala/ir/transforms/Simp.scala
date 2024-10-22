package ir.transforms
import translating.serialiseIL

import util.Logger
import ir.eval.AlgebraicSimplifications
import ir.cilvisitor.*
import ir.*
import scala.collection.mutable
import analysis._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global

trait AbstractDomain[L] {
  def join(a: L, b: L): L
  def widen(a: L, b: L): L = join(a, b)
  def narrow(a: L, b: L): L = a
  def transfer(a: L, b: Statement): L

  def top: L
  def bot: L
}

object MakeLocalsBlockUnique extends CILVisitor {
  var blockLabel: String = ""

  override def vlvar(v: Variable) = v match {
    case LocalVar(name, t) => ChangeTo(LocalVar(blockLabel + "_" + name, t))
    case _                 => SkipChildren()
  }

  override def vrvar(v: Variable) = v match {
    case LocalVar(name, t) => ChangeTo(LocalVar(blockLabel + "_" + name, t))
    case _                 => SkipChildren()
  }

  override def vblock(b: Block) = {
    blockLabel = b.label
    DoChildren()
  }

  def apply(p: Program) = {
    for (proc <- p.procedures.filter(_.entryBlock.isDefined)) {
      blockLabel = ""
      visit_proc(this, proc)
    }
  }
}

def removeSlices(p: Program): Unit = {
  p.procedures.foreach(removeSlices)
}

def circularDeps(p: Procedure): Set[Variable] = {

  /** this is flow insensitive, used to remove copy prop candidates which produce a substitution cycle
    */

  case class VTerm(v: Variable) extends analysis.solvers.Var[VTerm]

  // map variable to the representative of the set of dependencies
  val results = mutable.Map[Variable, VTerm]()
  val ufsolver = analysis.solvers.UnionFindSolver[VTerm]()

  def addDependency(lhs: Variable, dependency: Variable) = {
    val rep = results.get(lhs) match {
      case Some(d) => {
        ufsolver.unify(d, VTerm(dependency))
        val nrep = ufsolver.find(d).asInstanceOf[VTerm]
        results(lhs) = nrep
      }
      case None => VTerm(dependency)
    }
  }

  p.foreach {
    case (Assign(lhs: LocalVar, rhs, _)) => {
      for (rvar <- rhs.variables) {
        addDependency(lhs, rvar)
      }
    }
    case d: DirectCall => {
      // unify formal and actual
      for (rvar <- d.actualParams) {
        for (r <- rvar._2.variables) {
          addDependency(rvar._1, r)
        }
      }
      for (lvar <- d.outParams) {
        addDependency(lvar._1, lvar._2)
        for (rvar <- d.actualParams.flatMap(_._2.variables)) {
          // unify in and out
          addDependency(lvar._2, rvar)
        }
      }
    }
    case _ => ()
  }

  val unif = ufsolver.unifications().map((k, v) => (k, v.toSet)).toMap
  val circular = results
    .filter((v, rep) => {
      unif(rep).contains(VTerm(v))
    })
    .map((v, rep) => v)
    .toSet

  circular

}

def removeSlices(p: Procedure): Unit = {
  case class LVTerm(v: LocalVar) extends analysis.solvers.Var[LVTerm]

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

def copypropTransform(p: Procedure) = {
  val t = util.PerformanceTimer(s"simplify ${p.name} (${p.blocks.size} blocks)")
  val dom = ConstCopyProp()
  val solver = worklistSolver(dom)

  // Logger.info(s"${p.name} ExprComplexity ${ExprComplexity()(p)}")
  val result = solver.solveProc(p)
  val solve = t.checkPoint("Solve CopyProp")

  val vis = Simplify(result.withDefaultValue(dom.bot))
  visit_proc(vis, p)
  val xf = t.checkPoint("transform")
  // Logger.info(s"    ${p.name} after transform expr complexity ${ExprComplexity()(p)}")

  visit_proc(CleanupAssignments(), p)
  t.checkPoint("redundant assignments")
  // Logger.info(s"    ${p.name} after dead var cleanup expr complexity ${ExprComplexity()(p)}")

  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  // Logger.info(s"    ${p.name}  after simp expr complexity ${ExprComplexity()(p)}")
  val sipm = t.checkPoint("algebraic simp")
}

def doCopyPropTransform(p: Program) = {

  applyRPO(p)

  Logger.info("[!] Simplify :: Expr/Copy-prop Transform")
  val work = p.procedures
    .filter(_.blocks.size > 0)
    .map(p =>
      p -> //Future
        {
          Logger
            .debug(s"CopyProp Transform ${p.name} (${p.blocks.size} blocks, expr complexity ${ExprComplexity()(p)})")
          copypropTransform(p)
        }
    )

  work.foreach((p, job) => {
    try {
      //Await.result(job, 10000.millis)
      job
    } catch {
      case e => {
        Logger.error("Simplify :: CopyProp " + p.name + ": " + e.toString)
      }
    }
  })

  Logger.info("[!] Simplify :: Dead variable elimination")

  // cleanup
  visit_prog(CleanupAssignments(), p)
  val toremove = p.collect {
    case b: Block if b.statements.size == 0 && b.prevBlocks.size == 1 && b.nextBlocks.size == 1 => b
  }

  Logger.info("[!] Simplify :: Merge empty blocks")
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

  def solveProc(p: Procedure) = {
    solve(p.blocks, Set(), Set())
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
          (prog, Await.result(x, 10000.millis))
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
      narrowpoints: Set[Block] // set of conditions
  ): Map[Block, L] = {
    val saved: mutable.HashMap[Block, L] = mutable.HashMap()
    val saveCount: mutable.HashMap[Block, Int] = mutable.HashMap()
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
        var blocks = mutable.LinkedHashSet[Block]()
        var thisBlock = b
        while ({
          blocks.add(thisBlock)

          if (thisBlock.nextBlocks.size == 1) {
            thisBlock = thisBlock.nextBlocks.head
            blocks.contains(thisBlock)
          } else {
            false
          }
        }) {}
        blocks.toList
      }

      val prev = saved.get(b)
      x = b.prevBlocks.flatMap(ib => saved.get(ib).toList).foldLeft(x)(domain.join)
      saved(b) = x
      // val todo = bs(b)
      val todo = List(b)
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
      saveCount(lastBlock) = saveCount.get(lastBlock).getOrElse(0) + 1
      if (!prev.contains(nx)) then {
        if (saveCount(lastBlock) == 50) {
          Logger.warn(s"Large join count on block ${lastBlock.label}, no fix point? (-v for mor info)")
          Logger.debug(lastBlock.label + "    ==> " + x)
          Logger.debug(lastBlock.label + "    <== " + nx)
        }
        worklist.addAll(lastBlock.nextBlocks)
      }
      x = nx
    }
    saved.toMap
  }
}

// case class CopyProp(from: Expr, expr: Expr, deps: Set[Variable])

enum CopyProp {
  case Bot
  case Prop(expr: Expr, deps: Set[Variable])
  case Clobbered
}

case class CCP(
    val state: Map[Variable, CopyProp] = Map()
)

object CCP {
  def toSubstitutions(c: CCP): Map[Variable, Expr] = {
    c.state.collect { case (v, CopyProp.Prop(e, _)) =>
      v -> e
    }
  }
}

class ConstCopyProp() extends AbstractDomain[CCP] {
  private final val callClobbers = (0 to 30).map("R" + _).map(c => Register(c, 64))

  def top: CCP = CCP(Map().withDefaultValue(CopyProp.Bot))
  def bot: CCP = CCP(Map().withDefaultValue(CopyProp.Clobbered))

  override def join(l: CCP, r: CCP): CCP = {
    val ks = l.state.keySet.intersect(r.state.keySet)
    val merged = ks.map(v =>
      (v ->
        ((l.state(v), r.state(v)) match {
          case (l, CopyProp.Bot)                                                            => l
          case (CopyProp.Bot, r)                                                            => r
          case (c @ CopyProp.Clobbered, _)                                                  => c
          case (_, c @ CopyProp.Clobbered)                                                  => c
          case (p1 @ CopyProp.Prop(e1, deps1), p2 @ CopyProp.Prop(e2, deps2)) if (p1 == p2) => p1
          case (_, _)                                                                       => CopyProp.Clobbered
        }))
    )
    CCP(merged.toMap)
  }

  def clobberFull(c: CCP, l: Variable) = {
    val p = clobber(c, l)
    p.copy(state = p.state + (l -> CopyProp.Clobbered))
  }

  def clobber(c: CCP, l: Variable) = {
    CCP(
      c.state
        .map((k, v) =>
          k -> (v match {
            case CopyProp.Prop(_, deps) if deps.contains(l) => CopyProp.Clobbered
            case o                                          => o
          })
        )
        .withDefaultValue(CopyProp.Bot)
    )
  }

  override def transfer(c: CCP, s: Statement): CCP = {
    s match {
      case m: MemoryAssign => {
        // c.copy(exprs = c.exprs.filterNot((k, v) => v.expr.loads.nonEmpty))
        c
      }
      case Assign(l, r, lb) => {
        if (r.loads.size > 0) {
          clobberFull(c, l)
        } else {
          val consts = c.state.collect {
            case (k, CopyProp.Prop(c, deps)) if deps.isEmpty => k -> c
          }
          val evaled = ir.eval.partialEvalExpr(Substitute(consts, false)(r).getOrElse(r), e => None)
          val rhsDeps = evaled.variables.toSet
          val existing = c.state.get(l).getOrElse(CopyProp.Bot)

          val ns = existing match {
            case CopyProp.Bot                                 => CopyProp.Prop(evaled, rhsDeps) // not seen yet
            case CopyProp.Prop(e, _)                          => CopyProp.Prop(evaled, rhsDeps)
            case _                                            => CopyProp.Clobbered // our expr value has changed
          }
          val p = c.copy(state = c.state + (l -> ns))
          clobber(p, l)
        }
      }
      case x: DirectCall => {
        val lhs = x.outParams.map(_._2)
        lhs.foldLeft(c)(clobberFull)
      }
      case x: IndirectCall => {
        val toClob = callClobbers
        toClob.foldLeft(c)(clobberFull)
      }
      case _ => c
    }
  }
}

class ExprComplexity extends CILVisitor {
  // count the nodes in the expression AST
  var count = 0
  override def vexpr(e: Expr) = {
    count += 1
    DoChildren()
  }

  def apply(e: Procedure) = {
    count = 0
    visit_proc(this, e)
    count
  }

  def apply(e: Expr) = {
    count = 0
    visit_expr(this, e)
    count
  }
}

class Substitute(
    val res: Map[Variable, Expr],
    val recurse: Boolean = true,
    val complexityThreshold: Int = 0
) extends CILVisitor {
  var madeAnyChange = false
  var complexity = 0

  override def vexpr(e: Expr) = {
    e match {
      case v: Variable if res.contains(v) => {
        val changeTo = res(v)
        if (complexityThreshold > 0) {
          complexity += ExprComplexity()(changeTo)
        }
        if (complexityThreshold > 0 && complexity > complexityThreshold) {
          SkipChildren()
        } else if (recurse) {
          madeAnyChange = true
          ChangeDoChildrenPost(changeTo, x => x)
        } else {
          madeAnyChange = true
          ChangeTo(changeTo)
        }
      }
      case e => DoChildren()
    }
  }

  def apply(e: Expr): Option[Expr] = {
    madeAnyChange = false
    val ne = visit_expr(this, e)
    val changed = madeAnyChange
    madeAnyChange = false
    if (changed) {
      Some(ne)
    } else {
      None
    }
  }

}

class Simplify(
    val res: Map[Block, CCP],
    val initialBlock: Block = null
) extends CILVisitor {

  var madeAnyChange = false
  var block: Block = initialBlock
  var skipped = Set[String]()

  override def vexpr(e: Expr) = {
    val threshold = 500
    val variables = e.variables.toSet
    val subst = Substitute(CCP.toSubstitutions(res(block)), true, threshold)
    val result = subst(e).getOrElse(e)
    if (subst.complexity > threshold) {
      val bl = s"${block.parent.name}::${block.label}"
      if (!skipped.contains(bl)) {
        skipped = skipped + bl
        Logger.warn(s"Some skipped substitution at $bl due to resulting expr size > ${threshold} threshold")
      }
    }
    ChangeTo(result)
  }

  override def vblock(b: Block) = {
    block = b
    DoChildren()
  }
}
