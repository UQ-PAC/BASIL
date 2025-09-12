package ir.transforms

import ir.*
import ir.cilvisitor.*
import ir.eval.{AlgebraicSimplifications, AssumeConditionSimplifications, simplifyExprFixpoint}
import translating.PrettyPrinter.*
import util.assertion.*
import util.{SimplifyLogger, condPropDebugLogger}

import java.io.BufferedWriter
import scala.collection.mutable
import scala.util.boundary

import boundary.break

/** Simplification pass, see also: docs/development/simplification-solvers.md
  */

def getLiveVars(p: Procedure): (Map[Block, Set[Variable]], Map[Block, Set[Variable]]) = {
  val liveVarsDom = IntraLiveVarsDomain()
  val liveVarsSolver = worklistSolver(liveVarsDom)
  liveVarsSolver.solveProc(p, backwards = true)
}

def difftestLiveVars(p: Procedure, compareResult: Map[CFGPosition, Set[Variable]]) = {
  val (liveBefore, liveAfter) = getLiveVars(p)
  var passed = true

  for ((b, s) <- liveBefore) {
    val c = (compareResult(b) == s)
    passed = passed && c
    if (!c) {
      SimplifyLogger.error(
        s"LiveVars unequal ${b.label}: ${compareResult(b)} == $s (differing ${compareResult(b).diff(s)})"
      )
    }
  }
  passed
}

def basicReachingDefs(p: Procedure): Map[Command, Map[Variable, Set[Assign | DirectCall]]] = {
  reversePostOrder(p)
  val lives = getLiveVars(p)
  basicReachingDefs(p, lives)
}

def basicReachingDefs(
  p: Procedure,
  liveVars: (Map[Block, Set[Variable]], Map[Block, Set[Variable]])
): Map[Command, Map[Variable, Set[Assign | DirectCall]]] = {
  val (beforeLive, afterLive) = liveVars
  val dom = DefUseDomain(beforeLive)
  val solver = worklistSolver(dom)
  // type rtype = Map[Block, Map[Variable, Set[Assign | DirectCall]]]
  val (beforeRes, afterRes) = solver.solveProc(p)

  val merged: Map[Command, Map[Variable, Set[Assign | DirectCall]]] =
    beforeRes
      .flatMap((block, sts) => {
        val b = Seq(IRWalk.firstInBlock(block) -> sts)
        val stmts =
          if (block.statements.nonEmpty) then
            (block.statements.toList: List[Command]).zip(block.statements.toList.tail ++ List(block.jump))
          else List()
        val transferred = stmts
          .foldLeft((sts, List[(Command, Map[Variable, Set[Assign | DirectCall]])]()))((st, s) => {
            // map successor to transferred predecessor
            val x = dom.transfer(st._1, s._1)
            (x, (s._2 -> x) :: st._2)
          })
          ._2
          .toMap
        b ++ transferred
      })
      .toMap
  merged
}

// map v -> definitions reached here
class DefUseDomain(liveBefore: Map[Block, Set[Variable]]) extends AbstractDomain[Map[Variable, Set[Assign]]] {

  override def transfer(s: Map[Variable, Set[Assign]], b: Command) = {
    b match {
      case a: Assign => a.assignees.foldLeft(s)((s, v) => s.updated(v, Set(a)))
      case _ => s
    }
  }
  override def top = ???
  def bot = Map[Variable, Set[Assign]]()
  def join(l: Map[Variable, Set[Assign]], r: Map[Variable, Set[Assign]], pos: Block) = {
    l.keySet
      .union(r.keySet)
      .filter(k => liveBefore(pos).contains(k))
      .map(k => {
        k -> (l.get(k).getOrElse(Set()) ++ r.get(k).getOrElse(Set()))
      })
      .toMap
  }
}

enum Def {
  case Def(a: Assign)
  case Entry
}

// map v -> definitions reached here
class DefUseEntryDomain() extends AbstractDomain[Map[Variable, Set[Def]]] {

  override def transfer(s: Map[Variable, Set[Def]], b: Command) = {
    b match {
      case a: LocalAssign => s.updated(a.lhs, Set(Def.Def(a)))
      case a: MemoryLoad => s.updated(a.lhs, Set(Def.Def(a)))
      case d: DirectCall => d.outParams.map(_._2).foldLeft(s)((s, r) => s.updated(r, Set(Def.Def(d))))
      case _ => s
    }
  }
  override def top = ???
  def bot = Map[Variable, Set[Def]]()
  def init = Map[Variable, Set[Def]]().withDefaultValue(Def.Entry)

  def join(l: Map[Variable, Set[Def]], r: Map[Variable, Set[Def]], pos: Block) = {
    l.keySet
      .union(r.keySet)
      .map(k => {
        k -> (l.get(k).getOrElse(Set(Def.Entry)) ++ r.get(k).getOrElse(Set(Def.Entry)))
      })
      .toMap
  }
}

class IntraLiveVarsDomain extends PowerSetDomain[Variable] {
  // expected backwards

  def transfer(s: Set[Variable], a: Command): Set[Variable] = {
    a match {
      case a: LocalAssign => (s - a.lhs) ++ a.rhs.variables
      case a: MemoryAssign => (s - a.lhs) ++ a.rhs.variables
      case c: SimulAssign => (s -- c.assignments.map(_._1)) ++ c.assignments.flatMap(_._2.variables)
      case a: MemoryLoad => (s - a.lhs) ++ a.index.variables
      case m: MemoryStore => s ++ m.index.variables ++ m.value.variables
      case a: Assume => s ++ a.body.variables
      case a: Assert => s ++ a.body.variables
      case i: IndirectCall => s + i.target
      case c: DirectCall => (s -- c.outParams.map(_._2)) ++ c.actualParams.flatMap(_._2.variables)
      case g: GoTo => s
      case r: Return => s ++ r.outParams.flatMap(_._2.variables)
      case r: Unreachable => s
      case n: NOP => s
    }
  }
}

class LiveVarsDomWatchFlags(defs: Map[Variable, Set[Assign]]) extends IntraLiveVarsDomain {

  val g = GuardVisitor()

  def substitute(deps: mutable.Set[Variable])(v: Variable) = {
    if (g.goodSubst(v) && defs.get(v).isDefined && defs(v).size == 1) {
      deps.add(v)
      defs(v).head match {
        case l: LocalAssign =>
          deps.addAll(l.rhs.variables)
          deps.add(v)
          Some(l.rhs)
        case _ => None
      }
    } else None
  }

  override def transfer(s: Set[Variable], a: Command): Set[Variable] = a match {
    case a: Assume => {
      val deps = mutable.Set[Variable]()
      deps.addAll(a.body.variables)
      Substitute(substitute(deps), true)(a.body)
      s ++ deps
    }
    case o => super.transfer(s, a)
  }
}

case class LVTerm(v: LocalVar) extends analysis.solvers.Var[LVTerm]

def removeSlices(p: Procedure): Unit = {

  /** if for each variable v there is some i:int such that (i) all its assignments have a ZeroExtend(i, x) and (ii) all
    * its uses have Extract(size(v) - i, 0, v) Then we replace v by a variable of bitvector size (size(v) - i)
    *
    * We check this flow-insensitively and recover precision using DSA form.
    */
  val assignments: Map[LocalVar, Iterable[Assign]] = p
    .collect {
      case a: SingleAssign => Seq(a.lhs -> a)
      case a: DirectCall => a.assignees.map(e => e -> a)
    }
    .flatten
    .groupBy(_._1)
    .map((k, v) => (k, v.map(_._2).toSet))
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
      case _ => None
    }
  }
  // unify variable uses across direct assignments
  val ufsolver = analysis.solvers.UnionFindSolver[LVTerm]()
  val unioned = assignments.foreach {
    case (lv, LocalAssign(lhs: LocalVar, rhs: LocalVar, _)) => ufsolver.unify(LVTerm(lhs), LVTerm(rhs))
    case _ => ()
  }
  val unifiedAssignments = ufsolver
    .unifications()
    .map {
      case (v @ LVTerm(_), rvs) =>
        v.v -> (rvs.map {
          case LVTerm(rv) =>
            rv
          case _ => ??? /* unreachable */
        }).toSet
      case _ => ??? /* unreachable */
    }
    .map((repr: LocalVar, elems: Set[LocalVar]) =>
      repr -> elems.flatMap(assignments(_).filter(_ match {
        // filter out the direct assignments we used to build the unif class
        case LocalAssign(lhs: LocalVar, rhs: LocalVar, _) if elems.contains(lhs) && elems.contains(rhs) => false
        case _ => true
      }))
    )
  // try and find a single extension size for all rhs of assignments to all variables in the assigned equality class
  val varHighZeroBits: Map[LocalVar, HighZeroBits] = assignments.map((v, assigns) =>
    // note: this overapproximates on x := y when x and y may both be smaller than their declared size
    val allRHSExtended = assigns.foldLeft(HighZeroBits.Bot: HighZeroBits)((e, assign) =>
      (e, assign) match {
        case (HighZeroBits.Bot, LocalAssign(_, ZeroExtend(i, lhs), _)) => HighZeroBits.Bits(i)
        case (b @ HighZeroBits.Bits(ei), LocalAssign(_, ZeroExtend(i, _), _)) if i == ei => b
        case (b @ HighZeroBits.Bits(ei), LocalAssign(_, ZeroExtend(i, _), _)) if i != ei => HighZeroBits.False
        case (b @ HighZeroBits.Bits(ei), m: MemoryLoad) => HighZeroBits.False
        case (b @ HighZeroBits.Bits(ei), m: DirectCall) => HighZeroBits.False
        case (HighZeroBits.False, _) => HighZeroBits.False
        case (_, other) => HighZeroBits.False
      }
    )
    (v, allRHSExtended)
  )
  val varsWithExtend: Map[LocalVar, HighZeroBits] = assignments
    .map((lhs, _) => {
      // map all lhs to the result for their representative
      val rep = ufsolver.find(LVTerm(lhs)) match {
        case LVTerm(r) => r
        case _ => ??? /* unreachable */
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
          ChangeTo(LocalVar(v.varName, BitVecType(size(v).get - varHighZeroBits(v)), v.index))
        }
        case _ => DoChildren()
      }
    }
    override def vstmt(s: Statement) = {
      s match {
        case a @ LocalAssign(lhs: LocalVar, ZeroExtend(sz, rhs), _)
            if size(lhs).isDefined && varHighZeroBits.contains(lhs) => {
          debugAssert(varHighZeroBits(lhs) == sz)
          a.lhs = LocalVar(lhs.varName, BitVecType(size(lhs).get - varHighZeroBits(lhs)), lhs.index)
          a.rhs = rhs
          DoChildren()
        }
        case _ => DoChildren()
      }
    }
  }
  visit_proc(ReplaceAlwaysSlicedVars(toSmallen), p)
}

def getRedundantAssignments(procedure: Procedure): Set[Variable] = {

  /** Get all assign statements which define a variable never used, assuming ssa form and proc parameters so that
    * interprocedural check is not required.
    */

  enum VS:
    case Bot
    case Assigned(definition: Set[Assign])
    case Read(definition: Set[Assign], uses: Set[CFGPosition])

  def joinVS(a: VS, b: VS) = {
    (a, b) match {
      case (VS.Bot, o) => o
      case (o, VS.Bot) => o
      case (VS.Read(d, u), VS.Read(d1, u1)) => VS.Read(d ++ d1, u ++ u1)
      case (VS.Assigned(d), VS.Read(d1, u1)) => VS.Read(d ++ d1, u1)
      case (VS.Read(d1, u1), VS.Assigned(d)) => VS.Read(d ++ d1, u1)
      case (VS.Assigned(d1), VS.Assigned(d2)) => VS.Assigned(d1 ++ d2)
    }
  }

  val assignedNotRead = mutable.Map[Variable, VS]().withDefaultValue(VS.Bot)

  for (c <- procedure) {
    c match {

      case a: LocalAssign => {
        assignedNotRead(a.lhs) = joinVS(assignedNotRead(a.lhs), VS.Assigned(Set(a)))
        a.rhs.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(a)))
        })
      }
      case a: MemoryAssign => {
        assignedNotRead(a.lhs) = joinVS(assignedNotRead(a.lhs), VS.Assigned(Set(a)))
        a.rhs.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(a)))
        })
      }
      case m: SimulAssign => {
        m.assignments.foreach { case (lhs, r) =>
          assignedNotRead(lhs) = joinVS(assignedNotRead(lhs), VS.Assigned(Set(m)))
        }
        m.assignments.toSeq
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
          })
      }
      case a: MemoryLoad => {
        assignedNotRead(a.lhs) = joinVS(assignedNotRead(a.lhs), VS.Assigned(Set(a)))
        a.index.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(a)))
        })
      }
      case m: MemoryStore => {
        m.index.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        })
        m.value.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        })
      }
      case m: IndirectCall => {
        assignedNotRead(m.target) = joinVS(assignedNotRead(m.target), VS.Read(Set(), Set(m)))
      }
      case m: Assert => {
        m.body.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        })
      }
      case m: Assume => {
        for (v <- m.body.variables) {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        }
      }
      case c: DirectCall => {
        c.actualParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(c)))
          })
      }
      case p: Return => {
        p.outParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(p)))
          })
      }
      case p: GoTo => ()
      case p: NOP => ()
      case p: Unreachable => ()
      case p: Procedure => ()
      case b: Block => ()
    }
  }

  var toRemove = assignedNotRead.filter(_._2.isInstanceOf[VS.Assigned])
  var removeOld = toRemove

  toRemove.map(_._1).toSet
}

class CleanupAssignments() extends CILVisitor {
  var deadVariables = Set[Variable]()
  var modified = false

  // def isRedundant(a: LocalAssign) = {
  //  a.lhs == a.rhs || redundantAssignments.contains(a)
  // }

  override def vproc(p: Procedure) = {
    modified = false
    deadVariables = getRedundantAssignments(p)
    DoChildren()
  }

  override def vstmt(s: Statement) = {
    var didAny = true
    val action: VisitAction[List[Statement]] = s match {
      case SimulAssign(assignments, _) if assignments.forall { case (lhs, rhs) =>
            deadVariables.contains(lhs)
          } =>
        ChangeTo(List())
      case a: SimulAssign =>
        val ns = a.assignments.filterNot { case (lhs, rhs) =>
          deadVariables.contains(lhs)
        }
        if (ns.size != a.assignments.size) {
          a.assignments = ns
          if a.assignments.isEmpty then ChangeTo(List()) else SkipChildren()
        } else {
          didAny = false
          SkipChildren()
        }
      case _ =>
        didAny = false
        SkipChildren()
    }
    modified = modified || didAny
    action
  }

  def transform(p: Procedure): Unit = {
    modified = true
    while (modified) {
      modified = false
      visit_proc(this, p)
    }
  }

  def transform(p: Program): Unit = {
    for (p <- p.procedures) {
      transform(p)
    }
  }

}

/**
 * Propagate flag calculation into this assume statement, finding a linear
 * backwards chain from this block until the first side-effecting or non-linear branch
 * and inlines the meet of all the assignments.
 *
 * Doesn't preserve assignment order or reason about clobbers so requires DSA form to be valid
 * and to correctly identify the linear backwards dependency.
 *
 * TODO: would be much more efficient if we memoised, as we are finding and evaluating
 * the backwards path twice for each branch.
 */
def inlineCond(a: Assume): Option[Expr] = boundary {

  condPropDebugLogger.debug(s"START : ${a.parent.label}")

  var cond = a.body
  var block = a.parent

  def goodSubst(v: Variable) = {
    v.name.startsWith("Cse")
    || v.name.startsWith("ZF")
    || v.name.startsWith("VF")
    || v.name.startsWith("CF")
    || v.name.startsWith("NF")
  }
  val interesting = a.body.variables.filter(goodSubst)
  if (interesting.isEmpty) {
    break(None)
  }

  var worklist = List(List(block))

  def processChain(incoming: Map[Variable, Expr])(bs: List[Block]) = boundary {
    var st = incoming

    for (s <- Vector.from(bs).flatMap(_.statements).reverseIterator) {
      s match {
        case assign: LocalAssign => {
          st = st.filterNot(_._2.variables.contains(assign.lhs)).updated(assign.lhs, assign.rhs)
        }
        case assign: MemoryLoad => {
          st = st.filterNot(_._2.variables.contains(assign.lhs))
        }
        case n: MemoryStore => ()
        case n: Assume => ()
        case n: Assume => ()
        case n: Assert => ()
        case n: NOP => ()
        case _ => break((st, false))
      }
    }
    (st, true)
  }

  var first = true
  var currJoin = Map[Variable, Expr]()
  var abortNow = false
  var seen = 1
  currJoin = boundary {
    while (worklist.nonEmpty) {
      condPropDebugLogger.debug(currJoin)

      val strata = worklist.head.map(findSimpleBackwardsChain)

      worklist = worklist.tail
      val extra = interesting.flatMap(currJoin.get(_).toSet.flatMap(_.variables))
      val extra2 = extra.flatMap(currJoin.get(_).toSet.flatMap(_.variables))

      val nextJoin = strata match {
        case (join :: _) :: Nil => join
        case (join :: _) :: tail if (tail.forall(_.headOption.contains(join))) => join
        case xs => {
          condPropDebugLogger.debug(s"Abort nonsame or empty head\n ${xs.map(_.headOption.map(_.label)).toList} ")
          break(currJoin)
        }
      }
      condPropDebugLogger.debug(s"Next join: ${nextJoin.label}")

      seen += strata.map(_.size).sum

      val res = strata.map(processChain(currJoin))
      condPropDebugLogger.debug(s"Res ${res.size} ${strata.map(_.map(_.label)).mkString("\n  ")}")

      currJoin = if (res.size == 1) {
        res.head._1
      } else if (res.forall(_._2)) {
        res
          .map(_._1)
          .reduce((acc, r) => {
            acc.keySet.intersect(r.keySet).filter(k => acc(k) == r(k)).map(k => k -> acc(k)).toMap
          })
      } else {
        condPropDebugLogger.debug(s"Abort found side effect:\n $res")
        break(currJoin)
      }

      if (seen > 20) {
        condPropDebugLogger.debug("Abort depth")
        break(currJoin)
      }

      if (!nextJoin.isLoopHeader()) {
        worklist = (nextJoin.prevBlocks.toList) :: worklist
      } else {
        condPropDebugLogger.debug("Abort loop header")
      }

    }

    currJoin
  }

  condPropDebugLogger.debug(s"subst $currJoin")
  cond = Substitute(currJoin.get, true)(cond).getOrElse(cond)
  condPropDebugLogger.debug(cond)

  Some(cond)
}

def collectUses(p: Procedure): Map[Variable, Set[Command]] = {
  val as = p.flatMap {
    case a: Command => freeVarsPos(a).map((_, a))
    case _ => Seq()
  }

  as.groupBy(_._1).map((v, r) => v -> r.map(_._2).toSet).toMap
}

def allDefinitions(p: Procedure): Map[Variable, Set[Assign]] = {
  p.collect { case a: Assign =>
    a.assignees.map(l => l -> a)
  }.flatten
    .groupBy(_._1)
    .map { case (v, ass) =>
      v -> ass.map(_._2).toSet
    }
}

class GuardVisitor(validate: Boolean = false) extends CILVisitor {

  /**
   *
   * This takes all variables in guards and substitutes them for their definitions IFF they have
   * exactly one definition (Assuming DSA form).
   *
   * Due to dsa form this heuristic alone should prevent any copies across loop iterations / clobbers etc
   * because transitively all definitions should dominate the use (the assume statement) we are propagating to.
   *
   * The [[validate]] parameter further checks this is the case by checking the dsa property is preserved
   * by the propagation, by checking the reaching-definitions set at the use site.
   *
   * This possibly relies on an implementation detail of DSA that it introduces enough clones
   * only at the beginning and very end of loops such that any loop exists can have any dependency on the
   * original loop variables involved in cycles without breaking dsa form when these dependencies are propaated out of the loop.
   * Unclear if the original DSA invariant is sufficient for this,
   * i.e. constructing a valid dsa program:
   *
   *      x0 := y
   *        |
   *        v
   *    +->
   *    |  CF := f(x0)
   *    |  x0 := x0 + 1
   *    |  |  \
   *    ---+   z := CF
   *
   *  Propagating CF to z (z := f(x0)) would break dsa form. However in practice we will duplicate x0
   *  and place a copy on the back edge so this contrived structure can't be created.
   *
   */

  var defs = Map[Variable, Set[Assign]]()

  def goodSubst(v: Variable) = {
    v.name.startsWith("Cse")
    || v.name.startsWith("ZF")
    || v.name.startsWith("VF")
    || v.name.startsWith("CF")
    || v.name.startsWith("NF")
  }

  // for validation
  var reachingDefs: Map[Command, Map[Variable, Set[Assign]]] = Map()
  var inparams = Set[Variable]()

  def getLiveVars(p: Procedure): (Map[Block, Set[Variable]], Map[Block, Set[Variable]]) = {
    val liveVarsDom = LiveVarsDomWatchFlags(defs)
    val liveVarsSolver = worklistSolver(liveVarsDom)
    liveVarsSolver.solveProc(p, backwards = true)
  }

  override def vproc(p: Procedure) = {
    defs = allDefinitions(p)
    inparams = p.formalInParam.toSet[Variable]
    reachingDefs = if validate then basicReachingDefs(p, getLiveVars(p)) else Map()
    DoChildren()
  }

  def substitute(pos: Command)(v: Variable): Option[Expr] = {
    if (goodSubst(v)) {
      val res = defs.get(v).getOrElse(Set())

      def propOK(e: Expr) = {
        // all dependent variables satisfy dsa property for the use we want to propage to
        // DSA property: our reaching definitions are all definitions of the variable
        e.variables.map(v => v -> reachingDefs(pos).get(v)).forall {
          case (v, d) => {
            (inparams.contains(v) && defs.get(v).forall(_.isEmpty))
            || (d.isDefined && d.get.toSet == defs.get(v).toSet.flatten)
          }
        }
      }

      if (res.size == 1) {
        res.head match {
          case SimulAssign(assignments, _) => {
            val (lhs, rhs) = assignments.find(_._1 == v).get
            if (validate) {
              debugAssert(propOK(rhs))
            }
            Some(rhs)
          }
          case o => {
            None
          }
        }
      } else {
        None
      }
    } else {
      None
    }
  }

  override def vstmt(s: Statement) = s match {
    case a @ Assume(body, b, c, d) if a.body.variables.exists(goodSubst) =>
      Substitute(substitute(s))(a.body) match {
        case Some(cond) => {
          a.body = cond
          SkipChildren()
        }
        case _ => SkipChildren()
      }
    case _ => SkipChildren()

  }

}

def simplifyCFG(p: Procedure) = {
  while (coalesceBlocks(p)) {}
  removeEmptyBlocks(p)
}

def copypropTransform(
  p: Procedure,
  procFrames: Map[Procedure, Set[Memory]],
  funcEntries: Map[BigInt, Procedure],
  constRead: (BigInt, Int) => Option[BitVecLiteral]
) = {
  val t = util.PerformanceTimer(s"simplify ${p.name} (${p.blocks.size} blocks)")
  // SimplifyLogger.info(s"${p.name} ExprComplexity ${ExprComplexity()(p)}")
  // val result = solver.solveProc(p, true).withDefaultValue(dom.bot)

  AlgebraicSimplifications(p)
  OffsetProp.transform(p)

  simplifyCFG(p)

  transforms.fixupGuards(p)
  val gvis = GuardVisitor(ir.eval.SimplifyValidation.validate)
  visit_proc(gvis, p)
  AlgebraicSimplifications(p)
  AssumeConditionSimplifications(p)
  ir.eval.cleanupSimplify(p)
  transforms.removeDuplicateGuard(p.blocks.toSeq)

  val xf = t.checkPoint("transform")

  CleanupAssignments().transform(p)
  t.checkPoint("redundant assignments")

  AlgebraicSimplifications(p)
  val sipm = t.checkPoint("algebraic simp")

  removeSlices(p)
  ir.eval.cleanupSimplify(p)
  AlgebraicSimplifications(p)

}

def removeEmptyBlocks(proc: Procedure): Unit = {
  val blocks = proc.blocks.toList
  for (b <- blocks) {
    b match {
      case b: Block if b.statements.size == 0 && b.prevBlocks.size == 1 && b.jump.isInstanceOf[GoTo] => {
        val prev = b.prevBlocks
        val next = b.nextBlocks
        for (p <- prev) {
          p.jump match {
            case g: GoTo => {
              for (n <- next) {
                g.addTarget(n)
              }
              g.removeTarget(b)
            }
            case _ => throw Exception("Must have goto")
          }
        }
        b.replaceJump(Unreachable())
        b.parent.removeBlocks(b)
      }
      case _ => ()
    }
  }
}

def removeEmptyBlocks(p: Program): Unit = {
  for (proc <- p.procedures) {
    removeEmptyBlocks(proc)
  }
}

/**
 * If we have two consecutive branches with a join between, duplicate the join for each
 * side of the first branch so that the subsequent analyses can differentiate dependency
 * behaviour of the second branch depending on which of the first branches was taken.
 */
def coalesceBlocksCrossBranchDependency(p: Program): Boolean = {
  val candidate = p.procedures.flatMap(_.blocks).collect {
    case b if b.statements.isEmpty && b.prevBlocks.size == 2 && b.nextBlocks.size == 2 => b
  }

  for (b <- candidate) {
    val left = b.nextBlocks.head
    val right = b.nextBlocks.last

    val proc = b.parent
    val nb = Block(b.label + "_disambiguate")
    proc.addBlock(nb)

    b.jump.asInstanceOf[GoTo].removeTarget(right)
    nb.replaceJump(GoTo(right))

    for (ic <- b.prevBlocks) {
      ic.jump.asInstanceOf[GoTo].addTarget(nb)
    }
  }
  candidate.nonEmpty
}

def coalesceBlocks(proc: Procedure): Boolean = {
  var didAny = false
  val blocks = proc.blocks.toList
  for (b <- blocks.sortBy(_.rpoOrder)) {
    if (
      b.prevBlocks.size == 1 && b.prevBlocks.head.statements.nonEmpty && b.statements.nonEmpty
      && b.prevBlocks.head.nextBlocks.size == 1
      && b.prevBlocks.head.statements.lastOption.forall(s => !s.isInstanceOf[Call])
      && !(b.parent.entryBlock.contains(b) || b.parent.returnBlock.contains(b))
      && b.atomicSection.isEmpty && b.prevBlocks.forall(_.atomicSection.isEmpty)
    ) {
      didAny = true
      // append topredecessor
      // we know prevBlock is only jumping to b and has no call at the end
      val prevBlock = b.prevBlocks.head
      val stmts = b.statements.map(b.statements.remove).toList
      prevBlock.statements.appendAll(stmts)
      // leave empty block b and cleanup with removeEmptyBlocks
    } else if (
      b.nextBlocks.size == 1 && b.nextBlocks.head.statements.nonEmpty && b.statements.nonEmpty
      && b.nextBlocks.head.prevBlocks.size == 1
      && b.statements.lastOption.forall(s => !s.isInstanceOf[Call])
      && !(b.parent.entryBlock.contains(b) || b.parent.returnBlock.contains(b))
      && b.atomicSection.isEmpty && b.nextBlocks.forall(_.atomicSection.isEmpty)
    ) {
      didAny = true
      // append to successor
      // we know b is only jumping to nextBlock and does not end in a call
      val nextBlock = b.nextBlocks.head
      val stmts = b.statements.map(b.statements.remove).toList
      nextBlock.statements.prependAll(stmts)
      // leave empty block b and cleanup with removeEmptyBlocks
    } else if (b.jump.isInstanceOf[Unreachable] && b.statements.isEmpty && b.prevBlocks.size == 1) {
      b.prevBlocks.head.replaceJump(Unreachable())
      b.parent.removeBlocks(b)
    }
  }
  didAny
}

def coalesceBlocks(p: Program): Boolean = {
  var didAny = false
  for (proc <- p.procedures) {
    didAny = didAny || coalesceBlocks(proc)
  }
  didAny
}

val coalesceBlocksOnce = Transform(
  "CoalesceBlocksOnce",
  (ctx, man) => {
    coalesceBlocks(ctx.program)
    man.ClobberAll
  }
)

val coalesceBlocksFixpoint = Transform(
  "CoalesceBlocksFixpoint",
  (ctx, man) => {
    while (coalesceBlocks(ctx.program)) {}
    man.ClobberAll
  }
)

def removeDeadInParams(p: Program): Boolean = {
  var modified = false
  debugAssert(invariant.correctCalls(p))

  for (
    block <- p.procedures.filterNot(_.isExternal.contains(true)).filterNot(p.mainProcedure == _).flatMap(_.entryBlock)
  ) {
    // FIXME: .filterNot(p.mainProcedure == _). is a bad hack to fix tests that refer to variabels in requries spec
    // that are not live in procedure. new spec should let us look at dependencies, or write spec about actual result
    val proc = block.parent

    val (liveBefore, _) = getLiveVars(proc)
    val live = liveBefore(block)
    val unused = proc.formalInParam.filterNot(live.contains(_))

    for (unusedFormalInParam <- unused) {
      modified = true
      proc.formalInParam.remove(unusedFormalInParam)

      for (call <- proc.incomingCalls()) {
        call.actualParams = call.actualParams.removed(unusedFormalInParam)
      }
    }
  }

  if (modified) debugAssert(invariant.correctCalls(p))
  modified
}

/*
 * Inline procedure output parameters where the procedure returns a constant or the same out param as the in param.
 *
 * Returns additional set newly inlined variables.
 */
def removeInvariantOutParameters(
  p: Program,
  alreadyInlined: Map[Procedure, Set[Variable]] = Map()
): Map[Procedure, Set[Variable]] = {
  debugAssert(invariant.correctCalls(p))
  debugAssert(invariant.singleCallBlockEnd(p))
  var modified = false
  var inlined = Map[Procedure, Set[Variable]]()

  val returns = p.procedures.flatMap(_.returnBlock).map(_.jump).collect { case r: Return => r }
  for (ret <- returns) {
    val proc = ret.parent.parent
    val inParams = proc.formalInParam.toSet

    val specDependencies: Set[Variable] = proc.ensuresExpr.flatMap(_.variables).toSet

    val doneAlready = alreadyInlined.getOrElse(proc, Set())
    var doneNow = Set[Variable]()
    var toRename = Map[Variable, LocalVar]()

    val invariantParams = ret.outParams.filterNot(x => specDependencies.contains(x._1)).collect {
      // we are returning a constant and can inline
      case (formalOut, binding: Literal) => (formalOut, binding)
      // we are returning the input parameter with the same name as the output parameter so can inline at callsite
      case (formalOut, binding: Expr) if binding.variables.forall {
            case l: LocalVar => inParams.contains(l)
            case _ => false
          } =>
        (formalOut, binding)
    }

    // remove invariant params from outparam signature, and outparam list of return, and out param list of all calls,
    // and add assignment after the call of bound actual param to bound outparam
    // TODO: dependency from specification into account when removing outparams
    val overApproxSpecDependency = ((0 to 7).map(n => LocalVar(s"R${n}_in", BitVecType(64))) ++ (0 to 7).map(n =>
      LocalVar(s"R${n}_out", BitVecType(64))
    )).toSet

    for ((invariantOutFormal, binding) <- invariantParams.filterNot((k, v) => doneAlready.contains(k))) {
      doneNow = doneNow + invariantOutFormal

      modified = true

      val remove = !overApproxSpecDependency.contains(invariantOutFormal)

      if (remove) {
        proc.formalOutParam.remove(invariantOutFormal)
        ret.outParams = ret.outParams.removed(invariantOutFormal)
      }

      val calls = proc.incomingCalls()

      for (call <- calls) {
        // substitute the call in params for
        val rhs = Substitute(
          (
            (v: Variable) =>
              v match {
                case l: LocalVar => call.actualParams.get(l)
                case _ => None
              }
          ),
          false
        )(binding).getOrElse(binding)

        // rename lhs to fresh ssa variable

        val lhs = call.outParams(invariantOutFormal)

        // if we are in dsa form, replace the return outparam with a new local so the
        // inlining does not break ssa form
        val callLHS = lhs match {
          case l: LocalVar if (l.index != 0) => {
            val newName = proc.getFreshSSAVar(l.varName + "_retval_inlined", l.getType)
            toRename = toRename + (l -> newName)
            newName
          }
          case o => o
        }
        call.outParams = call.outParams + (invariantOutFormal -> callLHS)

        if (remove) {
          call.outParams = call.outParams.removed(invariantOutFormal)
        }

        // insert assignment of to successor to maintain singleCallBlockEnd invariant
        // TODO: this would really be simpler if we just broke the singleCallBlockEnd invariant
        call.parent.jump match {
          case r: Return => {
            // substitute directly into return
            r.outParams =
              r.outParams.map((f, a) => (f, Substitute(v => if (v == lhs) then Some(rhs) else None)(a).getOrElse(a)))
          }
          case r: Unreachable => ()
          case g: GoTo => {

            val tgts = g.targets.toSet
            val label = g.parent.label + "_retval_inline"
            val b =
              if (g.targets.size == 1 && g.targets.forall(_.label == label)) then g.targets.head
              else {
                val b = Block(label)
                g.parent.parent.addBlock(b)
                g.parent.replaceJump(GoTo(b))
                b.replaceJump(GoTo(tgts))
              }

            b.statements.prepend(LocalAssign(lhs, rhs, Some("inlineret")))
          }
        }
      }
    }

    if (doneNow.nonEmpty) {
      inlined = inlined.updated(proc, doneNow)
    }
  }

  if (inlined.nonEmpty) {
    debugAssert(invariant.correctCalls(p))
    debugAssert(invariant.singleCallBlockEnd(p))
    applyRPO(p) /* Because we added blocks */
  }

  inlined
}

def cleanupBlocks(p: Program) = {
  var merged = true
  while (merged) {
    SimplifyLogger.debug("[!] Simplify :: Merge empty blocks")
    merged = coalesceBlocks(p)
    removeEmptyBlocks(p)
  }
}

def doCopyPropTransform(p: Program, rela: Map[BigInt, BigInt]) = {

  applyRPO(p)

  def isExternal(p: Procedure) = p.isExternal.contains(true) || p.blocks.isEmpty
  // assume some functions modify nothing
  def noModifies(p: Procedure) =
    p.procName match {
      case "strlen" | "assert" | "printf" | "__stack_chk_fail" | "__printf_chk" | "__syslog_chk" => true
      case _ => false
    }

  val procFrames = getProcFrame.solveInterproc(p)

  val addrToProc = p.procedures.toSeq.flatMap(p => p.address.map(addr => addr -> p).toSeq).toMap

  def read(addr: BigInt, size: Int): Option[BitVecLiteral] = {
    val rodata = p.initialMemory.filter((_, s) => s.readOnly)
    rodata.maxBefore(addr + 1) match {
      case None => None
      case (Some((k, s))) if s.canGetBytes(addr, size / 8) => {
        SimplifyLogger.debug(s"got [$addr..${addr + size}] from ${s.name} [${s.address}..${s.address + s.size}]")
        Some(
          s.getBytes(addr, size / 8)
            .reverse
            .foldLeft(BitVecLiteral(0, 0))((acc, r) => ir.eval.BitVectorEval.smt_concat(acc, r))
        )
      }
      case (Some((k, s))) => {
        // SimplifyLogger.debug(s"Cannot get [$addr..${addr + size}] from ${s.name} [${s.address}..${s.address + s.size}]")
        None
      }
    }
  }

  SimplifyLogger.info("[!] Simplify :: Expr/Copy-prop Transform")
  val work = p.procedures
    .filter(_.blocks.size > 0)
    .map(p =>
      p -> // Future
        {
          SimplifyLogger
            .debug(s"CopyProp Transform ${p.name} (${p.blocks.size} blocks, expr complexity ${ExprComplexity()(p)})")
          copypropTransform(p, procFrames, addrToProc, read)
        }
    )

  work.foreach((p, job) => {
    try {
      // Await.result(job, 10000.millis)
      job
    } catch {
      case e => {
        SimplifyLogger.error("Simplify :: CopyProp " + p.name + ": " + e.toString)
      }
    }
  })

  SimplifyLogger.info("[!] Simplify :: Dead variable elimination")

  // cleanup
  visit_prog(CleanupAssignments(), p)

  SimplifyLogger.info("[!] Simplify :: Merge empty blocks")
  cleanupBlocks(p)

  visit_prog(CopyProp.BlockyProp(), p)
  visit_prog(CleanupAssignments(), p)

}

def copyPropParamFixedPoint(p: Program, rela: Map[BigInt, BigInt]): Int = {
  SimplifyLogger.info(s"Simplify:: Copyprop iteration 0")
  doCopyPropTransform(p, rela)
  var inlinedOutParams: Map[Procedure, Set[Variable]] = removeInvariantOutParameters(p)
  var changed = inlinedOutParams.nonEmpty
  var iterations = 1
  val maxIterations = 2
  while (changed && iterations < maxIterations) {
    changed = false
    SimplifyLogger.info(s"Simplify:: Copyprop iteration $iterations")
    transforms.removeTriviallyDeadBranches(p)
    doCopyPropTransform(p, rela)
    val extraInlined = removeInvariantOutParameters(p, inlinedOutParams)
    inlinedOutParams = extraInlined.foldLeft(inlinedOutParams)((acc, v) =>
      acc + (v._1 -> (acc.getOrElse(v._1, Set[Variable]()) ++ v._2))
    )
    var deadIn = removeDeadInParams(p)

    while (removeDeadInParams(p)) {}

    changed = changed || extraInlined.nonEmpty || deadIn

    cleanupBlocks(p)
    iterations += 1
  }
  if (changed && iterations == maxIterations) {
    SimplifyLogger.info(s"Stopped at copyprop iteration bound: $maxIterations")
  }
  iterations
}

def reversePostOrder(p: Procedure): Unit = {
  /* Procedures may contain disconnected sets of blocks so we arbitrarily order these with respect to eachother. */
  for (b <- p.blocks) {
    b.rpoOrder = -1
  }
  var left = 0
  for (b <- p.blocks.filter(_.rpoOrder == -1)) {
    left = reversePostOrder(b, true, left) + 1
  }
  left = p.entryBlock.map(reversePostOrder(_, false, left)).getOrElse(0) + 1
}

def reversePostOrder(startBlock: Block, fixup: Boolean = false, begin: Int = 0): Int = {
  var count = begin
  val seen = mutable.HashSet[Block]()

  def walk(b: Block): Unit = {
    seen += b
    for (s <- b.nextBlocks) {
      if (!seen.contains(s)) {
        walk(s)
      }
    }
    if (!fixup || b.rpoOrder < count) {
      b.rpoOrder = count
    }
    count += 1
  }

  walk(startBlock)
  count
}

def applyRPO(p: Program) = {
  for (proc <- p.procedures) {
    reversePostOrder(proc)
  }
}

val applyRpoTransform = Transform(
  "ApplyRPO",
  (ctx, man) => {
    applyRPO(man.program)
    man.ClobberAll
  }
)

object getProcFrame {
  class GetProcFrame(frames: Procedure => Set[Memory]) extends CILVisitor {
    var modifies = Set[Memory]()

    override def vstmt(e: Statement) = e match {
      case s: MemoryStore => modifies = modifies + s.mem; SkipChildren()
      case d: DirectCall => modifies = modifies ++ frames(d.target); SkipChildren()
      case _ => SkipChildren()
    }

  }

  def solveProc(st: Procedure => Set[Memory], s: Set[Memory], p: Procedure): Set[Memory] = {
    val v = GetProcFrame(st)
    visit_proc(v, p)
    s ++ v.modifies
  }

  def solveInterproc(p: Program) = {
    val solver = BottomUpCallgraphWorklistSolver[Set[Memory]](solveProc, _ => Set[Memory]())
    solver.solve(p)

  }

}

object OffsetProp {

  /*
   * Copyprop for any expression of fitting into the structure
   *    bvadd(variable, constant)
   *
   * This is sufficient to propagate branch conditions through.
   */

  // None, None -> Top
  // Some(v), None -> v
  // Some(v), Some(Lit) -> v + Lit
  // None, Some(Lit) -> Lit
  type Value = (Option[Variable], Option[BitVecLiteral])

  def joinValue(l: Value, r: Value) = {
    (l, r) match {
      case ((None, None), _) => (None, None)
      case (_, (None, None)) => (None, None)
      case (l, r) if l != r => (None, None)
      case (l, r) => l
    }
  }

  class CopyProp() {
    val st = mutable.Map[Variable, Value]()
    var giveUp = false
    val lastUpdate = mutable.Map[Block, Int]()
    var stSequenceNo = 1

    def findOff(v: Variable, c: BitVecLiteral): BitVecLiteral | Variable | BinaryExpr = find(v) match {
      case lc: BitVecLiteral => ir.eval.BitVectorEval.smt_bvadd(lc, c)
      case lv: Variable => BinaryExpr(BVADD, lv, c)
      case BinaryExpr(BVADD, l: Variable, r: BitVecLiteral) =>
        BinaryExpr(BVADD, l, ir.eval.BitVectorEval.smt_bvadd(r, c))
      case _ => throw Exception("Unexpected expression structure created by find() at some point")
    }

    def find(v: Variable): BitVecLiteral | Variable | BinaryExpr = {
      st.get(v) match {
        case None => v
        case Some((None, None)) => v
        case Some((None, Some(c))) => c
        case Some((Some(v), None)) => find(v)
        case Some((Some(v), Some(c))) => findOff(v, c)
      }
    }

    def joinState(lhs: Variable, rhs: Expr) = {
      specJoinState(lhs, rhs) match {
        case Some((l, r)) => {
          if (st.contains(l) && st(l) != r) {
            stSequenceNo += 1
          }
          st(l) = r
        }
        case _ => ()
      }
    }

    def specJoinState(lhs: Variable, rhs: Expr): Option[(Variable, Value)] = {
      rhs match {
        case e @ BinaryExpr(BVADD, l: Variable, r: BitVecLiteral) if (!st.contains(lhs)) =>
          Some(lhs -> (Some(l), Some(r)))
        case e @ BinaryExpr(BVADD, l: Variable, r: BitVecLiteral) if findOff(l, r) == find(lhs) => None
        case v: Variable if (!st.contains(lhs)) => Some(lhs -> (Some(v), None))
        case v: BitVecLiteral if (!st.contains(lhs)) => Some(lhs -> (None, Some(v)))
        case v: Variable if (find(lhs) == find(v)) => None
        case c: BitVecLiteral if (find(lhs) != c) => Some(lhs -> (None, None))
        case _ => Some(lhs -> (None, None))
      }
    }

    def clob(v: Variable) = {
      st(v) = (None, None)
    }

    def transfer(s: Statement) = s match {
      case SimulAssign(assignments, _) =>
        assignments
          .flatMap {
            case (l: Variable, r: Variable) => specJoinState(l, r).toSeq
            case (l: Variable, r: Literal) => specJoinState(l, r).toSeq
            case (l: Variable, r @ BinaryExpr(BVADD, _: Variable, _: BitVecLiteral)) => specJoinState(l, r).toSeq
            case (l: Variable, _) => Seq(l -> (None, None))
          }
          .foreach { case (l, r) =>
            st(l) = r
          }
      case a: Assign => {
        // memoryload and DirectCall
        a.assignees.foreach(clob)
      }
      case _: MemoryStore => ()
      case _: NOP => ()
      case _: Assert => ()
      case _: Assume => ()
      case i: IndirectCall => giveUp = true
    }

    def analyse(p: Procedure): Map[Variable, Expr] = {
      reversePostOrder(p)
      val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
      worklist.addAll(p.entryBlock)
      while (worklist.nonEmpty && !giveUp) {
        val b = worklist.dequeue()
        val seq = lastUpdate.get(b).getOrElse(0)

        b.statements.foreach(transfer)

        if (stSequenceNo != seq || seq == 0) {
          lastUpdate(b) = stSequenceNo
          worklist.addAll(b.nextBlocks)
        }
      }

      val res: Map[Variable, Variable | Literal | BinaryExpr] =
        if giveUp then Map()
        else
          st.collect {
            case (v, (Some(v2), None)) => v -> find(v2)
            case (v, (None, Some(c))) => v -> c
            case (v, (Some(v2), Some(c))) => v -> findOff(v2, c)
          }.toMap

      res
    }

  }

  def transform(p: Procedure): Map[Variable, Expr] = {
    val solver = CopyProp()
    val res = solver.analyse(p)

    class SubstExprs(subst: Map[Variable, Expr]) extends CILVisitor {
      override def vexpr(e: Expr) = {
        Substitute(subst.get)(e) match {
          case Some(n) => ChangeTo(n)
          case _ => SkipChildren()
        }
      }
    }
    if (res.nonEmpty) {
      visit_proc(SubstExprs(res), p)
    }
    res
  }
}

object MinCopyProp {

  // None -> Top
  type Value = Option[Variable | Literal]

  class CopyProp() {
    val st = mutable.Map[Variable, Value]()
    val lastUpdate = mutable.Map[Block, Int]()
    var stSequenceNo = 1
    var giveUp = false

    def find(v: Variable): Literal | Variable = {
      var search: Variable = v

      boundary {
        while (st.contains(search)) {
          st(search) match {
            case None => break(search)
            case Some(c: Literal) => break(c)
            case Some(v: Variable) =>
              search = v
          }
        }
        search
      }
    }

    def specJoinState(lhs: Variable, rhs: Variable | Literal): Option[(Variable, Value)] = {
      rhs match {
        case v: Variable if (!st.contains(lhs)) => Some(lhs -> Some(v))
        case v: Literal if (!st.contains(lhs)) => Some(lhs -> Some(v))
        case v: Variable if (find(lhs) != find(v)) => Some(lhs -> None)
        case c: Literal if (find(lhs) != c) => Some(lhs -> None)
        case _ => None
      }
    }

    def joinState(lhs: Variable, rhs: Variable | Literal) = {
      specJoinState(lhs, rhs) match {
        case Some((l, r)) => {
          if (st.contains(l) && st(l) != r) {
            stSequenceNo += 1
          }
          st(l) = r
        }
        case _ => ()
      }
    }

    def clob(v: Variable) = {
      st(v) = None
    }

    def transfer(s: Statement) = s match {
      case LocalAssign(l: Variable, r: Variable, _) => joinState(l, r)
      case LocalAssign(l: Variable, r: Literal, _) => joinState(l, r)
      case LocalAssign(l: Variable, _, _) => clob(l)
      // case s: SimulAssign => s.assignments.flatMap {
      //   case (l: Variable, r: Variable) => specJoinState(l, r).toSeq
      //   case (l: Variable, r) => Seq(l -> None)
      // }.foreach {
      //   case (l, r) => st(l) = r
      // }
      case a: Assign => {
        // memoryload and DirectCall
        a.assignees.foreach(clob)
      }
      case _: MemoryStore => ()
      case _: NOP => ()
      case _: Assert => ()
      case _: Assume => ()
      case i: IndirectCall => giveUp = true
    }

    def analyse(p: Procedure): Map[Variable, Variable | Literal] = {
      reversePostOrder(p)
      val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
      worklist.addAll(p.entryBlock)
      while (worklist.nonEmpty && !giveUp) {
        val b = worklist.dequeue()
        val seq = lastUpdate.get(b).getOrElse(0)

        b.statements.foreach(transfer)

        if (stSequenceNo != seq || seq == 0) {
          lastUpdate(b) = stSequenceNo
          worklist.addAll(b.nextBlocks)
        }
      }

      val res: Map[Variable, Variable | Literal] =
        if giveUp then Map()
        else
          st.collect {
            case (v, Some(r: Variable)) => v -> find(r)
            case (v, Some(c: Literal)) => v -> c
          }.toMap

      res
    }
  }

  def transform(p: Procedure) = {
    val solver = CopyProp()
    val res = solver.analyse(p)

    class SubstExprs(subst: Map[Variable, Expr]) extends CILVisitor {
      override def vexpr(e: Expr) = {
        Substitute(subst.get)(e) match {
          case Some(n) => ChangeTo(n)
          case _ => SkipChildren()
        }
      }
    }
    if (res.nonEmpty) {
      visit_proc(SubstExprs(res), p)
    }

  }

}

object CopyProp {

  class BlockyProp(trivialOnly: Boolean = true, var transform: Boolean = true) extends CILVisitor {
    /* Flow-sensitive intra-block copyprop */

    var st = Map[Variable, Expr]()

    def replaceVar(lhs: Variable, rhs: Option[Expr] = None) = {
      st = st
        .filterNot { case (l, r) =>
          r.variables.contains(lhs) || l == lhs
        }
      rhs.foreach(nrhs => st = st.updated(lhs, nrhs))
    }

    def subst(e: Expr): Expr = {
      // val normed = normedST
      def getter(v: Variable) =
        st.get(v) match {
          case Some(n: Variable) if n != v => Some(n)
          case Some(n) if (!trivialOnly || isTrivial(n)) && !n.variables.contains(v) => Some(n)
          case _ => None
        }
      simplifyExprFixpoint(Substitute(getter, true)(e).getOrElse(e))(0)
    }

    override def vblock(b: Block) = {
      st = Map()
      DoChildren()
    }

    override def vstmt(s: Statement) = {
      s match {
        case l: LocalAssign => {
          val nrhs = subst(l.rhs)
          replaceVar(l.lhs, Some(nrhs))
          if transform then l.rhs = nrhs
          SkipChildren()
        }

        case l: SimulAssign => {
          val assignments = l.assignments.map { case (l, r) =>
            (l, subst(r))
          }
          assignments.foreach { case (l, r) =>
            st = st.updated(l, r)
          }
          l.assignments = assignments
          SkipChildren()
        }
        case x: Assert => {
          if transform then x.body = subst(x.body)
          SkipChildren()
        }
        case x: Assume => {
          if transform then x.body = subst(x.body)
          SkipChildren()
        }
        case x: DirectCall => {
          if transform then x.actualParams = x.actualParams.map((l, r) => (l, subst(r)))
          val lhs = x.outParams.map(_._2)
          lhs.foreach(replaceVar(_, None))
          SkipChildren()
        }
        case d: MemoryLoad => {
          if transform then d.index = subst(d.index)
          replaceVar(d.lhs, None)
          SkipChildren()
        }
        case d: MemoryStore => {
          if transform then d.index = subst(d.index)
          if transform then d.value = subst(d.value)
          SkipChildren()
        }
        case d: MemoryAssign => {
          if transform then d.rhs = subst(d.rhs)
          replaceVar(d.lhs, None)
          SkipChildren()
        }
        case x: IndirectCall => {
          st = Map()
          SkipChildren()
        }
        case _: NOP => SkipChildren()
      }
    }
  }

  case class PropState(var e: Expr, val deps: mutable.Set[Variable], var clobbered: Boolean, var useCount: Int)

  def clobberFull(c: mutable.HashMap[Variable, PropState], l: Variable): Unit = {
    clobberOne(c, l)
    for ((v, x) <- c) {
      if (x.deps.contains(l)) {
        clobberOne(c, v)
      }
    }
  }

  def clobberOne(c: mutable.HashMap[Variable, PropState], l: Variable): Unit = {
    val entry = c.getOrElseUpdate(l, PropState(FalseLiteral, mutable.Set(), true, 0))
    entry.clobbered = true
  }

  // TODO: consider copies where there is only one use trivial
  def isTrivial(e: Expr): Boolean = e match {
    case l: Literal => true
    case l: Variable => true
    case BinaryExpr(op, e: Variable, b: Literal) => true
    case ZeroExtend(_, e) if isTrivial(e) => true
    case SignExtend(_, e) if isTrivial(e) => true
    case Extract(_, _, e) if isTrivial(e) => true
    case UnaryExpr(_, e) if isTrivial(e) => true
    case _ => false
  }

  def canPropTo(s: mutable.HashMap[Variable, PropState], e: Expr): Option[(Expr, Set[Variable])] = {

    def proped(e: Expr) = {
      var deps = Set[Variable]() ++ e.variables
      val ne =
        if e.variables.size > 1 then Some(e)
        else
          Substitute(
            v => {
              s.get(v) match {
                case Some(vs) if !vs.clobbered => {
                  deps = deps ++ vs.deps + v
                  Some(vs.e)
                }
                case _ => None
              }
            },
            true
          )(e)

      // partial eval after prop
      (simplifyExprFixpoint(ne.getOrElse(e))._1, deps)
    }

    val (p, deps) = proped(e)
    p match {
      case l: Literal => Some(l, deps)
      case l: Variable => Some(l, deps)
      case e @ BinaryExpr(o, v: Variable, c: Literal) => Some(e, deps)
      case e: Expr if (e.variables.size < 2) => Some(e, deps)
      case _ => None
    }
  }

  def varForMem(m: Memory) = Register("symbolic_memory_var" + m.name, 1)
  def isMemVar(v: Variable) = v.name.startsWith("symbolic_memory_var")
  def varForLoadStore(m: Memory, addr: Expr, sz: Int) = {
    Register(m.name + "_symbolic_store_" + translating.PrettyPrinter.pp_expr(addr), sz)
  }

  def clobberAllMemory(c: mutable.HashMap[Variable, PropState]) = {
    c.keys.filter(isMemVar).foreach(v => clobberFull(c, v))
  }

  def DSACopyProp(
    p: Procedure,
    procFrames: Map[Procedure, Set[Memory]],
    funcEntries: Map[BigInt, Procedure],
    constRead: (BigInt, Int) => Option[BitVecLiteral]
  ) = {
    val updated = false
    val state = mutable.HashMap[Variable, PropState]()
    var poisoned = false // we have an indirect call

    // This is obviously invalid to do flow-insensitively as we don't have
    // a DSA form on memory imposing the order of loads/stores

    def transfer(c: mutable.HashMap[Variable, PropState], s: Statement): Unit = {
      // val callClobbers = ((0 to 7) ++ (19 to 30)).map("R" + _).map(c => Register(c, 64))

      s match {
        case l: MemoryStore => {
          ()
        }
        case l: MemoryLoad => {
          clobberFull(c, l.lhs)
        }
        case l: MemoryAssign => {
          clobberFull(c, l.lhs)
        }
        case s: SimulAssign =>
          s.assignments.foreach {
            case (l, r) => {
              var prop = canPropTo(c, r)
              val existing = c.get(l)

              val toDo = (prop, existing) match {
                case (Some(evaled, deps), None) => {
                  Seq(l -> PropState(evaled, mutable.Set.from(deps), false, 0))
                }
                case (_, Some(ps)) if ps.clobbered => {
                  Seq()
                }
                case (Some(evaled, deps), Some(ps))
                    if ps.e == r || ps.e == evaled || (canPropTo(c, ps.e).contains(evaled)) => {
                  Seq(l -> c(l).copy(e = evaled, deps = c(l).deps ++ deps))
                }
                case _ => {
                  // ps.e != evaled and have prop
                  Seq(l -> "clobber")
                }
              }
              toDo.foreach {
                case (l, n: PropState) => c(l) = n
                case (l, "clobber") => clobberFull(c, l)
                case _ => ???
              }
            }
          }
        case LocalAssign(l, r, lb) => {

          var prop = canPropTo(c, r)
          val existing = c.get(l)

          (prop, existing) match {
            case (Some(evaled, deps), None) => {
              c(l) = PropState(evaled, mutable.Set.from(deps), false, 0)
            }
            case (_, Some(ps)) if ps.clobbered => {
              ()
            }
            case (Some(evaled, deps), Some(ps))
                if ps.e == r || ps.e == evaled || (canPropTo(c, ps.e).contains(evaled)) => {
              c(l).e = evaled
              c(l).deps.addAll(deps)
            }
            case _ => {
              // ps.e != evaled and have prop
              clobberFull(c, l)
            }
          }
        }
        case x: DirectCall => {
          val lhs = x.outParams.map(_._2)
          for (l <- lhs) {
            clobberFull(c, l)
          }
        }

        case x: IndirectCall => {
          // need a reaching-defs to get inout args (just assume register name matches?)
          // this reduce we have to clobber with the indirect call this round
          poisoned = true
          val r = for {
            (addr, deps) <- canPropTo(c, x.target)
            addr <- addr match {
              case b: BitVecLiteral => Some(b.value)
              case _ => None
            }
            proc <- funcEntries.get(addr)
          } yield (proc, deps)

          r match {
            case Some(target, deps) => {
              SimplifyLogger.info("Resolved indirect call")
            }
            case None => {
              for ((i, v) <- c) {
                v.clobbered = true
              }
              poisoned = true
            }
          }

        }
        case _ => ()
      }
    }

    // sort by precedence
    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
    worklist.addAll(p.blocks)

    while (worklist.nonEmpty && !poisoned) {
      val b: Block = worklist.dequeue
      for (l <- b.statements) {
        transfer(state, l)
      }
    }

    val trivialOnly = false

    if (!poisoned) state else mutable.HashMap()
  }

  def toResult(s: mutable.Map[Variable, PropState])(trivialOnly: Boolean = true)(v: Variable): Option[Expr] = {
    s.get(v) match {
      case Some(c) if !c.clobbered && (!trivialOnly || isTrivial(c.e)) => Some(c.e)
      case _ => None
    }
  }

}

/** Use this to count the number of subexpressions in a basil-ir expression
  */
class ExprComplexity extends CILVisitor {
  // count the nodes in the expression AST
  var count = 0
  var stmt = 0
  override def vexpr(e: Expr) = {
    count += 1
    DoChildren()
  }

  override def vstmt(s: Statement) = {
    stmt += 1
    DoChildren()
  }

  def apply(e: Procedure) = {
    count = 0
    visit_proc(this, e)
    count
  }

  def stmtCount(p: Procedure) = {
    stmt = 0
    visit_proc(this, p)
    stmt
  }

  def apply(e: Expr) = {
    count = 0
    visit_expr(this, e)
    count
  }
}

/** Use this as a partially applied function. Substitute(Map.from(substs).get, recurse = false)
  *
  * @param res
  *   defines the substitutions to make
  * @param recurse
  *   continue substituting with `res` into each substituted expression
  * @param complexityThreshold
  *   Stop substituting after the AST node count has increased by this much
  *
  *  TODO: The recursive substitution here is a broken when res produces substitution loops.
  *  I want a substituter that takes a set of replacements to a canonical
  *  closure under substitution, but is more subtle to implement than it initially appears.
  *  One possibility is a union find on the set of rewrites to partition cycles it into canonical representative
  *  expressions, but generally we want analyses to establish a correct terminating rewrite rather than post-process to
  *  achieve it. I.e. stack overflows here are usually errors in the analysis used to produce the substitution [[res]].
  */
class Substitute(val res: Variable => Option[Expr], val recurse: Boolean = true, val complexityThreshold: Int = 0)
    extends CILVisitor {
  var madeAnyChange = false
  var complexity = 0
  var seen = Map[Variable, Variable]()

  override def vexpr(e: Expr) = {
    e match {
      case v: Variable if res(v).isDefined => {
        val changeTo = res(v).get
        if (complexityThreshold > 0) {
          complexity += ExprComplexity()(changeTo) - ExprComplexity()(e)
        }
        if (complexityThreshold > 0 && complexity > complexityThreshold) {
          SkipChildren()
        } else if (recurse) {
          madeAnyChange = true

          var newChange = changeTo
          var madeNewChange = true

          while (newChange.isInstanceOf[Variable] && madeNewChange) do {
            res(newChange.asInstanceOf[Variable]) match {
              case Some(v) =>
                newChange = v
                madeNewChange = true
              case _ =>
                madeNewChange = false
            }
          }

          ChangeDoChildrenPost(newChange, x => x)
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

enum PathExit:
  case Maybe
  case Return
  case NoReturn
  case Bot

object PathExit:
  def join(l: PathExit, r: PathExit) = (l, r) match {
    case (PathExit.Return, PathExit.Return) => PathExit.Return
    case (PathExit.NoReturn, PathExit.NoReturn) => PathExit.NoReturn
    case (PathExit.Return, PathExit.NoReturn) => PathExit.Maybe
    case (PathExit.NoReturn, PathExit.Return) => PathExit.Maybe
    case (PathExit.Maybe, x) => PathExit.Maybe
    case (x, PathExit.Maybe) => PathExit.Maybe
    case (PathExit.Bot, x) => x
    case (x, PathExit.Bot) => x
  }

class ProcExitsDomain(is_nonreturning: String => Boolean) extends AbstractDomain[PathExit] {
  /* backwards analysis to identify non-returning function */

  override def transfer(s: PathExit, b: Command) = {
    b match {
      case d: DirectCall if is_nonreturning(d.target.name) => PathExit.NoReturn
      case r: Return => PathExit.Maybe
      case _ => s
    }
  }
  override def top = PathExit.Maybe
  def bot = PathExit.Bot
  def join(l: PathExit, r: PathExit, pos: Block) = PathExit.join(l, r)
}

case class ProcReturnInfo(returning: Set[Procedure], nonreturning: Set[Procedure]) {
  override def toString =
    s"returning : ${returning.map(_.name).toList.sorted}\nnonretruning: ${nonreturning.map(_.name).toList.sorted}"
}

class DefinitelyExits(knownExit: Set[Procedure]) extends ProcedureSummaryGenerator[PathExit, PathExit] {
  def top: ir.transforms.PathExit = ???
  def bot: ir.transforms.PathExit = PathExit.Bot
  override def init(p: Procedure) = if p.procName == "exit" then PathExit.NoReturn else PathExit.Bot
  def join(l: PathExit, r: PathExit, p: Procedure) = PathExit.join(l, r)

  def transfer(a: ir.transforms.PathExit, b: ir.Procedure): ir.transforms.PathExit = ???

  /**
   *  Join the summary `summaryForTarget` for a call `p` into the local abstract state `l`.
   */
  def localTransferCall(
    l: ir.transforms.PathExit,
    summaryForTarget: ir.transforms.PathExit,
    p: ir.DirectCall
  ): ir.transforms.PathExit = (l, summaryForTarget) match {
    case (PathExit.Return, PathExit.Return) => PathExit.Return
    case (_, PathExit.NoReturn) => PathExit.NoReturn
    case (o, _) => o
  }

  /**
   * Return the new updated summary for a procedure based on the results of a dataflow analysis of that procedure.
   */
  def updateSummary(
    prevSummary: ir.transforms.PathExit,
    p: ir.Procedure,
    resBefore: Map[ir.Block, ir.transforms.PathExit],
    resAfter: Map[ir.Block, ir.transforms.PathExit]
  ): ir.transforms.PathExit = {
    p.entryBlock.flatMap(resBefore.get) match {
      case Some(PathExit.NoReturn) => PathExit.NoReturn
      case Some(PathExit.Return) => PathExit.Return
      case Some(PathExit.Maybe) => PathExit.Maybe
      case _ => prevSummary
    }
  }
}

def findDefinitelyExits(p: Program): ProcReturnInfo = {
  val exit = p.procedures.filter(p => p.procName == "exit").toSet
  val dom = DefinitelyExits(exit)
  val ldom = ProcExitsDomain(x => false)
  val solve = interprocSummaryFixpointSolver(ldom, dom)
  val res = solve.solveProgInterProc(p, true)
  ProcReturnInfo(
    res.collect { case (p, PathExit.Return) =>
      p
    }.toSet,
    res.collect { case (p, PathExit.NoReturn) =>
      p
    }.toSet
  )
}

val replaceJumpsInNonReturningProcs = Transform(
  "ReplaceJumpsInNonReturningProcs",
  (ctx, man) => {
    val nonReturning = findDefinitelyExits(ctx.program)
    ctx.program.mainProcedure.foreach {
      case d: DirectCall if nonReturning.nonreturning.contains(d.target) => d.parent.replaceJump(Return())
      case _ =>
    }
    man.ClobberAll
  }
)

val removeExternalFunctionReferences = Transform(
  "RemoveExternalFunctionReferences",
  (ctx, man) => {
    val externalNames = ctx.externalFunctions.map(_.name)
    val unqualifiedNames = externalNames.filter(_.contains('@')).map(_.split('@')(0))
    removeBodyOfExternal(externalNames ++ unqualifiedNames)(ctx.program)
    for (p <- ctx.program.procedures) {
      p.isExternal = Some(
        ctx.externalFunctions.exists(e => e.name == p.procName || p.address.contains(e.offset)) || p.isExternal
          .getOrElse(false)
      )
    }
    man.ClobberAll
  }
)

def getDoCleanupTransform(doSimplify: Boolean): Transform = TransformBatch(
  "DoCleanup",
  List(
    makeProcEntriesNonLoops,
    // useful for ReplaceReturns
    // (pushes single block with `Unreachable` into its predecessor)
    coalesceBlocksFixpoint,
    applyRpoTransform,
    replaceJumpsInNonReturningProcs,
    getEstablishProcedureDiamondFormTransform(doSimplify),
    removeExternalFunctionReferences
  ),
  notice = "Removing external function calls", // fixme: is this all the cleanup is doing?
  postRunChecks = ctx => {
    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    assert(invariant.procEntryNoIncoming(ctx.program))
  }
)

// these are called a lot so it's useful to create them here rather than generating many copies on the fly
lazy val doCleanupWithSimplify = getDoCleanupTransform(true)
lazy val doCleanupWithoutSimplify = getDoCleanupTransform(false)

class Simplify(val res: Boolean => Variable => Option[Expr], val initialBlock: Block = null) extends CILVisitor {

  var madeAnyChange = false
  var block: Block = initialBlock
  var skipped = Set[String]()
  var trivialOnly = true

  override def vstmt(s: Statement) = {
    s match {
      case _: Assume => {
        trivialOnly = false
      }
      case LocalAssign(_, rhs: Variable, _) => {
        trivialOnly = true
      }
      case _ => {
        trivialOnly = true
      }
    }
    DoChildren()
  }

  override def vexpr(e: Expr) = {
    val threshold = 500
    val variables = e.variables.toSet
    val subst = Substitute(res(trivialOnly), true, threshold)
    var old: Expr = e
    var result = subst(e).getOrElse(e)
    var limit = 0
    while (old != result && limit < 5) {
      limit += 1
      old = result
      result = subst(e).getOrElse(e)
    }

    if (subst.complexity > threshold) {
      val bl = s"${block.parent.name}::${block.label}"
      if (!skipped.contains(bl)) {
        skipped = skipped + bl
        SimplifyLogger.warn(s"Some skipped substitution at $bl due to resulting expr size > ${threshold} threshold")
      }
    }
    ChangeDoChildrenPost(result, x => x)
  }

  override def vblock(b: Block) = {
    block = b
    DoChildren()
  }
}

def fixupGuards(p: Program): Unit = {
  p.procedures.foreach(fixupGuards)
}

def fixupGuards(p: Procedure): Unit = {
  // the DSA transform can insert phi nodes on edges between a nondet branch and the block containing the guard
  // This breaks the interpreter because it can't immediately evaluate the guard.

  // This fixup clones assumes back to the statement immediately after the

  val concerning = p.collect {
    case b: Block if b.nextBlocks.size > 1 =>
      b.nextBlocks.filterNot(succ => IRWalk.firstInBlock(succ).isInstanceOf[Assume])
  }.flatten

  // traverse a straight-line path to find an assume, while evaluating the code
  def findAssume(b: Block): Option[Assume] = boundary {
    var visited = List[LocalAssign | SimulAssign]()
    var joinCount = 0
    var seenBlocks = Set[Block]()

    var block = b
    while (true) {

      seenBlocks = seenBlocks + b

      for (s <- block.statements) {
        s match {
          case l: LocalAssign => visited = l :: visited
          case l: SimulAssign => visited = l :: visited
          case Assume(oBody, _, b, c) => {
            var body = oBody
            for (assign <- visited) {
              body = Substitute(v =>
                if (!assign.assignees.contains(v)) then None
                else {
                  assign match {
                    case l: LocalAssign => Some(l.rhs)
                    case l: SimulAssign =>
                      l.assignments.collect {
                        case (lhs, r) if v == lhs => r
                      }.headOption
                  }
                }
              )(body).getOrElse(body)
            }
            break(Some(Assume(body, None, b, c)))
          }
          case n: NOP => ()
          case _ => break(None)
        }
      }

      if (block.nextBlocks.exists(b => b.prevBlocks.size > 1)) {
        joinCount += 1
      }

      if (block.nextBlocks.size != 1 || joinCount > 1 || seenBlocks.contains(block.nextBlocks.head)) {
        break(None)
      }

      block = block.nextBlocks.head
    }

    None
  }

  val propAssumes = concerning.map(bl => bl -> findAssume(bl))

  for ((bl, assume) <- propAssumes) {
    assume.foreach(bl.statements.prepend)
  }

}

def removeDuplicateGuard(b: Iterable[Block]): Unit = {
  b.foreach {
    case block: Block if IRWalk.firstInBlock(block).isInstanceOf[Assume] => {
      val assumes = block.statements.collect { case a: Assume =>
        a
      }.toList

      val chosen = assumes.head.body

      for (a <- assumes.tail) {
        if (a.body == TrueLiteral) {
          block.statements.remove(a)
        } else if (a.body == chosen) {
          block.statements.remove(a)
        }

      }
    }
    case _ => Seq()
  }
}

def removeDuplicateGuard(p: Program): Unit = {
  removeDuplicateGuard(p.procedures.flatMap(_.blocks).toSeq)
}

def findSimpleBackwardsChain(b: Block) = {
  var block = b

  var before = List[Block]()

  while {
    if (block.prevBlocks.size == 1) {
      block = block.prevBlocks.head
      before = block :: before
    }
    block.prevBlocks.size == 1 && block.prevBlocks.forall(_.nextBlocks.size == 1)
  } do {}

  before ++ Seq(b)
}

def findSimpleChain(b: Block) = {
  var block = b

  var before = List[Block]()
  var after = List[Block]()

  while (block.prevBlocks.size == 1 && block.prevBlocks.forall(_.nextBlocks.size == 1)) {
    block = block.prevBlocks.head
    before = block :: before
  }

  while (block.nextBlocks.size == 1 && block.nextBlocks.forall(_.prevBlocks.size == 1)) {
    block = block.nextBlocks.head
    after = block :: before
  }

  before ++ Seq(b) ++ after
}

def removeTriviallyDeadBranches(p: Program, removeAllUnreachableBlocks: Boolean = false): Boolean = {

  /**
   * This will remove branch on high checks, but since we can only eliminate branches
   * based on program constants (classified low), we can only realistically eliminate branches on low.
   */
  val dead = p.procedures.flatMap(_.blocks).map(IRWalk.firstInBlock(_)).collect {
    case a @ Assume(FalseLiteral, _, _, _) => a.parent
  }

  for (b <- dead) {
    if (b.hasParent) {
      val ch = findSimpleChain(b)
      val proc = b.parent

      proc.removeBlocksDisconnect(ch)

    }
  }

  if (removeAllUnreachableBlocks) {
    for (proc <- p.procedures) {
      val reachable = computeDomain(IntraProcBlockIRCursor, proc.entryBlock)
      for (b <- proc.blocks) {
        if (!reachable.contains(b)) {
          proc.removeBlocksDisconnect(b)
        }
      }
    }
  }

  applyRPO(p)

  dead.nonEmpty
}

val makeProcEntriesNonLoops = Transform(
  "MakeProcEntriesNonLoops",
  (ctx, man) => {
    ctx.program.procedures.foreach(p => {
      // ensure procedure entry has no incoming jumps, if it does replace with new
      // block jumping to the old procedure entry
      if (p.entryBlock.exists(_.prevBlocks.nonEmpty)) {
        val nb = Block(p.name + "_entry")
        p.addBlock(nb)
        val eb = p.entryBlock.get
        nb.replaceJump(GoTo(eb))
        p.entryBlock = nb
      }
    })
    man.ClobberAll
  }
)
