package analysis

import ir.*

type VarGammaMap = LatticeMap[Variable, LatticeSet[Variable]]

implicit val variableLatticeSetTerm: LatticeSet[Variable] = LatticeSet.Bottom()

/**
 * An abstract domain that determines for each variable, a set of variables whose gammas (at
 * the start of a procedure) are "affected" this variable's gamma. This is paramaterised by
 * whether we join or meet at branches to change whether this is a may or must analysis.
 *
 * At every program point, every variable's gamma is equal to the join of a set of input
 * variables (or memory values at procedure entry). Hence, there is a set S of these old
 * variables and memory locations that we can aim to compute. A may analysis of this domain
 * will overapproximate S, and a must analysis will underapproximate S.
 *
 * We have Gamma_x = Join(S), so if Join(S_o) is an overapproximation and Join(S_u) is an
 * underapproximation, we have the invariants that Gamma_x <= Join(S_o) and Join(S_u) <= Gamma_x.
 */
trait GammaDomain(initialState: VarGammaMap) extends PredMapDomain[Variable, LatticeSet[Variable]] {

  def transfer(m: VarGammaMap, c: Command): VarGammaMap = {
    c match {
      case c: LocalAssign =>
        m + (c.lhs -> c.rhs.variables.foldLeft(LatticeSet.Bottom[Variable]())((s, v) => s.union(m(v))))
      case c: SimulAssign =>
        m ++ c.assignments
          .map((lhs, rhs) => lhs -> rhs.variables.foldLeft(LatticeSet.Bottom[Variable]())((s, v) => s.union(m(v))))
          .toMap
      case c: MemoryAssign =>
        m + (c.lhs -> c.rhs.variables.foldLeft(LatticeSet.Bottom[Variable]())((s, v) => s.union(m(v))))
      case c: MemoryLoad => m + (c.lhs -> topTerm)
      case c: MemoryStore => m
      case c: Assume => m
      case c: Assert => m
      case c: IndirectCall => top
      case c: DirectCall => top
      case c: GoTo => m
      case c: Return =>
        m ++ c.outParams
          .map((l, e) => l -> e.variables.foldLeft(LatticeSet.Bottom[Variable]())((s, v) => s.union(m(v))))
          .toMap
      case c: Unreachable => m
      case c: NOP => m
    }
  }

  override def init(b: Block): LatticeMap[Variable, LatticeSet[Variable]] = {
    if Some(b) == b.parent.entryBlock then initialState else bot
  }

  def termToPred(m: LatticeMap[Variable, LatticeSet[Variable]], v: Variable, s: LatticeSet[Variable]): Predicate =
    (v, s) match {
      case (v: Variable, LatticeSet.FiniteSet(s)) => {
        val g = s.foldLeft(Some(GammaTerm.Low)) { (q: Option[GammaTerm], t) =>
          (q, t) match {
            case (Some(q), v: Variable) => Some(GammaTerm.Join(Set(q, GammaTerm.OldVar(v))))
            case _ => None
          }
        }
        g match {
          // TODO is this right? it should depend on whether this is a may or must analysis.
          case Some(g) => Predicate.gammaGeq(g, GammaTerm.Var(v)).simplify
          case None => Predicate.True
        }
      }
      case _ => Predicate.True
    }
}

/**
 * A may gamma domain. See `GammaDomain`. This is a forwards analysis.
 */
class MayGammaDomain(initialState: VarGammaMap)
    extends GammaDomain(initialState)
    with MayPredMapDomain[Variable, LatticeSet[Variable]] {
  def joinTerm(a: LatticeSet[Variable], b: LatticeSet[Variable], pos: Block): LatticeSet[Variable] = a.union(b)

  def topTerm: LatticeSet[Variable] = LatticeSet.Top()
  def botTerm: LatticeSet[Variable] = LatticeSet.Bottom()
}

/**
 * A must gamma domain. See `GammaDomain`. This is a forwards analysis.
 */
class MustGammaDomain(initialState: VarGammaMap)
    extends GammaDomain(initialState)
    with MustPredMapDomain[Variable, LatticeSet[Variable]] {
  // Meeting on places we would normally join is how we get our must analysis.
  def joinTerm(a: LatticeSet[Variable], b: LatticeSet[Variable], pos: Block): LatticeSet[Variable] = a.intersect(b)

  def topTerm: LatticeSet[Variable] = LatticeSet.Bottom()
  // Since bottom is set to Top, it is important to ensure that when running this analysis, the entry point
  // to the procedure you are analysing has things mostly set to Bottom. That way, Top does not propagate.
  def botTerm: LatticeSet[Variable] = LatticeSet.Top()
}

class BackwardsGammaDomain extends PredicateEncodingDomain[LatticeSet[Variable]] {
  def join(a: LatticeSet[Variable], b: LatticeSet[Variable], p: Block) = a.meet(b)

  import LatticeSet.*

  def transfer(b: LatticeSet[Variable], c: Command) = c match {
    case c: LocalAssign => if b.contains(c.lhs) then (b - c.lhs).union(FiniteSet(c.rhs.variables)) else b
    case c: SimulAssign =>
      c.assignments.foldLeft(b) { case (b, (lhs, rhs)) =>
        if b.contains(lhs) then (b - lhs).union(FiniteSet(rhs.variables)) else b
      }
    case c: MemoryAssign => if b.contains(c.lhs) then (b - c.lhs).union(FiniteSet(c.rhs.variables)) else b
    case c: MemoryLoad => b - c.lhs
    case c: MemoryStore => b
    case c: Assume if c.checkSecurity => b.union(FiniteSet(c.body.variables))
    case c: IndirectCall => top
    case c: DirectCall => top
    case c: Assume => b
    case c: Assert => b
    case c: GoTo => b
    case c: Return =>
      c.outParams.foldLeft(b) { case (b, (lhs, rhs)) =>
        if b.contains(lhs) then (b - lhs).union(FiniteSet(rhs.variables)) else b
      }
    case c: Unreachable => b
    case c: NOP => b
  }
  def top: LatticeSet[Variable] = Bottom()
  def bot: LatticeSet[Variable] = Top()
  def toPred(x: LatticeSet[Variable]): Predicate = {
    x match {
      case FiniteSet(s) => Predicate.GammaCmp(BoolIMPLIES, GammaTerm.Low, GammaTerm.Join(s.map(GammaTerm.Var(_))))
      case Bottom() => Predicate.True
      case Top() | DiffSet(_) => Predicate.False
    }
  }
}

/**
 * Computes sufficient conditions for a block to be reached. Currently, we only know that the blocks
 * prior to a branch are definitely reachable, and (soundly) assume everything else might not be
 * reachable.
 */
class ReachabilityConditions extends PredicateEncodingDomain[Predicate] {
  // As the expressions get more complex, it may be worth considering not simplifying at every join
  def join(a: Predicate, b: Predicate, pos: Block): Predicate = Predicate.and(a, b).simplify

  def transfer(b: Predicate, c: Command): Predicate = {
    c match {
      case a: LocalAssign => b
      case a: SimulAssign => b
      case c: MemoryAssign => b
      case a: MemoryLoad => b
      case m: MemoryStore => b
      case a: Assume => b
      case a: Assert => b
      case i: IndirectCall => b
      case c: DirectCall => b
      case g: GoTo => if g.targets.size == 1 then b else Predicate.False
      case r: Return => b
      case r: Unreachable => b
      case n: NOP => b
    }
  }

  def top: Predicate = Predicate.False
  def bot: Predicate = Predicate.True

  def toPred(x: Predicate): Predicate = x
}

/**
 * An abstract domain where abstract states are predicates (and concretise to the set of states satisfying the predicate).
 * This analysis should be run backwards.
 */
class PredicateDomain(summaries: Procedure => ProcedureSummary) extends PredicateEncodingDomain[Predicate] {
  import Predicate.*

  private var atTop = Set[Block]()

  private def expectPredicate(orig: Expr): Predicate = {
    exprToPredicate(orig) match {
      case Some(p) => p
      case None => top
    }
  }

  def join(a: Predicate, b: Predicate, pos: Block): Predicate =
    if a.size + b.size > 100 then atTop += pos
    if atTop.contains(pos) then top else and(a, b).simplify

  private def lowExpr(e: Expr): Predicate =
    gammaLeq(GammaTerm.Join(e.variables.map(v => GammaTerm.Var(v))), GammaTerm.Low)

  def transfer(b: Predicate, c: Command): Predicate = {
    c match {
      case SimulAssign(assignments, label) =>
        val vs = assignments.map((lhs, rhs) => BVTerm.Var(lhs) -> exprToBVTerm(rhs))
        val gamms = assignments.map((lhs, rhs) => GammaTerm.Var(lhs) -> exprToGammaTerm(rhs))
        val nb = vs.foldLeft(b) { case (a, (l, r)) =>
          r match {
            case Some(r) => a.replace(l, r)
            case None => a.remove(l, top)
          }
        }
        gamms.foldLeft(nb) { case (a, (l, r)) =>
          r match {
            case Some(r) => a.replace(l, r)
            case None => a.remove(l, top)
          }
        }
      case a: MemoryAssign =>
        b.replace(BVTerm.Var(a.lhs), exprToBVTerm(a.rhs).get)
          .replace(GammaTerm.Var(a.lhs), exprToGammaTerm(a.rhs).get)
          .simplify
      case a: MemoryLoad => b.remove(BVTerm.Var(a.lhs), True).remove(GammaTerm.Var(a.lhs), True).simplify
      case m: MemoryStore => b
      case a: Assume => and(b, expectPredicate(a.body)).simplify
      case a: Assert => and(b, expectPredicate(a.body)).simplify
      case i: IndirectCall => top
      case c: DirectCall =>
        c.actualParams.foldLeft(Conj(summaries(c.target).requires.map(_.pred).toSet).simplify) { case (p, (v, e)) =>
          p.replace(BVTerm.Var(v), exprToBVTerm(e).get).replace(GammaTerm.Var(v), exprToGammaTerm(e).get).simplify
        }
      case g: GoTo => b
      case r: Return => b
      case r: Unreachable => b
      case n: NOP => b
    }
  }

  def top: Predicate = False
  def bot: Predicate = True

  def toPred(x: Predicate): Predicate = x
  override def fromPred(p: Predicate): Predicate = p
}

/**
 * An abstract domain which computes conditions (as predicates) for assertions (including the check that gammas are low on branches) to be violated.
 *
 * See the ICFEM 2013 paper "Path-Sensitive Data Flow Analysis Simplified"
 */
class WpDualDomain(summaries: Procedure => ProcedureSummary) extends PredicateEncodingDomain[Predicate] {
  import Predicate.*

  private var atTop = Set[Block]()

  private def expectPredicate(orig: Expr): Predicate = {
    exprToPredicate(orig) match {
      case Some(p) => p
      case None => not(top)
    }
  }

  def join(a: Predicate, b: Predicate, pos: Block): Predicate =
    if a.size + b.size > 100 then atTop += pos
    if atTop.contains(pos) then top else or(a, b).simplify

  private def lowExpr(e: Expr): Predicate =
    gammaLeq(GammaTerm.Join(e.variables.map(v => GammaTerm.Var(v))), GammaTerm.Low)

  def transfer(b: Predicate, c: Command): Predicate = {
    c match {
      case SimulAssign(assigns, _) => {
        val terms = assigns.map((l, r) => (BVTerm.Var(l), exprToBVTerm(r)))
        val gammas = assigns.map((l, r) => (GammaTerm.Var(l), exprToGammaTerm(r)))
        val nb = terms.foldLeft(b) { case (acc, (l, r)) =>
          r match {
            case Some(rhs) => acc.replace(l, rhs)
            case None => acc.remove(l, top)
          }
        }
        gammas.foldLeft(nb) { case (acc, (l, r)) =>
          r match {
            case Some(rhs) => acc.replace(l, rhs)
            case None => acc.remove(l, top)
          }
        }
      }
      case a: MemoryAssign =>
        b.replace(BVTerm.Var(a.lhs), exprToBVTerm(a.rhs).get)
          .replace(GammaTerm.Var(a.lhs), exprToGammaTerm(a.rhs).get)
          .simplify
      case a: MemoryLoad => b.remove(BVTerm.Var(a.lhs), top).remove(GammaTerm.Var(a.lhs), top).simplify
      case m: MemoryStore => b
      case a: Assume => {
        if (a.checkSecurity) {
          or(and(b, expectPredicate(a.body)), not(lowExpr(a.body))).simplify
        } else {
          and(b, expectPredicate(a.body)).simplify
        }
      }
      case a: Assert => or(b, not(expectPredicate(a.body))).simplify
      case i: IndirectCall => top
      case c: DirectCall =>
        not(c.actualParams.foldLeft(Conj(summaries(c.target).requires.map(_.pred).toSet).simplify) { case (p, (v, e)) =>
          p.replace(BVTerm.Var(v), exprToBVTerm(e).get).replace(GammaTerm.Var(v), exprToGammaTerm(e).get).simplify
        })
      case g: GoTo => b
      case r: Return => b
      case r: Unreachable => b
      case n: NOP => b
    }
  }

  // We annotate the return block as unreachable as we only want to know conditions for paths that reach failing assertions.
  override def init(b: Block): Predicate = if b.isReturn then False else bot

  def top: Predicate = False
  def bot: Predicate = True

  def toPred(x: Predicate): Predicate = not(x)
  override def fromPred(p: Predicate): Predicate = not(p)
}

import collection.immutable.ListSet

class AssertionDomain[D1, D2](r: PredicateEncodingDomain[D1], d: PredicateEncodingDomain[D2], bound: Int)
    extends PredicateEncodingDomain[ListSet[(D1, D2)]] {
  def filter(r: D1, x: D2): Boolean = true
  def bound(x: ListSet[(D1, D2)]): ListSet[(D1, D2)] = x.filter((r, x) => filter(r, x)).take(bound)
  def join(a: ListSet[(D1, D2)], b: ListSet[(D1, D2)], pos: Block): ListSet[(D1, D2)] = bound(a.union(b))
  override def widen(a: ListSet[(D1, D2)], b: ListSet[(D1, D2)], pos: Block): ListSet[(D1, D2)] = bound(a.union(b))
  def transfer(x: ListSet[(D1, D2)], c: Command): ListSet[(D1, D2)] = {
    c match {
      case a: Assert => {
        bound(x + ((r.bot, d.transfer(d.top, c))))
      }
      case a: Assume => {
        x.map((d1, d2) => (r.transfer(d1, Assert(a.body)), d.transfer(d2, c)))
      }
      case c => x.map((d1, d2) => (r.transfer(d1, c), d.transfer(d2, c)))
    }
  }

  def top = ListSet((r.bot, d.top))
  def bot = ListSet()

  import Predicate.*

  def toPred(x: ListSet[(D1, D2)]): Predicate = x.foldLeft(True) { case (p, (d1, d2)) =>
    and(p, implies(r.toPred(d1), d.toPred(d2)))
  }
}

class BranchAssertionDomain(summaries: Procedure => ProcedureSummary, bound: Int) extends AssertionDomain(PredicateDomain(summaries), BackwardsGammaDomain(), bound) {
  override def filter(r: Predicate, x: LatticeSet[Variable]): Boolean = r != Predicate.False && x != LatticeSet.Bottom()

  override def transfer(
    x: ListSet[(Predicate, LatticeSet[Variable])],
    c: Command
  ): ListSet[(Predicate, LatticeSet[Variable])] = {
    c match {
      case c: Assume if c.checkSecurity =>
        val y = super.transfer(x, c)
        y + ((Predicate.True, LatticeSet.FiniteSet(c.body.variables)))
      case c: Assert => x
      case c => super.transfer(x, c)
    }
  }
  import Predicate.*
  override def toPred(x: ListSet[(Predicate, LatticeSet[Variable])]): Predicate =
    x.foldLeft(Map[LatticeSet[Variable], Predicate]()) { case (m, (d1, d2)) =>
      val cur = m.getOrElse(d2, Predicate.False)
      m + (d2 -> (Predicate.or(cur,d1)))
    }
    .foldLeft(True) { case (p, (d2, d1)) =>
    and(p, implies(d1, BackwardsGammaDomain().toPred(d2)))
  }
}
