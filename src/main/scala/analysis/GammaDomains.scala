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

  def join(a: Predicate, b: Predicate, pos: Block): Predicate =
    if a.size + b.size > 100 then atTop += pos
    if atTop.contains(pos) then top else or(a, b).simplify

  private def lowExpr(e: Expr): Predicate =
    gammaLeq(GammaTerm.Join(e.variables.map(v => GammaTerm.Var(v))), GammaTerm.Low)

  def transfer(b: Predicate, c: Command): Predicate = {
    c match {
      case SimulAssign(assignments, label) =>
        val vs = assignments.map((lhs, rhs) => BVTerm.Var(lhs) -> exprToBVTerm(rhs).get)
        val gamms = assignments.map((lhs, rhs) => GammaTerm.Var(lhs) -> exprToGammaTerm(rhs).get)
        val nb = vs.foldLeft(b) { case (a, (l, r)) =>
          a.replace(l, r)
        }
        gamms.foldLeft(nb) { case (a, (l, r)) =>
          a.replace(l, r)
        }
      case a: MemoryAssign =>
        b.replace(BVTerm.Var(a.lhs), exprToBVTerm(a.rhs).get)
          .replace(GammaTerm.Var(a.lhs), exprToGammaTerm(a.rhs).get)
          .simplify
      case a: MemoryLoad => b.remove(BVTerm.Var(a.lhs), True).remove(GammaTerm.Var(a.lhs), True).simplify
      case m: MemoryStore => b
      case a: Assume => {
        if (a.checkSecurity) {
          and(b, expectPredicate(a.body), lowExpr(a.body)).simplify
        } else {
          and(b, expectPredicate(a.body)).simplify
        }
      }
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

  override def init(b: Block): Predicate =
    if b.isReturn then /*Conj(summaries(b.parent).ensures.map(_.pred).toSet)*/ top else bot

  def top: Predicate = True
  def bot: Predicate = False

  def toPred(x: Predicate): Predicate = x
  override def fromPred(p: Predicate): Predicate = p
}

private def expectPredicate(orig: Expr): Predicate = {
  exprToPredicate(orig) match {
    case Some(p) => p
    case None => throw Exception(s"Expected to be able to construct predicate for: $orig")
  }
}

/**
 * An abstract domain which computes conditions (as predicates) for assertions (including the check that gammas are low on branches) to be violated.
 *
 * See the ICFEM 2013 paper "Path-Sensitive Data Flow Analysis Simplified"
 */
class WpDualDomain(summaries: Procedure => ProcedureSummary) extends PredicateEncodingDomain[Predicate] {
  import Predicate.*

  private var atTop = Set[Block]()

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
            case None => acc.remove(l, Predicate.True) // TODO verify soundness
          }
        }
        gammas.foldLeft(nb) { case (acc, (l, r)) =>
          r match {
            case Some(rhs) => acc.replace(l, rhs)
            case None => acc.remove(l, Predicate.True) // TODO verify soundness
          }
        }
      }
      case a: MemoryAssign =>
        b.replace(BVTerm.Var(a.lhs), exprToBVTerm(a.rhs).get)
          .replace(GammaTerm.Var(a.lhs), exprToGammaTerm(a.rhs).get)
          .simplify
      case a: MemoryLoad => b.remove(BVTerm.Var(a.lhs), False).remove(GammaTerm.Var(a.lhs), False).simplify
      case m: MemoryStore => b
      case a: Assume => {
        if (a.checkSecurity) {
          or(and(b, expectPredicate(a.body)), not(lowExpr(a.body))).simplify
        } else {
          and(b, expectPredicate(a.body)).simplify
        }
      }
      case a: Assert => or(b, not(expectPredicate(a.body))).simplify
      case i: IndirectCall => bot
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

  override def init(b: Block): Predicate = bot

  def top: Predicate = True
  def bot: Predicate = False

  def toPred(x: Predicate): Predicate = x
  override def fromPred(p: Predicate): Predicate = p
}
