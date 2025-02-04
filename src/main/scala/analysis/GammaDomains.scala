package analysis

import boogie.*
import ir.*
import ir.transforms.{AbstractDomain, worklistSolver}

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
  import LatticeMap.{Top, Bottom, TopMap, BottomMap}

  def transfer(m: VarGammaMap, c: Command): VarGammaMap = {
    c match {
      case c: LocalAssign  => m + (c.lhs -> c.rhs.variables.foldLeft(LatticeSet.Bottom[Variable]())((s, v) => s.union(m(v))))
      case c: MemoryLoad   => m + (c.lhs -> topTerm)
      case c: MemoryStore  => m
      case c: Assume       => m
      case c: Assert       => m
      case c: IndirectCall => top
      case c: DirectCall   => top
      case c: GoTo         => m
      case c: Return       => m ++ c.outParams.map((l, e) => l -> e.variables.foldLeft(LatticeSet.Bottom[Variable]())((s, v) => s.union(m(v)))).toMap
      case c: Unreachable  => m
      case c: NOP          => m
    }
  }

  override def init(b: Block): LatticeMap[Variable, LatticeSet[Variable]] = {
    if Some(b) == b.parent.entryBlock then initialState else bot
  }

  def termToPred(m: LatticeMap[Variable, LatticeSet[Variable]], v: Variable, s: LatticeSet[Variable]): Predicate =
    (v, s) match {
      case (v: Variable, LatticeSet.FiniteSet(s)) => {
        val g = s.foldLeft(Some(GammaTerm.Lit(TrueLiteral))) {
          (q: Option[GammaTerm], t) => (q, t) match {
            case (Some(q), v: Variable) => Some(GammaTerm.Join(Set(q, GammaTerm.OldVar(v))))
            case _ => None
          }
        }
        g match {
          case Some(g) => Predicate.GammaCmp(BoolIMPLIES, g, GammaTerm.Var(v)).simplify
          case None => Predicate.Lit(TrueLiteral)
        }
      }
      case _ => Predicate.Lit(TrueLiteral)
    }
}

/**
 * A may gamma domain. See `GammaDomain`. This is a forwards analysis.
 */
class MayGammaDomain(initialState: VarGammaMap) extends GammaDomain(initialState) with MayPredMapDomain[Variable, LatticeSet[Variable]] {
  def joinTerm(a: LatticeSet[Variable], b: LatticeSet[Variable], pos: Block): LatticeSet[Variable] = a.union(b)

  def topTerm: LatticeSet[Variable] = LatticeSet.Top()
  def botTerm: LatticeSet[Variable] = LatticeSet.Bottom()
}

/**
 * A must gamma domain. See `GammaDomain`. This is a forwards analysis.
 */
class MustGammaDomain(initialState: VarGammaMap) extends GammaDomain(initialState) with MustPredMapDomain[Variable, LatticeSet[Variable]]{
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
  def join(a: Predicate, b: Predicate, pos: Block): Predicate = Predicate.Bop(BoolAND, a, b).simplify

  def transfer(b: Predicate, c: Command): Predicate = {
    c match {
      case a: LocalAssign  => b
      case a: MemoryLoad   => b
      case m: MemoryStore  => b
      case a: Assume       => b
      case a: Assert       => b
      case i: IndirectCall => b
      case c: DirectCall   => b
      case g: GoTo         => if g.targets.size == 1 then b else Predicate.Lit(FalseLiteral)
      case r: Return       => b
      case r: Unreachable  => b
      case n: NOP          => b
    }
  }

  def top: Predicate = Predicate.Lit(FalseLiteral)
  def bot: Predicate = Predicate.Lit(TrueLiteral)

  def toPred(x: Predicate): Predicate = x
}

/**
 * Effectively a weakest precondition.
 * This analysis should be run backwards.
 */
class PredicateDomain extends PredicateEncodingDomain[Predicate] {
  import Predicate.*

  private var atTop = Set[Block]()

  def join(a: Predicate, b: Predicate, pos: Block): Predicate =
    if a.size + b.size > 100 then atTop += pos
    if atTop.contains(pos) then top else Bop(BoolOR, a, b).simplify

  private def lowExpr(e: Expr): Predicate = GammaCmp(BoolIMPLIES, GammaTerm.Lit(TrueLiteral), GammaTerm.Join(e.variables.map(v => GammaTerm.Var(v))))

  def transfer(b: Predicate, c: Command): Predicate = {
    c match {
      case a: LocalAssign  => b.replace(BVTerm.Var(a.lhs), exprToBVTerm(a.rhs).get).replace(GammaTerm.Var(a.lhs), exprToGammaTerm(a.rhs).get).simplify
      case a: MemoryLoad   => b.remove(BVTerm.Var(a.lhs)).remove(GammaTerm.Var(a.lhs)).simplify
      case m: MemoryStore  => b
      case a: Assume       => {
        if (a.checkSecurity) {
          Bop(BoolAND, Bop(BoolAND, b, exprToPredicate(a.body).get), lowExpr(a.body)).simplify
        } else {
          Bop(BoolAND, b, exprToPredicate(a.body).get).simplify
        }
      }
      case a: Assert       => Bop(BoolAND, b, exprToPredicate(a.body).get).simplify
      case i: IndirectCall => top
      case c: DirectCall   => top
      case g: GoTo         => b
      case r: Return       => b
      case r: Unreachable  => b
      case n: NOP          => b
    }
  }

  override def init(b: Block): Predicate = top

  def top: Predicate = Lit(TrueLiteral)
  def bot: Predicate = Lit(FalseLiteral)

  def toPred(x: Predicate): Predicate = x
  override def fromPred(p: Predicate): Predicate = p
}
