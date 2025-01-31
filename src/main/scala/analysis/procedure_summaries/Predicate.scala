package analysis

import util.StaticAnalysisLogger
import ir.*
import boogie.*
import ir.transforms.AbstractDomain

// TODO DAG predicates (don't represent the same expression twice)

/**
 */
enum BVTerm {
  case Lit(x: BitVecLiteral)
  case Var(v: Variable)
  case OldVar(v: Variable)
  case Uop(op: BVUnOp, x: BVTerm)
  case Bop(op: BVBinOp, x: BVTerm, y: BVTerm)
  case Extract(end: Int, start: Int, body: BVTerm)
  case Repeat(repeats: Int, body: BVTerm)
  case ZeroExtend(extension: Int, body: BVTerm)
  case SignExtend(extension: Int, body: BVTerm)

  private var simplified: Boolean = false

  override def toString(): String = this.toBoogie.toString()

  def toBoogie: BExpr = this match {
    case Lit(x) => x.toBoogie
    case Var(v) => v.toBoogie
    case OldVar(v) => Old(v.toBoogie)
    case Uop(op, x) => UnaryBExpr(op, x.toBoogie)
    case Bop(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
    case Extract(end, start, body) => BVExtract(end, start, body.toBoogie)
    case Repeat(repeats, body) => BVRepeat(repeats, body.toBoogie)
    case ZeroExtend(extension, body) => BVZeroExtend(extension, body.toBoogie)
    case SignExtend(extension, body) => BVSignExtend(extension, body.toBoogie)
  }

  def toBasil: Option[Expr] = this match {
    case Lit(x) => Some(x)
    case Var(v) => Some(v)
    case OldVar(v) => None
    case Uop(op, x) => x.toBasil.map(x => UnaryExpr(op, x))
    case Bop(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
    case Extract(end, start, body) => body.toBasil.map(x => ir.Extract(end, start, x))
    case Repeat(repeats, body) => body.toBasil.map(x => ir.Repeat(repeats, x))
    case ZeroExtend(extension, body) => body.toBasil.map(x => ir.ZeroExtend(extension, x))
    case SignExtend(extension, body) => body.toBasil.map(x => ir.SignExtend(extension, x))
  }

  /**
   */
  def simplify: BVTerm = {
    if this.simplified then return this
    val ret = this match {
      // TODO const prop
      //case Uop(op, x) => Uop(op, x.simplify)
      //case Bop(op, x, y) => Bop(op, x.simplify, y.simplify)
      //case Extract(end, start, body) => Extract(end, start, body.simplify)
      //case Repeat(repeats, body) => Repeat(repeats, body.simplify)
      //case ZeroExtend(extension, body) => ZeroExtend(extension, body.simplify)
      //case SignExtend(extension, body) => SignExtend(extension, body.simplify)
      case _ => this
    }
    ret.simplified = true
    ret
  }

  /**
   * Replace all instances of the term `prev` with the new term `cur`.
   */
  def replace(prev: BVTerm, cur: BVTerm): BVTerm = this match {
    case x if x == prev => cur
    case Lit(x) => this
    case Var(v) => this
    case OldVar(v) => this
    case Uop(op, x) => Uop(op, x.replace(prev, cur))
    case Bop(op, x, y) => Bop(op, x.replace(prev, cur), y.replace(prev, cur))
    case Extract(end, start, body) => Extract(end, start, body.replace(prev, cur))
    case Repeat(repeats, body) => Repeat(repeats, body.replace(prev, cur))
    case ZeroExtend(extension, body) => ZeroExtend(extension, body.replace(prev, cur))
    case SignExtend(extension, body) => SignExtend(extension, body.replace(prev, cur))
  }

  /**
   * Determines whether the provided term is a subexpression of this expression
   */
  def contains(term: BVTerm): Boolean = if this == term then true else this match {
    case Uop(op, x) => x.contains(term)
    case Bop(op, x, y) => x.contains(term) || y.contains(term)
    case Extract(end, start, body) => body.contains(term)
    case Repeat(repeats, body) => body.contains(term)
    case ZeroExtend(extension, body) => body.contains(term)
    case SignExtend(extension, body) => body.contains(term)
    case _ => false
  }
}

/**
 */
enum GammaTerm {
  case Lit(x: BoolLit)
  case Var(v: Variable)
  case OldVar(v: Variable)
  case Uop(op: BoolUnOp, x: GammaTerm)
  case Join(s: Set[GammaTerm])

  private var simplified: Boolean = false

  override def toString(): String = this.toBoogie.toString()

  def toBoogie: BExpr = this match {
    case Lit(x) => x.toBoogie
    case Var(v) => v.toGamma
    case OldVar(v) => Old(v.toGamma)
    case Uop(op, x) => UnaryBExpr(op, x.toBoogie)
    case Join(s) =>
      if s.size == 0 then TrueBLiteral
      else if s.size == 1 then s.head.toBoogie
      else s.tail.foldLeft(s.head.toBoogie) { (p, g) => BinaryBExpr(BoolAND, p, g.toBoogie) }
  }

  def toBasil: Option[Expr] = this match {
    case Lit(x) => Some(x)
    case Var(v) => Some(LocalVar(s"Gamma_${v.name}", BoolType))
    case OldVar(v) => None
    case Uop(op, x) => x.toBasil.map(x => UnaryExpr(op, x))
    case Join(s) =>
      if s.size == 0 then Some(TrueLiteral)
      else if s.size == 1 then s.head.toBasil
      else s.tail.foldLeft(s.head.toBasil) { (p, g) => g.toBasil.flatMap(g => p.map(p => BinaryExpr(BoolAND, p, g))) }
  }

  /**
   */
  def simplify: GammaTerm = {
    if this.simplified then return this
    val ret = this match {
      case Join(s) => {
        val set = s.map(g => g.simplify).foldLeft(Set[GammaTerm]()) {
          (s, g) => g match {
            case Join(s2) => s ++ s2
            case g => s + g
          }
        } - Lit(TrueLiteral)
        if set.size == 0 then Lit(TrueLiteral) else if set.size == 1 then set.head else Join(set)
      }
      case _ => this
    }
    ret.simplified = true
    ret
  }

  /**
   * Replace all instances of the term `prev` with the new term `cur`.
   */
  def replace(prev: GammaTerm, cur: GammaTerm): GammaTerm = this match {
    case x if x == prev => cur
    case Lit(x) => this
    case Var(v) => this
    case OldVar(v) => this
    case Uop(op, x) => Uop(op, x.replace(prev, cur))
    case Join(s) => Join(s.map(g => g.replace(prev, cur)))
  }

  /**
   * Determines whether the provided term is a subexpression of this expression
   */
  def contains(term: GammaTerm): Boolean = if this == term then true else this match {
    case Lit(x) => false
    case Var(v) => false
    case OldVar(v) => false
    case Uop(op, x) => x.contains(term)
    case Join(s) => s.exists(_.contains(term))
  }
}

/**
 */
enum Predicate {
  case Lit(x: BoolLit)
  case Uop(op: BoolUnOp, x: Predicate)
  case Bop(op: BoolBinOp, x: Predicate, y: Predicate)
  case Conj(s: Set[Predicate])
  case Disj(s: Set[Predicate])
  case BVCmp(op: BVCmpOp, x: BVTerm, y: BVTerm)
  case GammaCmp(op: BoolCmpOp, x: GammaTerm, y: GammaTerm)

  private var simplified: Boolean = false

  def toBoogie: BExpr = this match {
    case Lit(x) => x.toBoogie
    case Uop(op, x) => UnaryBExpr(op, x.toBoogie)
    case Bop(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
    case Conj(s) =>
      if s.size == 0 then TrueBLiteral
      else if s.size == 1 then s.head.toBoogie
      else s.tail.foldLeft(s.head.toBoogie) { (p, q) => BinaryBExpr(BoolAND, p, q.toBoogie) }
    case Disj(s) =>
      if s.size == 0 then FalseBLiteral
      else if s.size == 1 then s.head.toBoogie
      else s.tail.foldLeft(s.head.toBoogie) { (p, q) => BinaryBExpr(BoolOR, p, q.toBoogie) }
    case BVCmp(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
    case GammaCmp(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
  }

  def toBasil: Option[Expr] = this match {
    case Lit(x) => Some(x)
    case Uop(op, x) => x.toBasil.map(x => UnaryExpr(op, x))
    case Bop(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
    case Conj(s) =>
      if s.size == 0 then Some(TrueLiteral)
      else if s.size == 1 then s.head.toBasil
      else s.tail.foldLeft(s.head.toBasil) { (p, q) => p.flatMap { p => q.toBasil.map { q => BinaryExpr(BoolAND, p, q) } } }
    case Disj(s) =>
      if s.size == 0 then Some(FalseLiteral)
      else if s.size == 1 then s.head.toBasil
      else s.tail.foldLeft(s.head.toBasil) { (p, q) => p.flatMap { p => q.toBasil.map { q => BinaryExpr(BoolOR, p, q) } } }
    case BVCmp(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
    case GammaCmp(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
  }

  //override def toString(): String = this.toBoogie.toString

  /**
   */
  def simplify: Predicate = {
    if this.simplified then return this
    val ret = this match {
      case Bop(BoolAND, a, b) =>
        (a.simplify, b.simplify) match {
          case (Conj(a), Conj(b)) => Conj(a ++ b)
          case (Conj(a), b) => Conj(a + b)
          case (a, Conj(b)) => Conj(b + a)
          case (a, b) => Conj(Set(a, b))
        }
      case Bop(BoolOR, a, b) =>
        (a.simplify, b.simplify) match {
          case (Disj(a), Disj(b)) => Disj(a ++ b)
          case (Disj(a), b) => Disj(a + b)
          case (a, Disj(b)) => Disj(b + a)
          case (a, b) => Disj(Set(a, b))
        }
      case Bop(BoolIMPLIES, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), b) => b
          case (Lit(FalseLiteral), _) => Lit(TrueLiteral)
          case (_, Lit(TrueLiteral)) => Lit(TrueLiteral)
          case (a, b) if a == b => a
          case (a, b) => Bop(BoolIMPLIES, a, b)
        }
      case Bop(op, a, b) => Bop(op, a.simplify, b.simplify)
      case Uop(BoolNOT, a) =>
        a.simplify match {
          case Lit(TrueLiteral) => Lit(FalseLiteral)
          case Lit(FalseLiteral) => Lit(TrueLiteral)
          case a => Uop(BoolNOT, a)
        }
      case Uop(op, a) => Uop(op, a.simplify)
      case Conj(s) => {
        var cur = s.map(_.simplify)
        var changed = true
        while (changed) {
          changed = false

          // Merge internal disjuncts
          var disj = Set[Predicate]()
          for x <- cur do {
            x match {
              case d @ Disj(s) =>
                cur -= d
                disj = disj ++ s
              case _ => {}
            }
          }
          if disj.size == 1 then cur += disj.head
          else if disj.size > 1 then cur += Disj(disj)

          for x <- cur if !changed do {
            val cur1 = x match {
              case Lit(FalseLiteral) => Set(Lit(FalseLiteral))
              case p @ Conj(s2) => cur - p ++ s2
              case p @ BVCmp(BVSLE, a, b) if cur.contains(BVCmp(BVSLE, b, a)) => cur - p - BVCmp(BVSLE, b, a) + BVCmp(BVEQ, a, b)
              case p @ BVCmp(BVSLE, a, b) if cur.contains(BVCmp(BVSGE, a, b)) => cur - p - BVCmp(BVSGE, a, b) + BVCmp(BVEQ, a, b)
              case p @ BVCmp(BVULE, a, b) if cur.contains(BVCmp(BVULE, b, a)) => cur - p - BVCmp(BVULE, b, a) + BVCmp(BVEQ, a, b)
              case p @ BVCmp(BVULE, a, b) if cur.contains(BVCmp(BVUGE, a, b)) => cur - p - BVCmp(BVUGE, a, b) + BVCmp(BVEQ, a, b)
              case p @ GammaCmp(BoolIMPLIES, a, b) if cur.contains(GammaCmp(BoolIMPLIES, b, a)) => cur - p - GammaCmp(BoolIMPLIES, b, a) + GammaCmp(BoolEQ, a, b)
              case _ => cur
            }
            changed = cur != cur1
            cur = cur1
          }
          cur -= Lit(TrueLiteral)
        }
        if cur.size == 1 then cur.head else Conj(cur)
      }
      case Disj(s) => {
        var cur = s.map(_.simplify)
        var changed = true
        while (changed) {
          changed = false

          // Merge internal conjuncts
          var conj = Set[Predicate]()
          for x <- cur do {
            x match {
              case d @ Conj(s) =>
                cur -= d
                conj = conj ++ s
              case _ => {}
            }
          }
          if conj.size == 1 then cur += conj.head
          else if conj.size > 1 then cur += Conj(conj)

          for x <- cur if !changed do {
            val cur1 = x match {
              case p @ Disj(s2) => cur - p ++ s2
              case Lit(TrueLiteral) => Set(Lit(TrueLiteral))
              case _ => cur
            }
            changed = cur != cur1
            cur = cur1
          }
          cur -= Lit(FalseLiteral)
        }
        if cur.size == 1 then cur.head else Disj(cur)
      }
      case BVCmp(op, a, b) =>
        (op, a.simplify, b.simplify) match {
          case (op, a, b) => BVCmp(op, a, b)
        }
      case GammaCmp(op, a, b) =>
        (op, a.simplify, b.simplify) match {
          case (BoolIMPLIES, a, b) if a == b => Lit(TrueLiteral)
          case (BoolEQ, a, b) if a == b => Lit(TrueLiteral)
          case (op, a, b) => GammaCmp(op, a, b)
        }
      case _ => this
    }
    ret.simplified = true
    ret
  }

  /**
   * Convert a conjunction of predicates into the set of predicates in the conjunction.
   * All predicates are at least conjunctions of one term.
   */
  def split: List[Predicate] =
    this match {
      case Bop(BoolAND, a, b) => a.split ++ b.split
      case _ => List(this)
    }

  /**
   * Replace all instances of the term `prev` with the new term `cur`.
   */
  def replace(prev: BVTerm, cur: BVTerm): Predicate = this match {
    case Lit(x) => this
    case Uop(op, x) => Uop(op, x.replace(prev, cur))
    case Bop(op, x, y) => Bop(op, x.replace(prev, cur), y.replace(prev, cur))
    case Conj(s) => Conj(s.map(_.replace(prev, cur)))
    case Disj(s) => Disj(s.map(_.replace(prev, cur)))
    case BVCmp(op, x, y) => BVCmp(op, x.replace(prev, cur), y.replace(prev, cur))
    case GammaCmp(op, x, y) => this
  }

  /**
   * Replace all instances of the term `prev` with the new term `cur`.
   */
  def replace(prev: GammaTerm, cur: GammaTerm): Predicate = this match {
    case Lit(x) => this
    case Uop(op, x) => Uop(op, x.replace(prev, cur))
    case Bop(op, x, y) => Bop(op, x.replace(prev, cur), y.replace(prev, cur))
    case Conj(s) => Conj(s.map(_.replace(prev, cur)))
    case Disj(s) => Disj(s.map(_.replace(prev, cur)))
    case BVCmp(op, x, y) => this
    case GammaCmp(op, x, y) => GammaCmp(op, x.replace(prev, cur), y.replace(prev, cur))
  }

  /**
   * Remove atomic expressions containing `term` (replacing with True if needed)
   */
  def remove(term: BVTerm): Predicate = this match {
    case Lit(x) => this
    case Uop(op, x) => Uop(op, x.remove(term))
    case Bop(op, x, y) => Bop(op, x.remove(term), y.remove(term))
    case Conj(s) => Conj(s.map(_.remove(term)))
    case Disj(s) => Disj(s.map(_.remove(term)))
    case BVCmp(op, x, y) => if x.contains(term) || y.contains(term) then Lit(TrueLiteral) else this
    case GammaCmp(op, x, y) => this
  }

  /**
   * Remove atomic expressions containing `term` (replacing with True if needed)
   */
  def remove(term: GammaTerm): Predicate = this match {
    case Lit(x) => this
    case Uop(op, x) => Uop(op, x.remove(term))
    case Bop(op, x, y) => Bop(op, x.remove(term), y.remove(term))
    case Conj(s) => Conj(s.map(_.remove(term)))
    case Disj(s) => Disj(s.map(_.remove(term)))
    case BVCmp(op, x, y) => this
    case GammaCmp(op, x, y) => if x.contains(term) || y.contains(term) then Lit(TrueLiteral) else this
  }
}

/**
 * Try to convert a BASIL expression into a BVTerm.
 */
def exprToBVTerm(e: Expr): Option[BVTerm] = e match {
  case b: BitVecLiteral => Some(BVTerm.Lit(b))
  case v: Variable => Some(BVTerm.Var(v))
  case UnaryExpr(op: BVUnOp, arg) => exprToBVTerm(arg).map(x => BVTerm.Uop(op, x))
  case BinaryExpr(op: BVBinOp, arg1, arg2) => exprToBVTerm(arg1).flatMap(x => exprToBVTerm(arg2).map(y => BVTerm.Bop(op, x, y)))
  case Extract(end, start, body) => exprToBVTerm(body).map(x => BVTerm.Extract(end, start, x))
  case Repeat(repeats, body) => exprToBVTerm(body).map(x => BVTerm.Repeat(repeats, x))
  case ZeroExtend(extension, body) => exprToBVTerm(body).map(x => BVTerm.ZeroExtend(extension, x))
  case SignExtend(extension, body) => exprToBVTerm(body).map(x => BVTerm.SignExtend(extension, x))
  case _ => None
}

/**
 * Get the gamma of a bitvector expression, i.e. the join of all of the gammas of the variables in the expression
 */
def exprToGammaTerm(e: Expr): Option[GammaTerm] = e match {
  case b: BitVecLiteral => Some(GammaTerm.Lit(TrueLiteral))
  case v: Variable => Some(GammaTerm.Var(v))
  case UnaryExpr(op: BVUnOp, arg) => exprToGammaTerm(arg)
  case BinaryExpr(op: BVBinOp, arg1, arg2) => exprToGammaTerm(arg1).flatMap(x => exprToGammaTerm(arg2).map(y => GammaTerm.Join(Set(x, y))))
  case Extract(end, start, body) => exprToGammaTerm(body)
  case Repeat(repeats, body) => exprToGammaTerm(body)
  case ZeroExtend(extension, body) => exprToGammaTerm(body)
  case SignExtend(extension, body) => exprToGammaTerm(body)
  case _ => None
}

/**
 * Try to convert a BASIL expression into a predicate.
 */
def exprToPredicate(e: Expr): Option[Predicate] = e match {
  case b: BoolLit => Some(Predicate.Lit(b))
  case UnaryExpr(op: BoolUnOp, arg) => exprToPredicate(arg).map(p => Predicate.Uop(op, p))
  case BinaryExpr(op: BoolBinOp, arg1, arg2) => exprToPredicate(arg1).flatMap(p => exprToPredicate(arg2).map(q => Predicate.Bop(op, p, q)))
  case BinaryExpr(op: BVCmpOp, arg1, arg2) => exprToBVTerm(arg1).flatMap(p => exprToBVTerm(arg2).map(q => Predicate.BVCmp(op, p, q)))
  case _ => None
}

/**
 * A domain with a conversion between abstract states and predicates.
 *
 * Abstractly, we can define a concretisation function from the set of predicates to the concrete domain, giving the set of states that satisfy the predicate.
 * For soundness, ensure that
 * - for may analyses, gamma_p (toPred(l)) \supset gamma_a(l) for all l of type L
 *                 and gamma_a (fromPred(p)) \supset gamma_p(p) for all predicates p
 * - for must analyses, gamma_p (toPred(l)) \subset gamma_a(l) for all l of type L
 *                 and gamma_a (fromPred(p)) \subset gamma_p(p) for all predicates p
 * where gamma_p is the predicate concretisation function and gamma_a is the domain concretisation function.
 * Intuitively, the predicate should be an approximation of the abstract state, cannot soundly be more precise than the domain itself.
 */
trait PredicateEncodingDomain[L] extends AbstractDomain[L] {
  /**
   * Convert an abstract value to a predicate encoding its meaning.
   */
  def toPred(x: L): Predicate

  /**
   * Extract an abstract value from a predicate.
   */
  def fromPred(p: Predicate): L = top
}
