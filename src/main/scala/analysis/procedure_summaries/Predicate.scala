package analysis

import util.StaticAnalysisLogger
import ir.*
import boogie.*
import ir.transforms.AbstractDomain

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
}

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
      else s.tail.foldLeft(s.head.toBoogie) {
        (p, g) => BinaryBExpr(BoolAND, p, g.toBoogie)
      }
  }

  def toBasil: Option[Expr] = this match {
    case Lit(x) => Some(x)
    case Var(v) => Some(LocalVar(s"Gamma_${v.name}", BoolType))
    case OldVar(v) => None
    case Uop(op, x) => x.toBasil.map(x => UnaryExpr(op, x))
    case Join(s) =>
      if s.size == 0 then Some(TrueLiteral)
      else if s.size == 1 then s.head.toBasil
      else s.tail.foldLeft(s.head.toBasil) {
        (p, g) => g.toBasil.flatMap(g => p.map(p => BinaryExpr(BoolAND, p, g)))
      }
  }

  def vars: Set[Variable] = this match {
    case Lit(_) => Set()
    case Var(v) => Set(v)
    case OldVar(v) => Set(v)
    case Uop(_, x) => x.vars
    case Join(s) => s.foldLeft(Set()) { (s, g) => s ++ g.vars }
  }

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

  def replace(prev: GammaTerm, cur: GammaTerm): GammaTerm = this match {
    case x if x == prev => cur
    case Lit(x) => this
    case Var(v) => this
    case OldVar(v) => this
    case Uop(op, x) => Uop(op, x.replace(prev, cur))
    case Join(s) => Join(s.map(g => g.replace(prev, cur)))
  }
}

enum Predicate {
  case Lit(x: BoolLit)
  case Uop(op: BoolUnOp, x: Predicate)
  case Bop(op: BoolBinOp, x: Predicate, y: Predicate)
  case BVCmp(op: BVCmpOp, x: BVTerm, y: BVTerm)
  case GammaCmp(op: BoolCmpOp, x: GammaTerm, y: GammaTerm)

  private var simplified: Boolean = false

  def toBoogie: BExpr = this match {
    case Lit(x) => x.toBoogie
    case Uop(op, x) => UnaryBExpr(op, x.toBoogie)
    case Bop(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
    case BVCmp(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
    case GammaCmp(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
  }

  def toBasil: Option[Expr] = this match {
    case Lit(x) => Some(x)
    case Uop(op, x) => x.toBasil.map(x => UnaryExpr(op, x))
    case Bop(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
    case BVCmp(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
    case GammaCmp(op, x, y) => x.toBasil.flatMap(x => y.toBasil.map(y => BinaryExpr(op, x, y)))
  }

  override def toString(): String = this.toBoogie.toString

  def simplify: Predicate = {
    if this.simplified then return this
    val ret = this match {
      case Bop(BoolAND, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), b) => b
          case (a, Lit(TrueLiteral)) => a
          case (Lit(FalseLiteral), _) => Lit(FalseLiteral)
          case (_, Lit(FalseLiteral)) => Lit(FalseLiteral)
          case (BVCmp(BVSLE, a, b), BVCmp(BVSLE, c, d)) if a == d && b == c => BVCmp(BVEQ, a, b)
          case (BVCmp(BVULE, a, b), BVCmp(BVULE, c, d)) if a == d && b == c => BVCmp(BVEQ, a, b)
          case (BVCmp(BVSGE, a, b), BVCmp(BVSGE, c, d)) if a == d && b == c => BVCmp(BVEQ, a, b)
          case (BVCmp(BVUGE, a, b), BVCmp(BVUGE, c, d)) if a == d && b == c => BVCmp(BVEQ, a, b)
          case (BVCmp(BVSLE, a, b), BVCmp(BVSGE, c, d)) if a == c && b == d => BVCmp(BVEQ, a, b)
          case (BVCmp(BVULE, a, b), BVCmp(BVUGE, c, d)) if a == c && b == d => BVCmp(BVEQ, a, b)
          case (BVCmp(BVSGE, a, b), BVCmp(BVSLE, c, d)) if a == c && b == d => BVCmp(BVEQ, a, b)
          case (BVCmp(BVUGE, a, b), BVCmp(BVULE, c, d)) if a == c && b == d => BVCmp(BVEQ, a, b)
          case (GammaCmp(BoolIMPLIES, a, b), GammaCmp(BoolIMPLIES, c, d)) if a == d && b == c => GammaCmp(BoolEQ, a, b)
          case (a, b) if a == b => a
          case (a, b) => Bop(BoolAND, a, b)
        }
      case Bop(BoolOR, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), _) => Lit(TrueLiteral)
          case (_, Lit(TrueLiteral)) => Lit(TrueLiteral)
          case (Lit(FalseLiteral), b) => b
          case (a, Lit(FalseLiteral)) => a
          case (a, b) if a == b => a
          case (a, b) => Bop(BoolOR, a, b)
        }
      case Bop(BoolIMPLIES, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), b) => b
          case (Lit(FalseLiteral), _) => Lit(TrueLiteral)
          case (_, Lit(TrueLiteral)) => Lit(TrueLiteral)
          case (a, b) if a == b => a
          case (a, b) => Bop(BoolIMPLIES, a, b)
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
    StaticAnalysisLogger.debug(s"simplified $this into $ret")
    ret.simplified = true
    ret
  }

  def split: List[Predicate] =
    this match {
      case Bop(BoolAND, a, b) => a.split ++ b.split
      case _ => List(this)
    }

  def replace(prev: BVTerm, cur: BVTerm): Predicate = this match {
    case Lit(x) => this
    case Uop(op, x) => Uop(op, x.replace(prev, cur))
    case Bop(op, x, y) => Bop(op, x.replace(prev, cur), y.replace(prev, cur))
    case BVCmp(op, x, y) => BVCmp(op, x.replace(prev, cur), y.replace(prev, cur))
    case GammaCmp(op, x, y) => this
  }

  def replace(prev: GammaTerm, cur: GammaTerm): Predicate = this match {
    case Lit(x) => this
    case Uop(op, x) => Uop(op, x.replace(prev, cur))
    case Bop(op, x, y) => Bop(op, x.replace(prev, cur), y.replace(prev, cur))
    case BVCmp(op, x, y) => this
    case GammaCmp(op, x, y) => GammaCmp(op, x.replace(prev, cur), y.replace(prev, cur))
  }
}

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
  def toPred(x: L): Predicate

  def fromPred(p: Predicate): L = top
}
