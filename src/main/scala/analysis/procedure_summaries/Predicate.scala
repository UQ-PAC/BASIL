package analysis

import ir.*
import boogie.*
import ir.transforms.AbstractDomain

// TODO
// to avoid duplicating simplify calls (it should be idempotent) we could perhaps annotate terms which have already been simplified.

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

  def simplify: BVTerm =
    this match {
      // TODO const prop
      //case Uop(op, x) => Uop(op, x.simplify)
      //case Bop(op, x, y) => Bop(op, x.simplify, y.simplify)
      //case Extract(end, start, body) => Extract(end, start, body.simplify)
      //case Repeat(repeats, body) => Repeat(repeats, body.simplify)
      //case ZeroExtend(extension, body) => ZeroExtend(extension, body.simplify)
      //case SignExtend(extension, body) => SignExtend(extension, body.simplify)
      case _ => this
    }
}

enum GammaTerm {
  case Lit(x: BoolLit)
  case Var(v: Variable)
  case OldVar(v: Variable)
  case Uop(op: BoolUnOp, x: GammaTerm)
  case Bop(op: BoolBinOp, x: GammaTerm, y: GammaTerm)

  def toBoogie: BExpr = this match {
    case Lit(x) => x.toBoogie
    case Var(v) => v.toGamma
    case OldVar(v) => Old(v.toGamma)
    case Uop(op, x) => UnaryBExpr(op, x.toBoogie)
    case Bop(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
  }

  def vars: Set[Variable] = this match {
    case Lit(_) => Set()
    case Var(v) => Set(v)
    case OldVar(v) => Set(v)
    case Uop(_, x) => x.vars
    case Bop(_, x, y) => x.vars ++ y.vars
  }

  def simplify: GammaTerm =
    this match {
      case Bop(BoolAND, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), b) => b
          case (a, Lit(TrueLiteral)) => a
          case (Lit(FalseLiteral), _) => Lit(FalseLiteral)
          case (_, Lit(FalseLiteral)) => Lit(FalseLiteral)
          case (a, b) => Bop(BoolAND, a, b)
        }
      case Bop(BoolOR, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), _) => Lit(TrueLiteral)
          case (_, Lit(TrueLiteral)) => Lit(TrueLiteral)
          case (Lit(FalseLiteral), b) => b
          case (a, Lit(FalseLiteral)) => a
          case (a, b) => Bop(BoolOR, a, b)
        }
      case Bop(BoolIMPLIES, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), b) => b
          case (Lit(FalseLiteral), _) => Lit(TrueLiteral)
          case (_, Lit(TrueLiteral)) => Lit(TrueLiteral)
          case (a, b) => Bop(BoolIMPLIES, a, b)
        }
      case _ => this
    }
}

enum Predicate {
  case Lit(x: BoolLit)
  case Uop(op: BoolUnOp, x: Predicate)
  case Bop(op: BoolBinOp, x: Predicate, y: Predicate)
  case BVCmp(op: BVCmpOp, x: BVTerm, y: BVTerm)
  case GammaCmp(op: BoolCmpOp, x: GammaTerm, y: GammaTerm)

  assert(this.simplify == this.simplify.simplify)

  def toBoogie: BExpr = this match {
    case Lit(x) => x.toBoogie
    case Uop(op, x) => UnaryBExpr(op, x.toBoogie)
    case Bop(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
    case BVCmp(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
    case GammaCmp(op, x, y) => BinaryBExpr(op, x.toBoogie, y.toBoogie)
  }

  override def toString(): String = this.toBoogie.toString

  def simplify: Predicate =
    this match {
      case Bop(BoolAND, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), b) => b
          case (a, Lit(TrueLiteral)) => a
          case (Lit(FalseLiteral), _) => Lit(FalseLiteral)
          case (_, Lit(FalseLiteral)) => Lit(FalseLiteral)
          case (a, b) => Bop(BoolAND, a, b)
        }
      case Bop(BoolOR, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), _) => Lit(TrueLiteral)
          case (_, Lit(TrueLiteral)) => Lit(TrueLiteral)
          case (Lit(FalseLiteral), b) => b
          case (a, Lit(FalseLiteral)) => a
          case (a, b) => Bop(BoolOR, a, b)
        }
      case Bop(BoolIMPLIES, a, b) =>
        (a.simplify, b.simplify) match {
          case (Lit(TrueLiteral), b) => b
          case (Lit(FalseLiteral), _) => Lit(TrueLiteral)
          case (_, Lit(TrueLiteral)) => Lit(TrueLiteral)
          case (a, b) => Bop(BoolIMPLIES, a, b)
        }
      case BVCmp(op, a, b) =>
        (op, a.simplify, b.simplify) match {
          case (op, a, b) => BVCmp(op, a, b)
        }
      case GammaCmp(op, a, b) =>
        (op, a.simplify, b.simplify) match {
          case (BoolIMPLIES, a, b) if a == b => Lit(TrueLiteral)
          case (op, a, b) => GammaCmp(op, a, b)
        }
      case _ => this
    }

  def split: List[Predicate] =
    this match {
      case Bop(BoolAND, a, b) => a.split ++ b.split
      case _ => List(this)
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
