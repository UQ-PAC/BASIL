package analysis

import ir.*
import ir.eval.BitVectorEval.*

// Signed infinity
private def sInf(size: Int): BigInt = BigInt(2).pow(size-1)-1
// Signed negative infinity
private def sNInf(size: Int): BigInt = -BigInt(2).pow(size-1)
// Unsigned infinity
private def uInf(size: Int): BigInt = BigInt(2).pow(size)-1
// Unsigned "negative infinity"
private def uNInf(size: Int): BigInt = 0

def signedInt2bv(bitSize: Int, n: BigInt): BitVecLiteral =
  require(bitSize > 0, "length of bitvector must be positive")
  require(sNInf(bitSize) <= n && n <= sInf(bitSize), "input must be within bounds")

  if n < 0 then smt_bvneg(BitVecLiteral((-n) % BigInt(2).pow(bitSize), bitSize)) else BitVecLiteral(n % BigInt(2).pow(bitSize), bitSize)

enum Interval extends InternalLattice[Interval] {
  case Top
  case ConcreteInterval(lower: BigInt, upper: BigInt, width: Int)
  case Bottom

  import ir.eval.BitVectorEval.*

  assert(this match {
    case ConcreteInterval(lower, upper, width) => lower <= upper
    case _ => true
  })

  def join(other: Interval): Interval =
    (this, other) match {
      case (Top, b) => Top
      case (Bottom, b) => b
      case (ConcreteInterval(l1, u1, w1), ConcreteInterval(l2, u2, w2)) if w1 == w2 => ConcreteInterval(l1.min(l2), u1.max(u2), w1)
      case (ConcreteInterval(l1, u1, w1), ConcreteInterval(l2, u2, w2)) if w1 != w2 => throw Exception("Joining intervals of mismatching bitvector sizes")
      case (a, b) => b.join(a)
    }
  def meet(other: Interval): Interval =
    (this, other) match {
      case (Top, b) => b
      case (Bottom, b) => Bottom
      case (ConcreteInterval(l1, u1, w1), ConcreteInterval(l2, u2, w2)) if w1 == w2 => {
        val max = l1.max(l2)
        val min = u1.min(u2)
        if max <= min then ConcreteInterval(max, min, w1) else Bottom
      }
      case (ConcreteInterval(l1, u1, w1), ConcreteInterval(l2, u2, w2)) if w1 != w2 => throw Exception("Meeting intervals of mismatching bitvector sizes")
      case (a, b) => b.meet(a)
    }

  def top: Interval = Top
  def bottom: Interval = Bottom
}

private implicit val intervalTerm: Interval = Interval.Bottom

class IntervalDomain(signed: Boolean, inf: Int => BigInt, negInf: Int => BigInt, bvto: BitVecLiteral => BigInt, tobv: (Int, BigInt) => BitVecLiteral)
  extends MayPredMapDomain[Variable, Interval] {
  import Interval.*
  import ir.eval.BitVectorEval.*

  def joinTerm(a: Interval, b: Interval, pos: Block): Interval = a.join(b)

  override def widenTerm(a: Interval, b: Interval, pos: Block): Interval =
    (a, b) match {
      case (ConcreteInterval(l1, u1, w1), ConcreteInterval(l2, u2, w2)) if w1 != w2 => throw Exception("Widening intervals of mismatching bitvector sizes")
      case (ConcreteInterval(l1, u1, w1), ConcreteInterval(l2, u2, w2)) if w1 == w2 =>
        ConcreteInterval(if l1 <= l2 then l1 else negInf(w1), if u1 >= u2 then u1 else inf(w1), w1)
      case (a, b) => joinTerm(a, b, pos) // TODO?
    }

  def transfer(b: LatticeMap[Variable, Interval], c: Command): LatticeMap[Variable, Interval] = {
    c match {
      case c: LocalAssign  => b + (c.lhs -> eval(c.rhs, b))
      case c: MemoryLoad   => b + (c.lhs -> Top)
      case c: MemoryStore  => b
      case c: Assume       => exprToPredicate(c.body).map(p => b.meet(fromPred(p))).getOrElse(b)
      case c: Assert       => exprToPredicate(c.body).map(p => b.meet(fromPred(p))).getOrElse(b)
      case c: IndirectCall => top
      case c: DirectCall   => top
      case c: GoTo         => b
      case c: Return       => b ++ c.outParams.map((l, e) => l -> eval(e, b))
      case c: Unreachable  => b
      case c: NOP          => b
    }
  }

  override def init(b: Block): LatticeMap[Variable, Interval] =
    if Some(b) == b.parent.entryBlock then
      b.parent.formalInParam.foldLeft(top) {
        (m, v) => m + (v -> Top)
      }
    else bot

  def topTerm: Interval = Top
  def botTerm: Interval = Bottom

  def termToPred(m: LatticeMap[Variable, Interval], v: Variable, l: Interval): Predicate = l match {
    case Top => Predicate.Lit(TrueLiteral)
    case Bottom => Predicate.Lit(TrueLiteral)
    case ConcreteInterval(lower, upper, width) if signed =>
      Predicate.Bop(BoolAND,
        Predicate.BVCmp(BVSLE, BVTerm.Lit(tobv(width, lower)), BVTerm.Var(v)),
        Predicate.BVCmp(BVSLE, BVTerm.Var(v), BVTerm.Lit(tobv(width, upper))))
    case ConcreteInterval(lower, upper, width) /* if !signed */ =>
      Predicate.Bop(BoolAND,
        Predicate.BVCmp(BVULE, BVTerm.Lit(tobv(width, lower)), BVTerm.Var(v)),
        Predicate.BVCmp(BVULE, BVTerm.Var(v), BVTerm.Lit(tobv(width, upper))))
  }

  def eval(e: Expr, m: LatticeMap[Variable, Interval]): Interval = {
    val i = e match {
      case x: BitVecLiteral => {
        val y = bvto(x)
        ConcreteInterval(y, y, x.size)
      }
      case v: Variable => m(v)
      case UnaryExpr(op, arg) => eval(arg, m) match {
        case ConcreteInterval(lower, upper, width) => op match {
          case BVNEG => ConcreteInterval(-upper, -lower, width)
          // -x = ~x + 1, so ~x = -x - 1
          case BVNOT => ConcreteInterval(-upper - 1, -lower - 1, width)
          case _ => Top
        }
        case _ => Top
      }
      case BinaryExpr(op, arg1, arg2) => (eval(arg1, m), eval(arg2, m)) match {
        case (ConcreteInterval(l1, u1, w1), ConcreteInterval(l2, u2, w2)) if w1 == w2 => op match {
          case BVADD => ConcreteInterval(l1 + l2, u1 + u2, w1)
          case BVSUB => ConcreteInterval(l1 - u2, u1 - l2, w1)
          // TODO
          // see 4.3 of Warren Jr's "Hacker's Delight" for bitwise operation bounds
          case _ => Top
        }
        case _ => Top
      }
      case _ => Top
    }
    i match {
      case ConcreteInterval(lower, upper, width) => if negInf(width) <= lower && upper <= inf(width) then i else Top
      case i => i
    }
  }

  override def fromPred(p: Predicate): LatticeMap[Variable, Interval] = {
    import Predicate.*
    p match {
      case Bop(BoolAND, x, y) => fromPred(x).meet(fromPred(y))
      case Bop(BoolOR, x, y) => fromPred(x).join(fromPred(y))
      case Conj(s) =>
        if s.size == 0 then top
        else if s.size == 1 then fromPred(s.head)
        else s.tail.foldLeft(fromPred(s.head)) { (i, p) => i.meet(fromPred(p)) }
      case Disj(s) =>
        if s.size == 0 then bot
        else if s.size == 1 then fromPred(s.head)
        else s.tail.foldLeft(fromPred(s.head)) { (i, p) => i.join(fromPred(p)) }

      case BVCmp(BVEQ, BVTerm.Lit(x), BVTerm.Var(v)) => top + (v -> ConcreteInterval(bvto(x), bvto(x), x.size))
      case BVCmp(BVEQ, BVTerm.Var(v), BVTerm.Lit(x)) => top + (v -> ConcreteInterval(bvto(x), bvto(x), x.size))

      case BVCmp(BVSLE, BVTerm.Lit(x), BVTerm.Var(v)) if signed => top + (v -> ConcreteInterval(bvto(x), inf(x.size), x.size))
      case BVCmp(BVSGE, BVTerm.Lit(x), BVTerm.Var(v)) if signed => top + (v -> ConcreteInterval(negInf(x.size), bvto(x), x.size))
      case BVCmp(BVSLE, BVTerm.Var(v), BVTerm.Lit(x)) if signed => top + (v -> ConcreteInterval(negInf(x.size), bvto(x), x.size))
      case BVCmp(BVSGE, BVTerm.Var(v), BVTerm.Lit(x)) if signed => top + (v -> ConcreteInterval(bvto(x), inf(x.size), x.size))

      case BVCmp(BVSLT, BVTerm.Lit(x), BVTerm.Var(v)) if signed && inf(x.size) >= bvto(x) => top + (v -> ConcreteInterval(bvto(x) + 1, inf(x.size), x.size))
      case BVCmp(BVSGT, BVTerm.Lit(x), BVTerm.Var(v)) if signed && negInf(x.size) <= bvto(x) => top + (v -> ConcreteInterval(negInf(x.size), bvto(x) - 1, x.size))
      case BVCmp(BVSLT, BVTerm.Var(v), BVTerm.Lit(x)) if signed && negInf(x.size) <= bvto(x) => top + (v -> ConcreteInterval(negInf(x.size), bvto(x) - 1, x.size))
      case BVCmp(BVSGT, BVTerm.Var(v), BVTerm.Lit(x)) if signed && inf(x.size) >= bvto(x) => top + (v -> ConcreteInterval(bvto(x) + 1, inf(x.size), x.size))

      case BVCmp(BVULE, BVTerm.Lit(x), BVTerm.Var(v)) if !signed => top + (v -> ConcreteInterval(bvto(x), inf(x.size), x.size))
      case BVCmp(BVUGE, BVTerm.Lit(x), BVTerm.Var(v)) if !signed => top + (v -> ConcreteInterval(negInf(x.size), bvto(x), x.size))
      case BVCmp(BVULE, BVTerm.Var(v), BVTerm.Lit(x)) if !signed => top + (v -> ConcreteInterval(negInf(x.size), bvto(x), x.size))
      case BVCmp(BVUGE, BVTerm.Var(v), BVTerm.Lit(x)) if !signed => top + (v -> ConcreteInterval(bvto(x), inf(x.size), x.size))

      case BVCmp(BVULT, BVTerm.Lit(x), BVTerm.Var(v)) if !signed && inf(x.size) >= bvto(x) => top + (v -> ConcreteInterval(bvto(x) + 1, inf(x.size), x.size))
      case BVCmp(BVUGT, BVTerm.Lit(x), BVTerm.Var(v)) if !signed && negInf(x.size) <= bvto(x) => top + (v -> ConcreteInterval(negInf(x.size), bvto(x) - 1, x.size))
      case BVCmp(BVULT, BVTerm.Var(v), BVTerm.Lit(x)) if !signed && negInf(x.size) <= bvto(x) => top + (v -> ConcreteInterval(negInf(x.size), bvto(x) - 1, x.size))
      case BVCmp(BVUGT, BVTerm.Var(v), BVTerm.Lit(x)) if !signed && inf(x.size) >= bvto(x) => top + (v -> ConcreteInterval(bvto(x) + 1, inf(x.size), x.size))

      case BVCmp(op, x, y) => top
      case Bop(op, x, y) => top
      case Lit(TrueLiteral) => top
      case Lit(FalseLiteral) => bot
      case Not(x) => top
      case GammaCmp(_, _, _) => top
    }
  }
}

class SignedIntervalDomain extends IntervalDomain(true, sInf, sNInf, bv2SignedInt, signedInt2bv)
class UnsignedIntervalDomain extends IntervalDomain(false, uInf, uNInf, bv2nat, nat2bv)

class DoubleIntervalDomain extends PredProductDomain(SignedIntervalDomain(), UnsignedIntervalDomain())
