package ir.eval
import ir.*
import util.Logger
import scala.collection.mutable

import java.io.{BufferedWriter}
import ir.cilvisitor.*

val assocOps: Set[BinOp] =
  Set(BVADD, BVMUL, BVOR, BVAND, BVEQ, BoolAND, BoolEQ, BoolOR, BoolEQUIV, BoolEQ, IntADD, IntMUL, IntEQ)

object AlgebraicSimplifications extends CILVisitor {
  override def vexpr(e: Expr) = ChangeDoChildrenPost(e, eval.simplifyExprFixpoint)

  def apply(e: Expr) = {
    visit_expr(this, e)
  }
}

object SimplifyValidation {
  var traceLog = mutable.LinkedHashSet[(Expr, Expr)]()
  var validate: Boolean = false

  def makeValidation(writer: BufferedWriter) = {

    def makeEQ(a: Expr, b: Expr) = {
      require(a.getType == b.getType)
      a.getType match {
        case BitVecType(sz) => BinaryExpr(BVEQ, a, b)
        case IntType        => BinaryExpr(IntEQ, a, b)
        case BoolType       => BinaryExpr(BoolEQ, a, b)
        case m: MapType     => ???
      }
    }

    var ind = 0

    for ((o, n) <- traceLog) {
      ind += 1
      if (ir.transforms.ExprComplexity()(n) > 5000) {
        Logger.warn(s"Skipping simplification proof $ind because too large (> 5000)!")
      } else {
        if (ind % 100 == 0) Logger.info(s"Wrote simplification proof $ind / ${traceLog.size}")
        val equal = UnaryExpr(BoolNOT, makeEQ(o, n))
        val expr = translating.BasilIRToSMT2.exprUnsat(equal, Some(s"simp$ind"))
        writer.write(expr)
        writer.write("\n\n")
      }
    }
  }

}

def simplifyExprFixpoint(e: Expr): Expr = {
  val begin = e
  var pe = e
  var count = 0
  var ne = e
  var changedAny = false
  var changed = true
  while (changed) {
    val (x, didAnything) = simplifyExpr(ne)
    changed = didAnything
    changedAny = changedAny || changed
    count += 1
    ne = ir.eval.fastPartialEvalExpr(x)
  }
  if (changed) {
    Logger.error(s"stopping simp before fixed point: there is likely a simplificatinon loop: $pe !=== $ne")
  }
  if (SimplifyValidation.validate) {
    // normalise to deduplicate log entries
    val normer = VarNameNormalise()
    val a = visit_expr(normer, begin)
    val b = visit_expr(normer, ne)

    SimplifyValidation.traceLog.add((a, b))
  }
  ne
}

class VarNameNormalise() extends CILVisitor {
  var count = 1
  val assigned = mutable.Map[Variable, Variable]()

  def rename(v: Variable, newName: String) = {
    v match {
      case LocalVar(n, t)  => LocalVar(newName, t)
      case Register(n, sz) => Register(newName, sz)
    }
  }

  override def vlvar(v: Variable) = {
    if (assigned.contains(v)) {
      ChangeTo(assigned(v))
    } else {
      count += 1
      val newName = "Var" + count
      val nv = rename(v, newName)
      assigned(v) = nv
      ChangeTo(nv)
    }
  }

  def apply(e: Expr) = {
    count = 1
    assigned.clear()
    val ne = visit_expr(this, e)
    count = 1
    assigned.clear()
    ne
  }
}

def simplifyExpr(e: Expr): (Expr, Boolean) = {
  // println((0 until indent).map(" ").mkString("") + e)

  def bool2bv1(e: Expr) = {
    e.getType match {
      case BitVecType(1) => e
      case BoolType      => UnaryExpr(BoolToBV1, e) 
      case _             => ???
    }

  }

  val ineqToStrict = Map[BinOp, BinOp](
    BVSGE -> BVSGT,
    BVUGE -> BVUGT,
    BVSLE -> BVSLT,
    BVULE -> BVULT
  )

  def pushExtend(e: Expr, extend: Expr => Expr): Expr = {
    e match {
      case BinaryExpr(op, lhs, rhs) if size(e).isDefined =>
        BinaryExpr(op, pushExtend(lhs, extend), pushExtend(rhs, extend))
      case UnaryExpr(op, v) if size(e).isDefined => UnaryExpr(op, pushExtend(v, extend))
      case l: Literal if size(l).isDefined       => extend(l)
      case v: Variable if size(v).isDefined      => extend(v)
      case e: Extract                            => extend(e)
      case e: SignExtend                         => extend(e)
      case e: ZeroExtend                         => extend(e)
      case o                                     => o
    }
  }

  def bvLogOpToBoolOp = Map[BinOp, BinOp](
    // logical ops when bv1
    BVAND -> BoolAND,
    BVOR -> BoolOR
  )

  /** Apply the rewrite rules once. Note some rules expect a canonical form produced by other rules, and hence this is
    * more effective when applied iteratively until a fixed point.
    */
  var didAnything = true
  val simped = e match {
    // constant folding
    // const + (expr + const) -> expr + (const + const)

    case BinaryExpr(BVADD, BinaryExpr(BVADD, body, l: BitVecLiteral), r: BitVecLiteral)
        if !body.isInstanceOf[Literal] =>
      BinaryExpr(BVADD, (body), (BinaryExpr(BVADD, r, l)))

    //case BinaryExpr(BVADD, BinaryExpr(BVADD, body1, r1: BitVecLiteral), BinaryExpr(BVADD, body2, r2)) => {
    //  BinaryExpr(BVADD, BinaryExpr(BVADD, body1, body2), BinaryExpr(BVADD, r1, r2))
    //}

    case BinaryExpr(BVMUL, BinaryExpr(BVMUL, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVMUL, BinaryExpr(BVMUL, l, r), (body))

    case BinaryExpr(BVOR, BinaryExpr(BVOR, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVOR, BinaryExpr(BVOR, l, r), (body))
    case BinaryExpr(BVAND, BinaryExpr(BVAND, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVAND, BinaryExpr(BVAND, l, r), (body))

    case BinaryExpr(BVADD, BinaryExpr(BVADD, l, lc: BitVecLiteral), BinaryExpr(BVADD, r, rc: BitVecLiteral)) =>
      BinaryExpr(BVADD, (BinaryExpr(BVADD, l, r)), (BinaryExpr(BVADD, lc, rc)))
    // normalise
    // make all comparisons positive so double negatives can be cleaned up
    case BinaryExpr(BVEQ, UnaryExpr(BVNOT, BinaryExpr(BVCOMP, x, y)), BitVecLiteral(1, 1)) =>
      UnaryExpr(BoolNOT, BinaryExpr(BVEQ, BinaryExpr(BVCOMP, (x), (y)), BitVecLiteral(1, 1)))
    case BinaryExpr(BVEQ, l, BitVecLiteral(0, 1)) =>
      UnaryExpr(BoolNOT, BinaryExpr(BVEQ, (l), BitVecLiteral(1, 1)))
    case BinaryExpr(BVEQ, BinaryExpr(BVCOMP, e1: Expr, e2: Expr), BitVecLiteral(1, 1)) =>
      BinaryExpr(BVEQ, (e1), (e2))
    case BinaryExpr(BVEQ, BinaryExpr(BVCOMP, e1: Expr, e2: Expr), BitVecLiteral(0, 1)) =>
      UnaryExpr(BoolNOT, BinaryExpr(BVEQ, (e1), (e2)))
    case BinaryExpr(BVNEQ, e1, e2) => UnaryExpr(BoolNOT, BinaryExpr(BVEQ, (e1), (e2)))

    case BinaryExpr(op, BinaryExpr(op1, a, b: Literal), BinaryExpr(op2, c, d: Literal))
        if !a.isInstanceOf[Literal] && !c.isInstanceOf[Literal]
          && assocOps.contains(op) && op == op1 && op == op2 =>
      BinaryExpr(op, BinaryExpr(op, a, c), BinaryExpr(op, b, d))

    case BinaryExpr(op, x: Literal, y: Expr) if !y.isInstanceOf[Literal] && assocOps.contains(op) =>
      BinaryExpr(op, (y), (x))

    case BinaryExpr(BVADD, UnaryExpr(BVNOT, x), BitVecLiteral(1, _)) => UnaryExpr(BVNEG, x)

    case BinaryExpr(BVADD, BinaryExpr(BVADD, y, UnaryExpr(BVNOT, x)), BitVecLiteral(1, _))
        if !(y.isInstanceOf[BitVecLiteral]) =>
      BinaryExpr(BVADD, y, UnaryExpr(BVNEG, x))
    case BinaryExpr(BVADD, BinaryExpr(BVADD, y, ed @ SignExtend(sz, UnaryExpr(BVNOT, x))), BitVecLiteral(1, sz2))
        if size(ed).contains(sz2) && !(y.isInstanceOf[BitVecLiteral]) =>
      BinaryExpr(BVADD, y, UnaryExpr(BVNEG, SignExtend(sz, x)))

    /*
     * Simplify BVop to Bool ops in a boolean context.
     */

    /* intro bool2bv */
    case BinaryExpr(
          BVCOMP,
          l,
          r
        ) => {
      bool2bv1(BinaryExpr(BVEQ, l, r))
    }
    /* push bool2bv upwards */
    case BinaryExpr(
          bop,
          UnaryExpr(BoolToBV1, l),
          UnaryExpr(BoolToBV1, r),
        ) if bvLogOpToBoolOp.contains(bop) => {
      bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r)))
    }
    case BinaryExpr(
          bop,
          UnaryExpr(BoolToBV1, l),
          UnaryExpr(BoolToBV1, r),
        ) if bvLogOpToBoolOp.contains(bop) => {
      bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), (l), (r)))
    }

    case UnaryExpr(BVNOT, UnaryExpr(BoolToBV1, arg)) =>
      bool2bv1(UnaryExpr(BoolNOT, arg))

    /* remove bool2bv in boolean context */
    case BinaryExpr(BVEQ, UnaryExpr(BoolToBV1, body),  BitVecLiteral(1, 1)) => body
    case BinaryExpr(BVEQ, UnaryExpr(BoolToBV1, l), UnaryExpr(BoolToBV1, r)) => BinaryExpr(BoolEQ, (l), (r))
    case UnaryExpr(BoolToBV1, FalseLiteral) => BitVecLiteral(0, 1)
    case UnaryExpr(BoolToBV1, TrueLiteral)  => BitVecLiteral(1, 1)

    case BinaryExpr(BoolAND, x, TrueLiteral)  => x
    case BinaryExpr(BoolAND, x, FalseLiteral) => FalseLiteral
    case BinaryExpr(BoolOR, x, FalseLiteral)  => x
    case BinaryExpr(BoolOR, x, TrueLiteral)   => TrueLiteral

    case BinaryExpr(BVEQ, BinaryExpr(BVADD, x, y: BitVecLiteral), BitVecLiteral(0, s))
        if ir.eval.BitVectorEval.isNegative(y) =>
      BinaryExpr(BVEQ, x, UnaryExpr(BVNEG, y))

    case BinaryExpr(BVCONCAT, BitVecLiteral(0, sz), expr) => ZeroExtend(sz, expr)
    case BinaryExpr(BVCONCAT, expr, BitVecLiteral(0, sz)) if (BigInt(2).pow(sz + size(expr).get) > sz) =>
      BinaryExpr(BVSHL, ZeroExtend(sz, expr), BitVecLiteral(sz, sz + size(expr).get))

    /*  COMPARISON FLAG HANDLING
     *
     * We quite precisely pattern match ASLp's output for C and V,
     * these are computed by comparing the test to a higher-precision calculation of the test.
     */

    // NF check on expr
    case Extract(upper, lower, b) if size(b).contains(upper) && (upper == (lower + 1)) => {
      bool2bv1(BinaryExpr(BVSLT, (b), BitVecLiteral(0, size(b).get)))
    }

    /** https://developer.arm.com/documentation/dui0801/l/Condition-Codes/Condition-code-suffixes-and-related-flags
      *
      * match NF == VF
      *
      */
    case BinaryExpr(
     // add case
          BoolEQ,
          // N set
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BVEQ,
              SignExtend(exts, orig @ BinaryExpr(o1, x1, y1)),
              compar @ BinaryExpr(o2, x2, y2)
            ) // high precision op
          )
        )
      if sz > 1 && (o1 == o2) && o1 == BVADD && (lhs) == (orig)
          && AlgebraicSimplifications(SignExtend(exts, x1)) == x2
          && AlgebraicSimplifications(SignExtend(exts, y1)) == y2 => {
      BinaryExpr(BVSGE, x1, UnaryExpr(BVNEG, y1))
    }

    case BinaryExpr(
        // sub case (with two args)
          BoolEQ,
          // N set
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BVEQ,
              SignExtend(exts, orig @ BinaryExpr(o1, x1, UnaryExpr(BVNEG, y1))),
              compar @ BinaryExpr(o2, SignExtend(ext1, x2), UnaryExpr(BVNEG, SignExtend(ext2, y2)))
            ) // high precision op
          )
        )
        if (o1 == o2) && o1 == BVADD && (lhs) == (orig)
          && exts == ext1 && exts == ext2
          && x2 == x1
          && y2 == y1 => {

      BinaryExpr(BVSGE, x1, y1)
    }

    // NF == VF
    case BinaryExpr(
          // this matches sub case a - b ===> (x1 + (bvneg y1)) + 1
          BoolEQ,
          // N set
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          // V set
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BVEQ,
              SignExtend(exts, orig @ BinaryExpr(o1, BinaryExpr(o3, x1, y1), z1)),
              BinaryExpr(o2, compar @ BinaryExpr(o4, x2, y2), z2) // high precision op
            )
          )
        )
        if sz > 1 && (o1 == o2) && o2 == o4 && o1 == BVADD && (lhs) == (orig)
          && AlgebraicSimplifications(x2) == AlgebraicSimplifications(SignExtend(exts, x1))
          && AlgebraicSimplifications(y2) == AlgebraicSimplifications(SignExtend(exts, y1))
          && AlgebraicSimplifications(z2) == AlgebraicSimplifications(SignExtend(exts, z1)) => {
      BinaryExpr(BVSGE, x1, UnaryExpr(BVNEG, BinaryExpr(BVADD, y1, z1)))
    }

    // CF Unsigned Overflow
    case BinaryExpr(
          BVEQ,
          ZeroExtend(exts, orig @ BinaryExpr(o1, x1, y1)),
          compar @ BinaryExpr(o2, x2, y2)
        )
      if size(x1).get > 1 && (o1 == o2) && o1 == BVADD
          && AlgebraicSimplifications(x2) == AlgebraicSimplifications(ZeroExtend(exts, x1))
          && AlgebraicSimplifications(y2) == AlgebraicSimplifications(ZeroExtend(exts, y1)) => {
      // C not Set
      UnaryExpr(BoolNOT, BinaryExpr(BVUGE, x1, UnaryExpr(BVNEG, y1)))
    }

    case BinaryExpr(
          BVEQ,
          ZeroExtend(exts, orig @ BinaryExpr(o1, BinaryExpr(o3, x1, y1), z1)),
          BinaryExpr(o2, compar @ BinaryExpr(o4, x2, y2), z2) // high precision op
        )
        if size(x1).get > 1 && (o1 == o2) && o2 == o4 && o1 == BVADD
          && (x2) == (ZeroExtend(exts, x1))
          && (y2) == (ZeroExtend(exts, y1))
          && (z2) == (ZeroExtend(exts, z1)) => {
      // C not Set
      UnaryExpr(BoolNOT, BinaryExpr(BVUGE, x1, UnaryExpr(BVNEG, BinaryExpr(BVADD, y1, z1))))
    }

    case BinaryExpr(
          BVEQ,
          ZeroExtend(exts, orig @ BinaryExpr(o1, x1, UnaryExpr(BVNEG, y1))),
          BinaryExpr(o2, compar @ BinaryExpr(o4, ZeroExtend(ext1, x2), ZeroExtend(ext2, UnaryExpr(BVNOT, y2))), BitVecLiteral(1, _)) // high precision op
        )
        if size(x1).get > 1 && (o1 == o2) && o2 == o4 && o1 == BVADD
          && exts == ext1 && exts == ext2
          && x1 == x2 && y1 == y2 => {
      // C not Set
      UnaryExpr(BoolNOT, BinaryExpr(BVUGE, x1, y1))
    }

    /* generic comparison simplification */

    // weak to strict inequality
    // x >= 0 && x != 0 ===> x > 0
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, BitVecLiteral(0, sz)), UnaryExpr(BoolNOT, rhs))
        if size(lhs).isDefined && (AlgebraicSimplifications(BinaryExpr(BVEQ, lhs, BitVecLiteral(0, size(lhs).get))) == rhs) && ineqToStrict.contains(op) => {
      BinaryExpr(ineqToStrict(op), lhs, BitVecLiteral(0, size(lhs).get))
    }
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, rhs), UnaryExpr(BoolNOT, BinaryExpr(BVEQ, lhs2, rhs2))) 
      if lhs == lhs2 && rhs == rhs2  && ineqToStrict.contains(op) => {
      BinaryExpr(ineqToStrict(op), lhs, rhs)
    }
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, rhs), UnaryExpr(BoolNOT, BinaryExpr(BVEQ, BinaryExpr(BVADD, lhs2, rhs2), BitVecLiteral(0, _))))
      if lhs == lhs2 && AlgebraicSimplifications(rhs) == AlgebraicSimplifications(UnaryExpr(BVNEG, rhs2)) && ineqToStrict.contains(op) => {
      BinaryExpr(ineqToStrict(op), lhs, rhs)
    }
    case BinaryExpr(BoolAND, BinaryExpr(op, lhs, rhs), UnaryExpr(BoolNOT, BinaryExpr(BVEQ, BinaryExpr(BVADD, lhs2, rhs2), BitVecLiteral(0, _))))
      if rhs == rhs2 && (AlgebraicSimplifications(lhs) == AlgebraicSimplifications(UnaryExpr(BVNEG, lhs2))) && ineqToStrict.contains(op) => {
      BinaryExpr(ineqToStrict(op), lhs, rhs)
    }

    // inequality negation
    case UnaryExpr(BoolNOT, BinaryExpr(BVSLT, lhs, rhs)) => BinaryExpr(BVSGE, lhs, rhs)
    case UnaryExpr(BoolNOT, BinaryExpr(BVSGT, lhs, rhs)) => BinaryExpr(BVSLE, lhs, rhs)
    case UnaryExpr(BoolNOT, BinaryExpr(BVULT, lhs, rhs)) => BinaryExpr(BVUGE, lhs, rhs)
    case UnaryExpr(BoolNOT, BinaryExpr(BVUGT, lhs, rhs)) => BinaryExpr(BVULE, lhs, rhs)
    case UnaryExpr(BoolNOT, BinaryExpr(BVSLE, lhs, rhs)) => BinaryExpr(BVSGT, lhs, rhs)
    case UnaryExpr(BoolNOT, BinaryExpr(BVSGE, lhs, rhs)) => BinaryExpr(BVSLT, lhs, rhs)
    case UnaryExpr(BoolNOT, BinaryExpr(BVULE, lhs, rhs)) => BinaryExpr(BVUGT, lhs, rhs)
    case UnaryExpr(BoolNOT, BinaryExpr(BVUGE, lhs, rhs)) => BinaryExpr(BVULT, lhs, rhs)

    // tighten inequality bounds
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVSLT, x, y), (BinaryExpr(BVSLT, z, y2))) if y == y2 => {
      BinaryExpr(BVSLT, x, z)
    }
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVULT, x, y), (BinaryExpr(BVULT, z, y2))) if y == y2 => {
      BinaryExpr(BVULT, x, z)
    }
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVULT, x, y), (BinaryExpr(BVULT, x2, z))) if x == x2 => {
      AlgebraicSimplifications(BinaryExpr(BVULT, y, z)) match {
        case TrueLiteral  => BinaryExpr(BVULT, x, y)
        case FalseLiteral => BinaryExpr(BVULT, x, z)
        case _            => e
      }
    }
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVSLT, x, y), (BinaryExpr(BVSLT, x2, z))) if x == x2 => {
      AlgebraicSimplifications(BinaryExpr(BVSLT, y, z)) match {
        case TrueLiteral  => BinaryExpr(BVSLT, x, y)
        case FalseLiteral => BinaryExpr(BVSLT, x, z)
        case _            => e
      }
    }

    // identities
    case Extract(ed, 0, body) if (body.getType == BitVecType(ed))                        => (body)
    case ZeroExtend(0, body)                                                             => (body)
    case SignExtend(0, body)                                                             => (body)
    case BinaryExpr(BVADD, body, BitVecLiteral(0, _))                                    => (body)
    case BinaryExpr(BVMUL, body, BitVecLiteral(1, _))                                    => (body)
    case Repeat(1, body)                                                                 => (body)
    case Extract(ed, 0, ZeroExtend(extension, body)) if (body.getType == BitVecType(ed)) => (body)
    case Extract(ed, 0, SignExtend(extension, body)) if (body.getType == BitVecType(ed)) => (body)

    case BinaryExpr(BVXOR, l, r) if l == r =>
      e.getType match {
        case BitVecType(sz) => BitVecLiteral(0, sz)
        case _              => throw Exception("Type error (should be unreachable)")
      }
    case BinaryExpr(BoolEQ, x, FalseLiteral) => UnaryExpr(BoolNOT, x)

    // redundant extends
    // extract extended zero part
    case Extract(ed, bg, ZeroExtend(x, expr)) if (bg > size(expr).get) => BitVecLiteral(0, ed - bg)
    // extract below extend
    case Extract(ed, bg, ZeroExtend(x, expr)) if (bg < size(expr).get) && (ed < size(expr).get) => Extract(ed, bg, expr)
    case Extract(ed, bg, SignExtend(x, expr)) if (bg < size(expr).get) && (ed < size(expr).get) => Extract(ed, bg, expr)

    case BinaryExpr(BVEQ, ZeroExtend(sz, expr), BitVecLiteral(0, sz2)) => BinaryExpr(BVEQ, expr, BitVecLiteral(0, size(expr).get))



    // double negation
    case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body))     => (body)
    case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body))     => (body)
    case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => (body)

    // syntactic equality
    case BinaryExpr(BVEQ, a, b) if a.loads.isEmpty && b.loads.isEmpty && a == b => TrueLiteral

    // compose slices
    case Extract(ed1, be1, Extract(ed2, be2, body)) => Extract(ed1 + be2, be1 + be2, (body))
    case SignExtend(sz1, SignExtend(sz2, exp)) => SignExtend(sz1 + sz2, exp)
    case ZeroExtend(sz1, ZeroExtend(sz2, exp)) => ZeroExtend(sz1 + sz2, exp)

    // (comp (comp x y) 1) = (comp x y)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(1, 1)) => (body)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)) =>
      UnaryExpr(BVNOT, (body))
    case BinaryExpr(
          BVEQ,
          BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)),
          BitVecLiteral(1, 1)
        ) =>
      BinaryExpr(BVEQ, (body), BitVecLiteral(0, 1))

    case BinaryExpr(BVSUB, x: Expr, y: BitVecLiteral) => BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y))
    case r => {
      didAnything = false
      r
    }
  }

  (simped, didAnything)
}
