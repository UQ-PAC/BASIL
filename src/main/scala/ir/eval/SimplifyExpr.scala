package ir.eval
import ir.*

val assocOps: Set[BinOp] =
  Set(BVADD, BVMUL, BVOR, BVAND, BVEQ, BoolAND, BoolEQ, BoolOR, BoolEQUIV, BoolEQ, IntADD, IntMUL, IntEQ)

def simplifyExprFixpoint(e: Expr): Expr = {
  var pe = e
  var ne = simplifyExpr(pe)
  while (ne != pe) {
    pe = ne
    ne = simplifyExpr(pe)
  }
  ne
}

def conditionSimplify(e: Expr): Expr = {
  val s = simplifyExpr(e)
  s match {
    case r => r
  }
}

def simplifyExpr(e: Expr): Expr = {
  // println((0 until indent).map(" ").mkString("") + e)

  def bool2bv1(e: Expr) = {
    e.getType match {
      case BitVecType(1) => e
      case BoolType      => UninterpretedFunction("bool2bv1", Seq(e), BitVecType(1))
      case _             => ???
    }

  }

  def isNegBV(e: Expr) = {
    simplifyExpr(e).getType match {
      case BitVecType(sz) => {
        BinaryExpr(BVSLT, e, BitVecLiteral(0, sz))
      }
      case _ => ???
    }
  }

  def getDisjuncts(e: Expr): List[Expr] = {
    e match {
      case BinaryExpr(BoolOR, l, r) => getDisjuncts(l) ++ getDisjuncts(r)
      case e                        => List(e)
    }
  }

  def isFalse(e: Expr): Boolean = {
    e match {
      case FalseLiteral                                                                                      => true
      case BinaryExpr(BoolAND, e1, UnaryExpr(BoolNOT, e2)) if e1 == e2                                       => true
      case BinaryExpr(BoolOR, e1, e2) if isFalse(e1) && isFalse(e2)                                          => true
      case BinaryExpr(BoolAND, e1, e2) if isFalse(e1) || isFalse(e2)                                         => true
      case BinaryExpr(BoolAND, BinaryExpr(BVSLT, e1, e2), BinaryExpr(BVSLT, e3, e4)) if e1 == e4 && e2 == e3 => true
      case _                                                                                                 => false
    }

  }

  def resolveAND(e1: Expr, e2: Expr): Expr = {
    require((e1.loads.size == 0) && (e2.loads.size == 0))

    (e1, e2) match {
      case (e, UnaryExpr(BoolNOT, ep)) if e == ep => FalseLiteral
      //case (left, right) => {

      //  val ld = getDisjuncts(left)
      //  val rd = getDisjuncts(right)
      //  val prod = ld.flatMap(l => rd.map(r => (l, r)))

      //  val rs = prod.map( p => p -> resolveAND(p._1, p._2)).filter(c => c._2 != FalseLiteral)
      //    .map(_._1).map(c => Set(c._1, c._2)).flatten

      //  if (rs.size == 0) {
      //    FalseLiteral
      //  } else {
      //    rs.tail.foldLeft(rs.head)((l, r) => BinaryExpr(BoolOR, l , r))
      //  }
      //}
      case (BinaryExpr(BoolOR, a, b), e) => {
        BinaryExpr(BoolOR, resolveAND(a, e), resolveAND(b, e))
      }
      case (e1, e2) => BinaryExpr(BoolAND, e1, e2)
      //case (BinaryExpr(BoolOR, a, b), BinaryExpr(BoolOR, c, d)) if (resolveAND(a, d) != FalseLiteral) && (resolveAND(b, c) == FalseLiteral) => BinaryExpr
      //case (BinaryExpr(BoolOR, a, b), BinaryExpr(BoolOR, c, d)) if (resolveAND(a, c) != FalseLiteral) && (resolveAND(c, d) == FalseLiteral) => BinaryExpr(BoolOR, a, c)
    }
  }

  val e2 = ir.eval.partialEvalExpr(e, (v) => None)

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
    //BVCOMP -> BoolEQ
  )

  /** Apply the rewrite rules once. Note some rules expect a canonical form produced by other rules, and hence this is
    * more effective when applied iteratively until a fixed point.
    */
  val simped = e2 match {
    // constant folding
    // const + (expr + const) -> expr + (const + const)

    //case UnaryExpr(BVNOT, l) if size(l).isDefined && !size(l).contains(1) =>
    //  BinaryExpr(BVSUB, UnaryExpr(BVNEG, simplifyExpr(l)), BitVecLiteral(1, size(l).get))

    case BinaryExpr(BVADD, BinaryExpr(BVSUB, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVADD, BinaryExpr(BVSUB, r, l), simplifyExpr(body))
    case BinaryExpr(BVSUB, BinaryExpr(BVADD, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVADD, BinaryExpr(BVSUB, r, l), simplifyExpr(body))
    case BinaryExpr(BVSUB, BinaryExpr(BVSUB, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVSUB, simplifyExpr(body), BinaryExpr(BVADD, l, r))
    case BinaryExpr(BVADD, BinaryExpr(BVADD, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVADD, BinaryExpr(BVADD, l, r), simplifyExpr(body))
    case BinaryExpr(BVMUL, BinaryExpr(BVMUL, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVMUL, BinaryExpr(BVMUL, l, r), simplifyExpr(body))
    case BinaryExpr(BVOR, BinaryExpr(BVOR, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVOR, BinaryExpr(BVOR, l, r), simplifyExpr(body))
    case BinaryExpr(BVAND, BinaryExpr(BVAND, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVAND, BinaryExpr(BVAND, l, r), simplifyExpr(body))
    case BinaryExpr(BVADD, BinaryExpr(BVADD, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVADD, BinaryExpr(BVADD, r, l), simplifyExpr(body))

    case BinaryExpr(BVADD, BinaryExpr(BVADD, l, lc: BitVecLiteral), BinaryExpr(BVADD, r, rc: BitVecLiteral)) =>
      BinaryExpr(BVADD, simplifyExpr(BinaryExpr(BVADD, l, r)), simplifyExpr(BinaryExpr(BVADD, lc, rc)))
    // normalise
    // make all comparisons positive so double negatives can be cleaned up
    case BinaryExpr(BVEQ, UnaryExpr(BVNOT, BinaryExpr(BVCOMP, x, y)), BitVecLiteral(1, 1)) =>
      UnaryExpr(BoolNOT, BinaryExpr(BVEQ, BinaryExpr(BVCOMP, simplifyExpr(x), simplifyExpr(y)), BitVecLiteral(1, 1)))
    case BinaryExpr(BVEQ, l, BitVecLiteral(0, 1)) =>
      UnaryExpr(BoolNOT, BinaryExpr(BVEQ, simplifyExpr(l), BitVecLiteral(1, 1)))
    case BinaryExpr(BVEQ, BinaryExpr(BVCOMP, e1: Expr, e2: Expr), BitVecLiteral(1, 1)) =>
      BinaryExpr(BVEQ, simplifyExpr(e1), simplifyExpr(e2))
    case BinaryExpr(BVEQ, BinaryExpr(BVCOMP, e1: Expr, e2: Expr), BitVecLiteral(0, 1)) =>
      UnaryExpr(BoolNOT, BinaryExpr(BVEQ, simplifyExpr(e1), simplifyExpr(e2)))
    case BinaryExpr(BVNEQ, e1, e2) => UnaryExpr(BoolNOT, BinaryExpr(BVEQ, simplifyExpr(e1), simplifyExpr(e2)))

    case BinaryExpr(op, BinaryExpr(op1, a, b: Literal), BinaryExpr(op2, c, d: Literal))
        if !a.isInstanceOf[Literal] && !c.isInstanceOf[Literal]
          && assocOps.contains(op) && op == op1 && op == op2 =>
      BinaryExpr(op, BinaryExpr(op, a, c), BinaryExpr(op, b, d))

    case BinaryExpr(op, x: Literal, y: Expr) if !y.isInstanceOf[Literal] && assocOps.contains(op) =>
      BinaryExpr(op, simplifyExpr(y), simplifyExpr(x))

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
          UninterpretedFunction("bool2bv1", Seq(l), _),
          UninterpretedFunction("bool2bv1", Seq(r), _)
        ) if bvLogOpToBoolOp.contains(bop) => {
      bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), simplifyExpr(l), simplifyExpr(r)))
    }
    case BinaryExpr(
          bop,
          UninterpretedFunction("bool2bv1", Seq(l), _),
          UninterpretedFunction("bool2bv1", Seq(r), _)
        ) if bvLogOpToBoolOp.contains(bop) => {
      bool2bv1(BinaryExpr(bvLogOpToBoolOp(bop), simplifyExpr(l), simplifyExpr(r)))
    }

    case UnaryExpr(BVNOT, UninterpretedFunction("bool2bv1", Seq(arg), BitVecType(1))) =>
      bool2bv1(UnaryExpr(BoolNOT, simplifyExpr(arg)))

    /* remove bool2bv in boolean context */
    case BinaryExpr(BVEQ, UninterpretedFunction("bool2bv1", Seq(body), _), BitVecLiteral(1, 1)) => simplifyExpr(body)
    case BinaryExpr(BVEQ, UninterpretedFunction("bool2bv1", Seq(l), _), UninterpretedFunction("bool2bv1", Seq(r), _)) =>
      BinaryExpr(BoolEQ, simplifyExpr(l), simplifyExpr(r))
    case UninterpretedFunction("bool2bv1", Seq(FalseLiteral), _) => BitVecLiteral(0, 1)
    case UninterpretedFunction("bool2bv1", Seq(TrueLiteral), _)  => BitVecLiteral(1, 1)

    //case BinaryExpr(BVADD, x: Expr, y: BitVecLiteral) if ir.eval.BitVectorEval.isNegative(y) =>
    //  BinaryExpr(BVSUB, simplifyExpr(x), ir.eval.BitVectorEval.smt_bvneg(y))

    case BinaryExpr(BoolAND, x, TrueLiteral)  => x
    case BinaryExpr(BoolAND, x, FalseLiteral) => FalseLiteral
    case BinaryExpr(BoolOR, x, FalseLiteral)  => x
    case BinaryExpr(BoolOR, x, TrueLiteral)   => TrueLiteral

    case BinaryExpr(BVEQ, BinaryExpr(BVADD, x, y: BitVecLiteral), BitVecLiteral(0, s))
        if ir.eval.BitVectorEval.isNegative(y) =>
      BinaryExpr(BVEQ, x, y)


    /*  COMPARISON FLAG HANDLING 
     *
     * We quite precisely pattern match ASLp's output for C and V, 
     * these are computed by comparing the test to a higher-precision calculation of the test.
     */

    // NF check on expr
    case Extract(upper, lower, b) if size(b).contains(upper) && (upper == (lower + 1)) => {
      bool2bv1(BinaryExpr(BVSLT, simplifyExpr(b), BitVecLiteral(0, size(b).get)))
    }

    // Signed Overflow
    case UnaryExpr(
          BoolNOT,
          BinaryExpr(
            BVEQ,
            SignExtend(exts, orig @ BinaryExpr(o1, x1, y1)),
            compar @ BinaryExpr(o2, x2, y2)
          ) // high precision op
        )
        if (o1 == o2) && o1 == BVADD
          && simplifyExpr(x2) == simplifyExpr(SignExtend(exts, x1))
          && simplifyExpr(y2) == simplifyExpr(SignExtend(exts, y1)) => {
      // V set
      UninterpretedFunction("SignedOverflow", Seq(orig), BoolType)
    }

    case UnaryExpr(
          BoolNOT,
          BinaryExpr(
            BVEQ,
            SignExtend(exts, orig @ BinaryExpr(o1, BinaryExpr(o3, x1, y1), z1)),
            BinaryExpr(o2, compar @ BinaryExpr(o4, x2, y2), z2) // high precision op
          )
        )
        if (o1 == o2) && o2 == o4 && o1 == BVADD
          && simplifyExpr(x2) == simplifyExpr(SignExtend(exts, x1))
          && simplifyExpr(y2) == simplifyExpr(SignExtend(exts, y1))
          && simplifyExpr(z2) == simplifyExpr(SignExtend(exts, z1)) => {
      // V set
      UninterpretedFunction("SignedOverflow", Seq(orig), BoolType)
    }

    // Unsigned OVerflow
    case UnaryExpr(
          BoolNOT,
          BinaryExpr(
            BVEQ,
            ZeroExtend(exts, orig @ BinaryExpr(o1, x1, y1)),
            compar @ BinaryExpr(o2, x2, y2)
          )
        )
        if (o1 == o2) && o1 == BVADD
          && simplifyExpr(x2) == simplifyExpr(ZeroExtend(exts, x1))
          && simplifyExpr(y2) == simplifyExpr(ZeroExtend(exts, y1)) => {
      // C Set
      BinaryExpr(BVUGE, orig, BitVecLiteral(0, size(orig).get))
    }

    case UnaryExpr(
          BoolNOT,
          BinaryExpr(
            BVEQ,
            ZeroExtend(exts, orig @ BinaryExpr(o1, BinaryExpr(o3, x1, y1), z1)),
            BinaryExpr(o2, compar @ BinaryExpr(o4, x2, y2), z2) // high precision op
          )
        )
        if (o1 == o2) && o2 == o4 && o1 == BVADD
          && simplifyExpr(x2) == simplifyExpr(ZeroExtend(exts, x1))
          && simplifyExpr(y2) == simplifyExpr(ZeroExtend(exts, y1))
          && simplifyExpr(z2) == simplifyExpr(ZeroExtend(exts, z1)) => {
      // C Set
      BinaryExpr(BVUGE, orig, BitVecLiteral(0, size(orig).get))
    }

    /**
     * https://developer.arm.com/documentation/dui0801/l/Condition-Codes/Condition-code-suffixes-and-related-flags
     */
    case BinaryExpr(
          BoolEQ,
          (BinaryExpr(BVSLT, lhs, BitVecLiteral(0, sz))),
          UninterpretedFunction("SignedOverflow", Seq(rhs), BoolType)
        ) if simplifyExpr(lhs) == simplifyExpr(rhs) => {
      BinaryExpr(BVSGE, lhs, BitVecLiteral(0, size(lhs).get))
    }

    /* generic comparison simplification */

    // x >= 0 && x != 0 ===> x > 0
    case BinaryExpr(BoolAND, BinaryExpr(BVSGE, lhs, BitVecLiteral(0, sz)), UnaryExpr(BoolNOT, rhs))
        if size(lhs).isDefined && (simplifyExpr(BinaryExpr(BVEQ, lhs, BitVecLiteral(0, size(lhs).get))) == rhs) => {
      BinaryExpr(BVSGT, lhs, BitVecLiteral(0, size(lhs).get))
    }

    // x <= 0 && x != 0 ===> x < 0
    case BinaryExpr(BoolAND, BinaryExpr(BVSLE, lhs, BitVecLiteral(0, sz)), UnaryExpr(BoolNOT, rhs))
        if size(lhs).isDefined && (simplifyExpr(BinaryExpr(BVEQ, lhs, BitVecLiteral(0, size(lhs).get))) == rhs) => {
      BinaryExpr(BVSLT, lhs, BitVecLiteral(0, size(lhs).get))
    }

    // x >= 0 && x != 0 ===> x > 0
    case BinaryExpr(BoolAND, BinaryExpr(BVUGE, lhs, BitVecLiteral(0, sz)), UnaryExpr(BoolNOT, rhs))
        if simplifyExpr(BinaryExpr(BVEQ, lhs, BitVecLiteral(0, size(lhs).get))) == rhs => {
      BinaryExpr(BVUGT, lhs, BitVecLiteral(0, sz))
    }

    // x <= 0 && x != 0 ===> x < 0
    case BinaryExpr(BoolAND, BinaryExpr(BVULE, lhs, BitVecLiteral(0, sz)), UnaryExpr(BoolNOT, rhs))
        if simplifyExpr(BinaryExpr(BVEQ, lhs, BitVecLiteral(0, size(lhs).get))) == rhs => {
      BinaryExpr(BVULT, lhs, BitVecLiteral(0, sz))
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
      simplifyExpr(BinaryExpr(BVULT, y, z)) match {
        case TrueLiteral  => BinaryExpr(BVULT, x, y)
        case FalseLiteral => BinaryExpr(BVULT, x, z)
        case _            => e
      }
    }
    case e @ BinaryExpr(BoolAND, BinaryExpr(BVSLT, x, y), (BinaryExpr(BVSLT, x2, z))) if x == x2 => {
      simplifyExpr(BinaryExpr(BVSLT, y, z)) match {
        case TrueLiteral  => BinaryExpr(BVSLT, x, y)
        case FalseLiteral => BinaryExpr(BVSLT, x, z)
        case _            => e
      }
    }

    // identities
    case Extract(ed, 0, body) if (body.getType == BitVecType(ed))                        => simplifyExpr(body)
    case ZeroExtend(0, body)                                                             => simplifyExpr(body)
    case SignExtend(0, body)                                                             => simplifyExpr(body)
    case BinaryExpr(BVADD, body, BitVecLiteral(0, _))                                    => simplifyExpr(body)
    case BinaryExpr(BVMUL, body, BitVecLiteral(1, _))                                    => simplifyExpr(body)
    case Repeat(1, body)                                                                 => simplifyExpr(body)
    case Extract(ed, 0, ZeroExtend(extension, body)) if (body.getType == BitVecType(ed)) => simplifyExpr(body)
    case Extract(ed, 0, SignExtend(extension, body)) if (body.getType == BitVecType(ed)) => simplifyExpr(body)
    case BinaryExpr(BVXOR, l, r) if l == r =>
      e.getType match {
        case BitVecType(sz) => BitVecLiteral(0, sz)
        case _              => throw Exception("Type error (should be unreachable)")
      }
    case BinaryExpr(BoolEQ, x, FalseLiteral) => UnaryExpr(BoolNOT, x)
    // double negation
    case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body))     => simplifyExpr(body)
    case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body))     => simplifyExpr(body)
    case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => simplifyExpr(body)

    // syntactic equality
    case BinaryExpr(BVEQ, a, b) if a.loads.isEmpty && b.loads.isEmpty && a == b => TrueLiteral

    // compose slices
    case Extract(ed1, be1, Extract(ed2, be2, body)) => Extract(ed1 - be2, be1 + be2, simplifyExpr(body))

    // (comp (comp x y) 1) = (comp x y)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(1, 1)) => simplifyExpr(body)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)) =>
      UnaryExpr(BVNOT, simplifyExpr(body))
    case BinaryExpr(
          BVEQ,
          BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)),
          BitVecLiteral(1, 1)
        ) =>
      BinaryExpr(BVEQ, simplifyExpr(body), BitVecLiteral(0, 1))

    case BinaryExpr(BVSUB, x: Expr, y: BitVecLiteral)         => BinaryExpr(BVADD, x, UnaryExpr(BVNEG, y))
    case BinaryExpr(BVAND, l, r) if l == r && l.loads.isEmpty => simplifyExpr(l)
    case r => r
  }

  if (simped != e) {
    // println(s"old $e -> ")
    // println(s"    $simped")
  }
  simped
}
