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

  /** Apply the rewrite rules once. Note some rules expect a canonical form produced by other rules, and hence this is
    * more effective when applied iteratively until a fixed point.
    */
  val simped = e2 match {
    // constant folding
    // const + (expr + const) -> expr + (const + const)
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
    case BinaryExpr(op, x: Literal, y: Expr) if !y.isInstanceOf[Literal] && assocOps.contains(op) =>
      BinaryExpr(op, simplifyExpr(y), simplifyExpr(x))

    //case BinaryExpr(
    //      BVAND,
    //      UninterpretedFunction("bool2bv1", Seq(l), _),
    //      UninterpretedFunction("bool2bv1", Seq(r), _)
    //    ) => {
    //  bool2bv1(BinaryExpr(BoolAND, l, r))
    //}
    //case BinaryExpr(BVADD, x: Expr, y: BitVecLiteral) if ir.eval.BitVectorEval.isNegative(y) =>
    //  BinaryExpr(BVSUB, simplifyExpr(x), ir.eval.BitVectorEval.smt_bvneg(y))

    case BinaryExpr(BoolAND, x, TrueLiteral)  => x
    case BinaryExpr(BoolAND, x, FalseLiteral) => FalseLiteral
    case BinaryExpr(BoolOR, x, FalseLiteral)  => x
    case BinaryExpr(BoolOR, x, TrueLiteral)   => TrueLiteral

    case BinaryExpr(BVEQ, BinaryExpr(BVADD, x, y: BitVecLiteral), BitVecLiteral(0, s))
        if ir.eval.BitVectorEval.isNegative(y) =>
      BinaryExpr(BVEQ, x, y)

    // lift comparison
    // bvsub32(R8_9[32:0], 2bv32) == 0bv32
    // ZF check
    case BinaryExpr(BVEQ, BinaryExpr(BVSUB, e1, e2), BitVecLiteral(0, sz)) =>
       BinaryExpr(BVEQ, simplifyExpr(e1), simplifyExpr(e2))
    case BinaryExpr(BVCOMP, BinaryExpr(BVSUB, e1, e2), BitVecLiteral(0, sz)) =>
      BinaryExpr(BVCOMP, simplifyExpr(e1), simplifyExpr(e2))
    // NF check on expr
    //case Extract(upper, lower, b)
    //    if (b.getType.isInstanceOf[BitVecType]) && (upper == (lower + 1)) && (upper == b.getType
    //      .asInstanceOf[BitVecType]
    //      .size) => {
    //  bool2bv1(BinaryExpr(BVSLT, simplifyExpr(b), BitVecLiteral(0, b.getType.asInstanceOf[BitVecType].size)))
    //}
    case BinaryExpr(BVSLT, BinaryExpr(BVSUB, a, b), BitVecLiteral(0, sz)) => BinaryExpr(BVSLT, a, b)
    case BinaryExpr(BVSLT, BinaryExpr(BVADD, a, b: BitVecLiteral), BitVecLiteral(0, sz)) =>
      BinaryExpr(BVSLT, a, UnaryExpr(BVNEG, b))
    case BinaryExpr(BVULT, BinaryExpr(BVSUB, a, b), BitVecLiteral(0, sz)) => BinaryExpr(BVULT, a, b)

    case BinaryExpr(BVULT, BinaryExpr(BVADD, a, b: BitVecLiteral), c: BitVecLiteral) =>
      BinaryExpr(BVULT, a, BinaryExpr(BVADD, c, UnaryExpr(BVNEG, b)))
    case BinaryExpr(BVSLT, BinaryExpr(BVADD, a, b: BitVecLiteral), c: BitVecLiteral) =>
      BinaryExpr(BVSLT, a, BinaryExpr(BVADD, c, UnaryExpr(BVNEG, b)))

    //  VF check
    // (bvcomp33(sign_extend1_32(bvsub32(R0_45[32:0], 1000bv32)), bvsub33(sign_extend1_32(R0_45[32:0]), 1000bv33)))
    case UnaryExpr(
          BoolNOT,
          BinaryExpr(
            BVEQ,
            SignExtend(1, orig @ BinaryExpr(BVADD, x1, y1)),
            compar @ BinaryExpr(BVADD, xx, yy)
          )
        ) if (simplifyExpr(SignExtend(1, x1)) == xx) && (simplifyExpr(SignExtend(1, y1)) == yy) => {
      val osz = orig.getType match {
        case BitVecType(sz) => sz
        case _              => ???
      }
      UninterpretedFunction("overflow", Seq(orig), BoolType)
      // BinaryExpr(
      //BoolOR,
      //  UnaryExpr(BoolNOT, BinaryExpr(BVULT, compar, BitVecLiteral(BigInt(2).pow(osz / 2 - 1), osz + 1))),
      //  BinaryExpr(BVULT, compar, UnaryExpr(BVNEG, BitVecLiteral(BigInt(2).pow(osz / 2 - 1), osz + 1)))
      // )
    }

    // VF check
    //(bvcomp33(sign_extend1_32(bvsub32(R0_45[32:0], 1000bv32)), bvsub33(sign_extend1_32(R0_45[32:0]), 1000bv33)))
    //case UnaryExpr(
    //      BoolNOT,
    //      BinaryExpr(
    //        BVEQ,
    //        SignExtend(1, BinaryExpr(BVADD, x1, y1)),
    //        BinaryExpr(BVADD, xx, yy)
    //      )
    //    ) if (simplifyExpr(SignExtend(1, x1)) == xx) && (simplifyExpr(SignExtend(1, y1)) == yy) => {
    //  val r = BinaryExpr(
    //      BoolOR,
    //      BinaryExpr(BoolAND, isNegBV(BinaryExpr(BVADD, x1, y1)), BinaryExpr(BoolAND, isNegBV(x1), isNegBV(y1))),
    //      BinaryExpr(
    //        BoolAND,
    //        UnaryExpr(BoolNOT, isNegBV(BinaryExpr(BVADD, x1, y1))),
    //        BinaryExpr(BoolAND, UnaryExpr(BoolNOT, isNegBV(x1)), UnaryExpr(BoolNOT, isNegBV(y1)))
    //      )
    //    )
    //  r
    //}
    //case BinaryExpr(
    //      BVAND,
    //      UninterpretedFunction("bool2bv1", Seq(l), BitVecType(1)),
    //      UninterpretedFunction("bool2bv1", Seq(r), BitVecType(1))
    //    ) =>
    //  bool2bv1(BinaryExpr(BoolAND, l, r))
    //case BinaryExpr(
    //      BVCOMP,
    //      UninterpretedFunction("bool2bv1", Seq(l), BitVecType(1)),
    //      UninterpretedFunction("bool2bv1", Seq(r), BitVecType(1))
    //    ) =>
    //  bool2bv1(BinaryExpr(BoolEQ, l, r))
    //case BinaryExpr(BVSLT, BinaryExpr(BVADD, x, const: BitVecLiteral), y) => {
    //  BinaryExpr(BVSLT, x, BinaryExpr(BVSUB, y, const))
    //}

    // tighten bounds
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
    // double negation
    case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body))     => simplifyExpr(body)
    case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body))     => simplifyExpr(body)
    case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => simplifyExpr(body)

    // syntactic equality
    case BinaryExpr(BVEQ, a, b) if a.loads.isEmpty && b.loads.isEmpty && a == b => a

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
    // case BinaryExpr(BVCOMP, l, r)                             => bool2bv1(BinaryExpr(BVEQ, l, r))
    //case BinaryExpr(BVEQ, UninterpretedFunction("bool2bv1", Seq(l), BitVecType(1)), BitVecLiteral(1, 1)) => l
    //case UnaryExpr(BVNOT, UninterpretedFunction("bool2bv1", Seq(l), BitVecType(1))) => bool2bv1(UnaryExpr(BoolNOT, l))

    case r => r
  }

  if (simped != e) {
    //println(s"old $e -> ")
    //println(s"    $simped")
  }
  simped
}
