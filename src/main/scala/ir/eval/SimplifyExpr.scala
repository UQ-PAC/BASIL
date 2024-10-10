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

def simplifyExpr(e: Expr): Expr = {
  /**
   * Apply the rewrite rules once. 
   * Note some rules expect a canonical form produced by other rules, and hence this is more effective 
   * when applied iteratively until a fixed point.
   */
  e match {
    // normalise
    case BinaryExpr(op, x: Literal, y: Expr) if !y.isInstanceOf[Literal] && assocOps.contains(op) =>
      BinaryExpr(op, y, x)
    case BinaryExpr(BVADD, x: Expr, y: BitVecLiteral) if ir.eval.BitVectorEval.isNegative(y) =>
      BinaryExpr(BVSUB, x, ir.eval.BitVectorEval.smt_bvneg(y))

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
        case _ => throw Exception("Type error (should be unreachable)")
      }
    // double negation
    case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body))     => simplifyExpr(body)
    case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body))     => simplifyExpr(body)
    case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => simplifyExpr(body)

    // compose slices
    case Extract(ed1, be1, Extract(ed2, be2, body)) => Extract(ed1 - be2, be1 + be2, simplifyExpr(body))

    // (comp (comp x y) 1) = (comp x y)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(1, 1)) => simplifyExpr(body)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)) => UnaryExpr(BVNOT, simplifyExpr(body))
    case BinaryExpr(
          BVEQ,
          BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)),
          BitVecLiteral(1, 1)
        ) =>
      BinaryExpr(BVEQ, simplifyExpr(body), BitVecLiteral(0, 1))

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
    case r => r
  }
}


