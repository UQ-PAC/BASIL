package ir.eval
import ir.*

/**
 * One rewrite pass of recursive descent BASIL IR simplification.
 */
def simplifyExpression(e: Expr): Expr = {
  exprSimp(e)
}


/**
 * Simplify BASIL IR expression to fixed point using rewrite rules.
 */
def simplifyExpressionFull(e: Expr): Expr = {
  var pe = e
  var ne = exprSimp(pe)
  while (ne != pe) {
    pe = ne
    ne = exprSimp(pe)
  }
  ne
}


private def exprSimp(e: Expr): Expr = {

  val assocOps: Set[BinOp] =
    Set(BVADD, BVMUL, BVOR, BVAND, BVEQ, BoolAND, BoolEQ, BoolOR, BoolEQUIV, BoolEQ, IntADD, IntMUL, IntEQ)

  e match {
    // normalise
    case BinaryExpr(op, x: Literal, y: Expr) if !y.isInstanceOf[Literal] && assocOps.contains(op) =>
      BinaryExpr(op, y, x)

    // identities
    case Extract(ed, 0, body) if (body.getType == BitVecType(ed))                        => exprSimp(body)
    case ZeroExtend(0, body)                                                             => exprSimp(body)
    case SignExtend(0, body)                                                             => exprSimp(body)
    case BinaryExpr(BVADD, body, BitVecLiteral(0, _))                                    => exprSimp(body)
    case BinaryExpr(BVMUL, body, BitVecLiteral(1, _))                                    => exprSimp(body)
    case Repeat(1, body)                                                                 => exprSimp(body)
    case Extract(ed, 0, ZeroExtend(extension, body)) if (body.getType == BitVecType(ed)) => exprSimp(body)
    case Extract(ed, 0, SignExtend(extension, body)) if (body.getType == BitVecType(ed)) => exprSimp(body)
    case BinaryExpr(BVXOR, l, r) if l == r =>
      e.getType match {
        case BitVecType(sz) => BitVecLiteral(0, sz)
      }
    // double negation
    case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body))     => exprSimp(body)
    case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body))     => exprSimp(body)
    case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => exprSimp(body)

    // compose slices
    case Extract(ed1, be1, Extract(ed2, be2, body)) => Extract(ed1 - be2, be1 + be2, exprSimp(body))

    // (comp (comp x y) 1) = (comp x y)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(1, 1)) => exprSimp(body)
    case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)) => UnaryExpr(BVNOT, exprSimp(body))
    case BinaryExpr(
          BVEQ,
          BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)),
          BitVecLiteral(1, 1)
        ) =>
      BinaryExpr(BVEQ, exprSimp(body), BitVecLiteral(0, 1))

    // constant folding
    // const + (e + const) -> (const + const) + e
    case BinaryExpr(BVADD, BinaryExpr(BVADD, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVADD, BinaryExpr(BVADD, l, r), exprSimp(body))
    case BinaryExpr(BVMUL, BinaryExpr(BVMUL, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVMUL, BinaryExpr(BVMUL, l, r), exprSimp(body))
    case BinaryExpr(BVOR, BinaryExpr(BVOR, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVOR, BinaryExpr(BVOR, l, r), exprSimp(body))
    case BinaryExpr(BVAND, BinaryExpr(BVAND, body, l: BitVecLiteral), r: BitVecLiteral) =>
      BinaryExpr(BVAND, BinaryExpr(BVAND, l, r), exprSimp(body))
    case r => r
  }
}

