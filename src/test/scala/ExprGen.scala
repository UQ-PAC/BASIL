package test_util
import ir.*
import org.scalacheck.Gen

object ExprGen {

  def arbBinOp =
    Gen.oneOf(
      BVAND,
      BVOR,
      BVADD,
      BVMUL,
      BVSHL,
      BVLSHR,
      BVNAND,
      BVNOR,
      BVXOR,
      BVXNOR,
      BVSUB,
      BVASHR, // broken
      BVUREM, // broken
      BVSREM, // broken
      BVSMOD, // broken
      BVUDIV, // broken
      BVSDIV // broken
    )

  def arbBinComp = Gen.oneOf(BVULE, BVUGT, BVULT, BVUGE, BVSLT, BVSLE, BVSGT, BVSGE, EQ, NEQ, BVCOMP)

  def genValue(givenSize: Option[Int] = None) = for {
    genSize <- Gen.chooseNum(1, 70)
    size = givenSize.getOrElse(genSize)
    maxVal = BitVecType(size).maxValue
    value <- Gen.chooseNum(BigInt(0), maxVal)
  } yield (BitVecLiteral(value, size))

  def genUnExp(size: Option[Int] = None) = for {
    v <- genValue(size)
    op <- Gen.oneOf(BVNOT, BVNEG)
  } yield (UnaryExpr(op, v))

  def genExt(givenSize: Option[Int] = None) =
    if givenSize.exists(_ < 1) then {
      genValue(givenSize)
    } else
      for {
        genSize <- Gen.chooseNum(1, 70)
        size = givenSize.getOrElse(genSize)
        amount <- Gen.chooseNum(0, size - 1)
        sizeLeft = size - amount
        v <- if (sizeLeft > 1) then genExpr(Some(sizeLeft)) else genValue(Some(sizeLeft))
        vv <- genExpr(Some(amount))
        op <- Gen.oneOf(ZeroExtend(amount, v), SignExtend(amount, v), BinaryExpr(BVCONCAT, v, vv))
      } yield (op)

  def genBinComp() = for {
    op <- arbBinComp
    genSize <- Gen.chooseNum(1, 70)
    l <- genExpr(Some(genSize))
    r <- genExpr(Some(genSize))
  } yield (BinaryExpr(op, l, r))

  def genBinExp(givenSize: Option[Int] = None): Gen[Expr] = {
    def genBV(min: BigInt, max: BigInt, size: Int) = for {
      v <- Gen.chooseNum(min, max)
    } yield BitVecLiteral(v, size)
    for {
      genSize <- Gen.chooseNum(1, 70)
      size = givenSize.getOrElse(genSize)
      op <- Gen.oneOf(arbBinOp, arbBinComp)
      maxVal = (BigInt(2).pow(size) - 1)
      smallMax = maxVal.min(255)
      rhs <- op match {
        case BVSDIV => genBV(BigInt(1), maxVal, size)
        case BVUDIV => genBV(BigInt(1), maxVal, size)
        case BVSREM => genBV(BigInt(1), maxVal, size)
        case BVSMOD => genBV(BigInt(1), maxVal, size)
        case BVSHL => genBV(BigInt(0), smallMax, size)
        case BVLSHR => genBV(BigInt(0), smallMax, size)
        case BVASHR => genBV(BigInt(0), smallMax, size)
        case _ => genExpr(Some(size))
      }
      lhs <- genExpr(Some(size))
      expr = BinaryExpr(op, lhs, rhs)
      nexpr <- expr.getType match {
        case BoolType if size != 1 =>
          Gen.oneOf(ZeroExtend(size - 1, UnaryExpr(BoolToBV1, expr)), SignExtend(size - 1, UnaryExpr(BoolToBV1, expr)))
        case BoolType => Gen.const(UnaryExpr(BoolToBV1, expr))
        case BitVecType(bvsz) if size > bvsz => Gen.oneOf(ZeroExtend(size - bvsz, expr), SignExtend(size - bvsz, expr))
        case BitVecType(sz) if size == sz => Gen.const(expr)
        case x => throw Exception(s"TYPE $x DOES NOT MATCH EXPECTED $size $expr")
      }
      // rhs <- Gen.chooseNum(BigInt(minBound), (BigInt(2).pow(sizeRhs) - 1))
    } yield nexpr
  }

  def genExpr(size: Option[Int] = None): Gen[Expr] =
    if (size.exists(_ <= 1)) then genValue(size) else Gen.oneOf(genBinExp(size), genUnExp(size), genValue(size))

}
