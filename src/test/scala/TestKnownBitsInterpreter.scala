import analysis.*
import ir.dsl.*
import ir.eval.*
import ir.{dsl, *}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.*
import org.scalatest.funsuite.*
import org.scalatestplus.scalacheck.*
import test_util.TestValueDomainWithInterpreter
import translating.PrettyPrinter.*

import scala.collection.immutable.LazyList

@test_util.tags.UnitTest
class TestKnownBitsInterpreter
    extends AnyFunSuite
    with TestValueDomainWithInterpreter[TNum]
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  def valueInAbstractValue(absVal: TNum, concrete: Expr) = {
    (absVal, concrete.getType) match {
      case (TNum(v, m), _: BitVecType) =>
        BinaryExpr(EQ, v, BinaryExpr(BVAND, concrete, UnaryExpr(BVNOT, m)))
      case (TNum(v, m), BoolType) =>
        BinaryExpr(EQ, v, BinaryExpr(BVAND, UnaryExpr(BoolToBV1, concrete), UnaryExpr(BVNOT, m)))
      case _ => ???
    }
  }

  val kbitsProg = prog(
    proc(
      "knownBitsExample_4196164",
      Seq("R0_in" -> BitVecType(64), "R1_in" -> BitVecType(64)),
      Seq("R0_out" -> BitVecType(64), "R2_out" -> BitVecType(64), "R3_out" -> BitVecType(64))
    )(
      block(
        "lknownBitsExample",
        LocalAssign(
          LocalVar("R2", BitVecType(64), 2),
          BinaryExpr(
            BVOR,
            BinaryExpr(BVAND, LocalVar("R0_in", BitVecType(64), 0), BitVecLiteral(BigInt("18374966859414961920"), 64)),
            BitVecLiteral(BigInt("18446744069414584320"), 64)
          ),
          Some("%0000023e")
        ),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 3),
          BinaryExpr(
            BVOR,
            BinaryExpr(BVAND, LocalVar("R0_in", BitVecType(64), 0), BitVecLiteral(BigInt("18374966859414961920"), 64)),
            BitVecLiteral(BigInt("71777218305454335"), 64)
          ),
          Some("%00000257")
        ),
        goto("lknownBitsExample_phi_lknownBitsExample_goto_l00000271", "lknownBitsExample_goto_l0000026d")
      ),
      block(
        "l00000274",
        LocalAssign(
          LocalVar("R0", BitVecType(64), 6),
          ZeroExtend(
            4,
            BinaryExpr(
              BVSHL,
              ZeroExtend(8, Extract(60, 8, LocalVar("R2", BitVecType(64), 9))),
              BitVecLiteral(BigInt("8"), 60)
            )
          ),
          Some("%0000027e")
        ),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 7),
          BinaryExpr(BVOR, LocalVar("R0", BitVecType(64), 6), BitVecLiteral(BigInt("15"), 64)),
          Some("%00000284")
        ),
        goto("l00000274_phi_l00000274_goto_l00000299", "l00000274_goto_l0000029d")
      ),
      block(
        "l000002a0",
        Assert(TrueLiteral, Some("is returning to caller-set R30"), None),
        goto("knownBitsExample_4196164_basil_return")
      ),
      block(
        "lknownBitsExample_goto_l0000026d",
        Assume(BinaryExpr(EQ, LocalVar("R1_in", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64)), None, None, true),
        LocalAssign(LocalVar("R2", BitVecType(64), 9), LocalVar("R0", BitVecType(64), 3), Some("phiback")),
        goto("l00000274")
      ),
      block(
        "l00000274_goto_l0000029d",
        Assume(
          BinaryExpr(EQ, Extract(16, 0, LocalVar("R2", BitVecType(64), 9)), BitVecLiteral(BigInt("0"), 16)),
          None,
          None,
          true
        ),
        LocalAssign(LocalVar("R0", BitVecType(64), 14), LocalVar("R2", BitVecType(64), 9), Some("phiback")),
        goto("l000002a0")
      ),
      block(
        "knownBitsExample_4196164_basil_return",
        ret(
          "R0_out" -> LocalVar("R0", BitVecType(64), 14),
          "R2_out" -> LocalVar("R2", BitVecType(64), 9),
          "R3_out" -> BitVecLiteral(BigInt("71777218305454335"), 64)
        )
      ),
      block(
        "lknownBitsExample_phi_lknownBitsExample_goto_l00000271",
        Assume(
          UnaryExpr(BoolNOT, BinaryExpr(EQ, LocalVar("R1_in", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64))),
          None,
          None,
          true
        ),
        LocalAssign(LocalVar("R2", BitVecType(64), 9), LocalVar("R2", BitVecType(64), 2), Some("phiback")),
        goto("l00000274")
      ),
      block(
        "l00000274_phi_l00000274_goto_l00000299",
        Assume(
          UnaryExpr(
            BoolNOT,
            BinaryExpr(EQ, Extract(16, 0, LocalVar("R2", BitVecType(64), 9)), BitVecLiteral(BigInt("0"), 16))
          ),
          None,
          None,
          true
        ),
        LocalAssign(LocalVar("R0", BitVecType(64), 14), LocalVar("R0", BitVecType(64), 7), Some("phiback")),
        goto("l000002a0")
      )
    )
  )

  val kbitsCtx = util.IRLoading.load(kbitsProg)

  def params(v1: BigInt, v2: BigInt) = Some(
    Seq(
      LocalVar("R0_in", BitVecType(64)) -> BitVecLiteral(v1, 64),
      LocalVar("R1_in", BitVecType(64)) -> BitVecLiteral(v2, 64)
    )
  )

  def testInterpret(arg1: BigInt, arg2: BigInt) = {
    val (testResult, _) = analysis.knownBitsAnalysis(kbitsCtx.program)
    val res = runTestInterpreter(kbitsCtx, testResult, callParams = params(arg1, arg2))
    assert(res.checksPassed.nonEmpty)
    assert(
      res.checksFailed.isEmpty,
      "\n\n" + pp_prog_with_analysis_results(res.toDotLabels, Map(), kbitsCtx.program, x => x.toString) + "\n\n"
    )
  }

  test("kbits(0,1)") {
    testInterpret(0, 1)
  }
  test("kbits(0,0)") {
    testInterpret(0, 0)
  }
  test("kbits(1,1)") {
    testInterpret(1, 1)
  }
  test("kbits(0x1000,0x0111)") {
    testInterpret(0x1000, 0x0111)
  }
  test("kbits(long max, long max)") {
    testInterpret(BigInt("ffffffffffffffff", 16), BigInt("ffffffffffffffff", 16))
  }

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

  implicit lazy val arbExpr: Arbitrary[Expr] = Arbitrary(for {
    sz <- Gen.chooseNum(0, 70)
    e <- genExpr(Some(sz))
  } yield (e))

  def crudeSliceExprToSize(expr: Expr, newSize: Int): Option[Expr] =
    ir.size(expr) match {
      case Some(size) if size < newSize => return None
      case None => return Some(expr)
      case _ => ()
    }

    expr match {
      case BitVecLiteral(n, _) => Some(BitVecLiteral(n.min(BigInt(2).pow(newSize) - 1), newSize))
      case n: Literal => Some(n)
      case Extract(ed, start, arg) => crudeSliceExprToSize(arg, newSize)
      case Repeat(repeats, arg) if ir.size(arg).get >= newSize => crudeSliceExprToSize(arg, newSize)
      case Repeat(_, _) => None
      case ZeroExtend(bits, arg) if ir.size(arg).get < newSize => Some(ZeroExtend(newSize - ir.size(arg).get, arg))
      case SignExtend(bits, arg) if ir.size(arg).get < newSize => Some(SignExtend(newSize - ir.size(arg).get, arg))
      case ZeroExtend(bits, arg) => crudeSliceExprToSize(arg, newSize)
      case SignExtend(bits, arg) => crudeSliceExprToSize(arg, newSize)
      case BinaryExpr(BVCONCAT, arg, arg2) =>
        val half = newSize / 2
        val otherhalf = newSize - half
        for {
          a1 <- crudeSliceExprToSize(arg, half)
          a2 <- crudeSliceExprToSize(arg2, otherhalf)
        } yield BinaryExpr(BVCONCAT, a1, a2)
      case BinaryExpr(op, arg, arg2) =>
        for {
          a1 <- crudeSliceExprToSize(arg, newSize)
          a2 <- crudeSliceExprToSize(arg2, newSize)
        } yield BinaryExpr(op, a1, a2)
      case b @ AssocExpr(op, arg) => None
      case UnaryExpr(op, arg) =>
        crudeSliceExprToSize(arg, newSize).map(UnaryExpr(op, _))
      case v: Variable => None
      case f @ FApplyExpr(n, params, rt, _) => None
      case q: QuantifierExpr => None
      case q: LambdaExpr => None
      case OldExpr(x) => crudeSliceExprToSize(x, newSize).map(OldExpr(_))
      case r: SharedMemory => None
      case r: StackMemory => None
    }

  def shrinkExprSizes(expr: Expr) =
    val oldSize = ir.size(expr).getOrElse(8)
    Shrink
      .shrinkWithOrig(oldSize)
      .filter(_ > 0)
      .toList

  def shrinkExprToSameSize(expr: Expr) =
    shrinkExprToSize(ir.size(expr), expr)

  def shrinkExprToSize(inputSize: Option[Int], expr: Expr): Iterable[Expr] = {

    implicit val shrink: Shrink[Expr] = Shrink.withLazyList(x => shrinkExprToSameSize(x).to(LazyList))

    val sizes = inputSize.map(List(_)).getOrElse(shrinkExprSizes(expr))
    val literalShrinks = expr.getType match {
      case _ if expr.isInstanceOf[Literal] => Nil
      case BoolType => Iterable(TrueLiteral, FalseLiteral)
      case IntType => Shrink.shrink(IntLiteral(BigInt(12389)))
      case BitVecType(_) if sizes.length <= 1 =>
        inputSize.iterator.flatMap { size =>
          Shrink.shrinkWithOrig(BitVecLiteral(BigInt(2).pow(size) - 1, size))
        }
      case _ => Nil
    }

    val normalShrinks = expr match {
      case BitVecLiteral(n, _) =>
        for {
          size <- sizes
          newN <- Shrink.shrink(n)
          if 0 <= newN && newN < BigInt(2).pow(size)
        } yield BitVecLiteral(newN, size)
      case IntLiteral(x) => Shrink.shrink(x).map(IntLiteral(_))
      case n: Literal => Nil
      case Extract(ed, start, arg) =>
        for {
          newArg <- Shrink.shrink(arg)
        } yield Extract(ed, start, newArg)
      case Repeat(repeats, arg) =>
        ZeroExtend(ir.size(expr).get - ir.size(arg).get, arg) +:
          Shrink.shrink(arg).map(Repeat(repeats, _))
      case ZeroExtend(bits, arg) =>
        for {
          newArg <- Shrink.shrink(arg)
        } yield ZeroExtend(bits, newArg)
      case SignExtend(bits, arg) =>
        for {
          newArg <- Shrink.shrink(arg)
        } yield SignExtend(bits, newArg)
      case BinaryExpr(op: (BVCmpOp | PolyCmp), arg, arg2) =>
        for {
          a1 <- shrinkExprToSmallerSize.shrink(arg)
          a2 <- shrinkExprToSize(Some(ir.size(a1).getOrElse(8)), arg2)
        } yield BinaryExpr(op, a1, a2)
      case BinaryExpr(op, arg, arg2) =>
        LazyList(arg, arg2).filter(_.getType == expr.getType) ++
          (for {
            (a1, a2) <- Shrink.shrink((arg, arg2))
            // if { require(a1.getType != a2.getType); true }
          } yield BinaryExpr(op, a1, a2)).filter {
            case BinaryExpr(BVSREM | BVSDIV | BVUREM | BVSMOD | BVUDIV, _, BitVecLiteral(n, _)) if n == BigInt(0) =>
              false
            case _ => true
          }
      case b @ AssocExpr(op, arg) => Nil
      case UnaryExpr(op @ BoolToBV1, arg) =>
        shrinkExprToSmallerSize.shrink(arg).map(UnaryExpr(op, _))
      case UnaryExpr(op, arg) =>
        println(arg.getType)
        println(expr.getType)
        LazyList(arg).filter(_.getType == expr.getType) ++ Shrink.shrink(arg).map(UnaryExpr(op, _))
      case v: Variable => Nil
      case f @ FApplyExpr(n, params, rt, _) => Nil
      case q: QuantifierExpr => Nil
      case q: LambdaExpr => Nil
      case OldExpr(x) => Shrink.shrink(x).map(OldExpr(_))
      case r: SharedMemory => Nil
      case r: StackMemory => Nil
    }

    val result = (for {
      size <- sizes.sorted.iterator
      expr <- literalShrinks.iterator ++ normalShrinks.iterator
      out <- crudeSliceExprToSize(expr, size)
    } yield out)
      .filter {
        case BinaryExpr(BVSREM | BVSDIV | BVUREM | BVSMOD | BVUDIV, _, BitVecLiteral(n, _)) if n == BigInt(0) => false
        case _ => true
      }
      .distinct
      .to(LazyList)

    val first = result.take(20).toList
    println("========================")
    println("size = " + sizes)
    println(expr)
    if (result.isEmpty) {
      println("no shrinks :( ")
    } else {
      println("shrinks: " + " -> " + first)
    }
    val sizeSet = sizes.toSet
    val conflicting = first.filterNot(x => ir.size(x).toSet.subsetOf(sizeSet)).toList
    if (conflicting.nonEmpty) {
      println("incorrect types!")
      println(conflicting)
    }
    result
  }

  implicit lazy val shrinkExprToSmallerSize: Shrink[Expr] = Shrink.withLazyList { expr =>
    shrinkExprToSize(None, expr)
      .to(LazyList)
  }

  def evaluateAbstract(e: Expr): TNum = TNumDomain().evaluateExprToTNum(Map(), e)

  test("TNUM soundness property") {
    forAll(minSuccessful(50000)) { (e: Expr) =>
      {
        val (r, c) =
          try {
            abstractEvalSoundnessProperty(evaluateAbstract)(e: Expr)
          } catch {
            case ex => {
              val msg = ex.toString + "\n" + ex.getStackTrace.mkString("\n")
              (false, msg)
            }
          }
        assert(r, c)
      }
    }
  }

}
