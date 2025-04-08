import ir.*
import ir.eval.*
import analysis.*
import ir.transforms.*
import java.io.{BufferedWriter, File, FileWriter}
import ir.Endian.LittleEndian
import org.scalatest.*
import org.scalatest.funsuite.*
import translating.PrettyPrinter.*
import specification.*
import util.{
  BASILConfig,
  IRLoading,
  ILLoadingConfig,
  IRContext,
  RunUtils,
  StaticAnalysis,
  StaticAnalysisConfig,
  StaticAnalysisContext,
  BASILResult,
  Logger,
  LogLevel,
  IRTransform
}
import ir.eval.{interpretTrace, interpret, ExecEffect, Stopped}
import ir.dsl

import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import ir.dsl.*
import util.RunUtils.loadAndTranslate

import scala.collection.mutable
import org.scalatestplus.scalacheck.*
import org.scalacheck.Arbitrary
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}

@test_util.tags.UnitTest
class TestKnownBitsInterpreter
    extends AnyFunSuite
    with TestValueDomainWithInterpreter[TNum]
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

  def valueInAbstractValue(absVal: TNum, concrete: Expr) = {
    (absVal, concrete.getType) match {
      case (TNumValue(v, m, w), _: BitVecType) =>
        BinaryExpr(BVEQ, BitVecLiteral(v, w), BinaryExpr(BVAND, concrete, UnaryExpr(BVNOT, BitVecLiteral(m, w))))
      case (TNumValue(v, m, w), BoolType) =>
        BinaryExpr(
          BVEQ,
          BitVecLiteral(v, w),
          BinaryExpr(BVAND, UnaryExpr(BoolToBV1, concrete), UnaryExpr(BVNOT, BitVecLiteral(m, w)))
        )
      case _ => ???
    }
  }

  val kbitsProg = prog(
    proc(
      "knownBitsExample_4196164",
      Seq("R0_in" -> BitVecType(64), "R1_in" -> BitVecType(64)),
      Seq("R0_out" -> BitVecType(64), "R2_out" -> BitVecType(64), "R3_out" -> BitVecType(64)),
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
        Assume(
          BinaryExpr(BVEQ, LocalVar("R1_in", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64)),
          None,
          None,
          true
        ),
        LocalAssign(LocalVar("R2", BitVecType(64), 9), LocalVar("R0", BitVecType(64), 3), Some("phiback")),
        goto("l00000274")
      ),
      block(
        "l00000274_goto_l0000029d",
        Assume(
          BinaryExpr(BVEQ, Extract(16, 0, LocalVar("R2", BitVecType(64), 9)), BitVecLiteral(BigInt("0"), 16)),
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
          UnaryExpr(BoolNOT, BinaryExpr(BVEQ, LocalVar("R1_in", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64))),
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
            BinaryExpr(BVEQ, Extract(16, 0, LocalVar("R2", BitVecType(64), 9)), BitVecLiteral(BigInt("0"), 16))
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
    val (testResult, _) = transforms.knownBitsAnalysis(kbitsCtx.program)
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

  val arbBinOp =
    Gen.oneOf(
      BVAND,
      BVOR,
      BVADD,
      BVMUL,
      BVSHL,
      BVLSHR,
      BVULT,
      BVNAND,
      BVNOR,
      BVXOR,
      BVXNOR,
      BVCOMP,
      BVSUB,
      BVASHR,
      BVUREM, // broken
      BVSREM, // broken
      BVSMOD, // broken
      BVUDIV, // broken
      BVSDIV, // broken
      BVULE,
      BVUGT,
      BVUGE,
      BVSLT,
      BVSLE,
      BVSGT,
      BVSGE,
      BVEQ,
      BVNEQ,
      BVCONCAT
    )

  implicit lazy val arbExpr: Arbitrary[Expr] = Arbitrary(for {
    size <- Gen.chooseNum(1, 70)
    op <- arbBinOp
    maxVal = (BigInt(2).pow(size) - 1)
    smallMax = maxVal.min(255)
    rhs <- op match {
      case BVSDIV => Gen.chooseNum(BigInt(1), maxVal)
      case BVUDIV => Gen.chooseNum(BigInt(1), maxVal)
      case BVSREM => Gen.chooseNum(BigInt(1), maxVal)
      case BVSMOD => Gen.chooseNum(BigInt(1), maxVal)
      case BVSHL => Gen.chooseNum(BigInt(0), smallMax)
      case BVLSHR => Gen.chooseNum(BigInt(0), smallMax)
      case BVASHR => Gen.chooseNum(BigInt(0), smallMax)
      case _ => Gen.chooseNum(BigInt(0), maxVal)
    }
    lhs <- Gen.chooseNum(BigInt(0), (BigInt(2).pow(size) - 1))
    // rhs <- Gen.chooseNum(BigInt(minBound), (BigInt(2).pow(sizeRhs) - 1))
  } yield BinaryExpr(op, BitVecLiteral(lhs, size), (BitVecLiteral(rhs, size))))

  def evaluateAbstract(e: Expr): TNum = TNumDomain().evaluateExprToTNum(Map(), e)

  test("binopprop") {
    forAll { (e: Expr) =>
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
