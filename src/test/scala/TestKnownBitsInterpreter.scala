import analysis.*
import ir.dsl.*
import ir.eval.*
import ir.{dsl, *}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.*
import org.scalatest.funsuite.*
import org.scalatestplus.scalacheck.*
import test_util.{ExprGen, TestValueDomainWithInterpreter}
import translating.PrettyPrinter.*

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

  implicit lazy val arbExpr: Arbitrary[Expr] = Arbitrary(for {
    sz <- Gen.chooseNum(0, 70)
    e <- ExprGen.genNonLiteralExpr(Some(sz))
  } yield (e))

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
