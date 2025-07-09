import ir.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.*
import org.scalatest.funsuite.*
import test_util.{CaptureOutput, ExprGen}
import translating.BasilIRToSMT2
import translating.PrettyPrinter.*
import util.Logger
import util.z3.*

@test_util.tags.UnitTest
class BitVectorEvalTest
    extends AnyFunSuite
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
    with CaptureOutput {

  def genSMT(size: Int) = {
    val max = BitVecType(size).maxValue.toInt

    val ops = List(
      BVAND,
      BVOR,
      BVADD,
      BVMUL,
      BVUDIV,
      BVUREM,
      BVSHL,
      BVLSHR,
      BVULT,
      BVNAND,
      BVNOR,
      BVXOR,
      BVXNOR,
      BVCOMP,
      BVSUB,
      BVSDIV,
      BVSREM,
      BVSMOD,
      BVASHR,
      BVULE,
      BVUGT,
      BVUGE,
      BVSLT,
      BVSLE,
      BVSGT,
      BVSGE,
      EQ,
      NEQ,
      BVCONCAT
    )

    var checks = Vector[(Expr, Expr)]()

    for (l <- 0 to max) {
      for (r <- 0 to max) {
        val lhs = BitVecLiteral(l, size)
        val rhs = BitVecLiteral(r, size)
        val exprs = ops.map(op => BinaryExpr(op, lhs, rhs))
        val test = exprs.map(e => (e, BinaryExpr(NEQ, e, ir.eval.evaluateExpr(e).get)))
        checks = checks ++ test
      }
    }

    // val query = checks.map(e => BasilIRToSMT2.exprUnsat(e, None, false))

    checks.foreach {
      case (l, exp) => {
        test("" + pp_expr(l)) {
          val q = BasilIRToSMT2.exprUnsat(exp, None, false)
          assert(util.z3.checkSATSMT2(q) == SatResult.UNSAT)
        }
      }
    }
  }

  implicit lazy val arbExpr: Arbitrary[Expr] = Arbitrary(for {
    sz <- Gen.oneOf(List(30, 31, 32, 33, 60, 63, 64, 65, 66, 70, 90, 128, 126))
    e <- ExprGen.genExpr(Some(sz))
  } yield (e))

  test("interp exprs smt") {
    forAll(minSuccessful(30)) { (exp: Expr) =>
      val test = BinaryExpr(NEQ, exp, ir.eval.evaluateExpr(exp).get)
      val q = BasilIRToSMT2.exprUnsat(test, None, false)
      Logger.info("assert: " + test)
      util.z3.checkSATSMT2(q, Some(30000)) == SatResult.UNSAT
    }
  }

  genSMT(2)

}
