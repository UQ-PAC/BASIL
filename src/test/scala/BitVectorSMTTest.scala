import ir.*
import ir.eval.*
import util.z3.*
import analysis.*
import translating.BasilIRToSMT2
import org.scalatest.*
import org.scalatest.funsuite.*
import translating.PrettyPrinter.*
import specification.*

@test_util.tags.UnitTest
class BitVectorEvalTest extends AnyFunSuite {

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
      BVEQ,
      BVNEQ,
      BVCONCAT
    )

    var checks = Vector[(Expr, Expr)]()

    for (l <- 0 to max) {
      for (r <- 0 to max) {
        val lhs = BitVecLiteral(l, size)
        val rhs = BitVecLiteral(r, size)
        val exprs = ops.map(op => BinaryExpr(op, lhs, rhs))
        val test = exprs.map(e => (e, BinaryExpr(BVNEQ, e, ir.eval.evaluateExpr(e).get)))
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

  genSMT(3)
}
