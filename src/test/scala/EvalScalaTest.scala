import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}
import ir.*
import ir.dsl.*
import test_util.CaptureOutput

@test_util.tags.UnitTest
class EvalScalaTest extends AnyFunSuite with CaptureOutput with Matchers {

  import util.EvalScala.EvalResult

  import ir.dsl.given

  val program: Program = prog(
    proc(
      "main",
      block(
        "first_call",
        LocalAssign(R0, bv64(1), None),
        LocalAssign(R1, bv64(1), None),
        directCall("callee1"),
        goto("second_call")
      ),
      block("second_call", directCall("callee2"), goto("returnBlock")),
      block("returnBlock", ret)
    ),
    proc("callee1", block("returnBlock", ret)),
    proc("callee2", block("returnBlock", ret)),
    proc("empty procedure")
  )

  test("basic eval") {
    assertResult(Success(10)) { util.EvalScala.eval("1+9").result }
    assertResult(Success("asd")) { util.EvalScala.eval("\"asd\"").result }
    assertResult(Success(List(1, 2, 3))) { util.EvalScala.eval("scala.collection.immutable.Seq(1, 2, 3)").result }
  }

  test("eval runtime failures") {
    // WARN: these tests rely on the possibly-unreliable unwrapScalaEvalException method.
    // if they start failing because the returned exceptions differ from the expected types,
    // check that method.

    util.EvalScala.eval("1 match { case 2 => () }").failure should matchPattern { case _: MatchError => }
    util.EvalScala.eval("throw new Exception(\"boop\")").failure should matchPattern {
      case x: Exception if x.getMessage == "boop" =>
    }
    util.EvalScala.eval("1 / 0").failure should matchPattern { case _: ArithmeticException => }
    util.EvalScala.eval("1 / 0\n1+2").failure should matchPattern { case _: ArithmeticException => }
  }

  test("eval compile failures") {
    util.EvalScala.eval("1+") should matchPattern { case EvalResult(Failure(_), s) if s != "" => }
    util.EvalScala.eval("undefinedvariable") should matchPattern { case EvalResult(Failure(_), s) if s != "" => }
    util.EvalScala.eval(".") should matchPattern { case EvalResult(Failure(_), s) if s != "" => }
  }

  test("eval of toscala") {
    val p2 = util.EvalScala.evalDSL(program.toScala).get
    assertResult(program.toString) { p2.toString }
  }
}
