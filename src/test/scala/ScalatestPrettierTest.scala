import org.scalactic.Prettifier
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

object ScalatestPrettierTest {

  class A extends Iterable[A] {
    var n = 0
    def iterator =
      n = n + 1
      if n < 500 then List(this, this).iterator else Iterator()
    override def toString = "tostring"
  }

}

@test_util.tags.UnitTest
class ScalatestPrettierTest extends AnyFunSuite with CaptureOutput {

  import ScalatestPrettierTest.*

  /**
   * By default, scalatest applies *their own* prettifier to render
   * values in failing assertions. In scalatest versions before 3.2.13,
   * this prettifier fails to detect cycles. This causes big problems
   * for iterables which return themself in their iterator, such as the
   * Basil IR procedures.
   *
   * This test case has a minimal example exhibiting this problem.
   */
  test("prettifier should use tostring for cyclic values :|") {
    assertResult(A().toString) {
      Prettifier.default(A())
    }
  }

}
