import analysis.Predicate
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import util.SMT.SMTSolver

@test_util.tags.UnitTest
class SMTTest extends AnyFunSuite with CaptureOutput {
  def evaluatePredicate(p: Predicate): Option[Boolean] = {
    val solver = SMTSolver()
    val res = solver.satisfiable(p)
    solver.close()
    res
  }

  test("true") {
    val p = Predicate.True
    assert(evaluatePredicate(p) == Some(true))
  }

  test("false") {
    val p = Predicate.False
    assert(evaluatePredicate(p) == Some(false))
  }
}
