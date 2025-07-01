import analysis.Predicate
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import util.SMT.{SMTSolver, SatResult}

@test_util.tags.UnitTest
class SMTTest extends AnyFunSuite with CaptureOutput {
  def checkPredicate(p: Predicate): SatResult = {
    val solver = SMTSolver()
    val res = solver.satisfiable(p)
    solver.close()
    res
  }

  test("true") {
    val p = Predicate.True
    assert(checkPredicate(p) == SatResult.SAT)
  }

  test("false") {
    val p = Predicate.False
    assert(checkPredicate(p) == SatResult.UNSAT)
  }
}
