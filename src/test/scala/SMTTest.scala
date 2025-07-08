import analysis.Predicate
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import util.SMT.{SMTSolver, SatResult}

@test_util.tags.UnitTest
class SMTTest extends AnyFunSuite with CaptureOutput {
  def checkPredicate(p: Predicate, timeoutMillis: Int = 50): SatResult = {
    val solver = SMTSolver(timeoutMillis)
    val res = solver.sat(p)
    solver.close()
    res
  }

  def checkSMT2(p: String, timeoutMillis: Int = 50): SatResult = {
    val solver = SMTSolver(timeoutMillis)
    val res = solver.smt2Sat(p)
    solver.close()
    res
  }

  test("Predicates") {
    assert(checkPredicate(Predicate.True) == SatResult.SAT)
    assert(checkPredicate(Predicate.False) == SatResult.UNSAT)
  }

  test("SMT2") {
    // If this says UNSAT, princess has dramatically improved as an SMT solver since this test case was made! You're
    // probably fine to turn this into an UNSAT case instead of an Unknown case, though maybe increase the timeout so
    // that potentially slower hardware doesn't have any issues.
    assert(checkSMT2("""
(declare-const x (_ BitVec 64))
(declare-const y (_ BitVec 64))
(assert (not (= (bvurem x (bvshl (_ bv1 64) y)) (bvand x (bvsub (bvshl (_ bv1 64) y) (_ bv1 64))))))
""").isInstanceOf[SatResult.Unknown])
  }
}
