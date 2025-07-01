import analysis.Predicate
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import util.SMT.{SMTSolver, SatResult}

@test_util.tags.UnitTest
class SMTTest extends AnyFunSuite with CaptureOutput {
  def checkPredicate(p: Predicate, timeoutMillis: Integer = 500): SatResult = {
    val solver = SMTSolver(Some(timeoutMillis))
    val res = solver.satisfiable(p)
    solver.close()
    res
  }

  def checkSMT2(p: String, timeoutMillis: Integer = 500): SatResult = {
    val solver = SMTSolver(Some(timeoutMillis))
    val res = solver.smt2Satisfiable(p)
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
(declare-const x (_ BitVec 16))
(declare-const y (_ BitVec 16))
(assert (not (= (bvurem x (bvshl (_ bv1 16) y)) (bvand x (bvsub (bvshl (_ bv1 16) y) (_ bv1 16))))))
""").isInstanceOf[SatResult.Unknown])
  }
}
