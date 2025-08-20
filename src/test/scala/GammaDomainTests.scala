import analysis.*
import ir.*
import ir.dsl.*
import ir.transforms.{reversePostOrder, worklistSolver}
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

// TODO more tests + system tests

@test_util.tags.UnitTest
class GammaDomainTests extends AnyFunSuite, CaptureOutput {
  private val registers = 0.to(28).map { n => Register(s"R$n", 64): Variable }.toSet
  private val baseRegisterMap = registers.map { r => (r, Set(r)) }.toMap

  def getMustGammaDomainResults(procedure: Procedure, initialState: VarGammaMap): Map[Block, VarGammaMap] = {
    reversePostOrder(procedure)
    val (before, after) = worklistSolver(MustGammaDomain(initialState)).solveProc(procedure, false)
    after
  }

  def getWpDualResults(procedure: Procedure): Map[Block, Predicate] = {
    reversePostOrder(procedure)
    val domain = WpDualDomain(_ => ProcedureSummary(List(), List()))
    val (before, after) = worklistSolver(domain).solveProc(procedure, true)
    after
  }

  def getReachabilityConditions(procedure: Procedure): Map[Block, Predicate] = {
    reversePostOrder(procedure)
    val (before, after) = worklistSolver(ReachabilityConditions()).solveProc(procedure, false)
    before
  }

  test("constantLiteral") {
    val program = prog(
      proc("main", block("main", directCall("f"), goto("mainRet")), block("mainRet", ret)),
      proc("f", block("assign", LocalAssign(R0, bv64(2), None), goto("returnBlock")), block("returnBlock", ret))
    )
    val f = program.nameToProcedure("f")
    val initialState = LatticeMap.BottomMap(registers.map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val constPropResults = InterProcConstantPropagation(program).analyze()
    val gammaResults = getMustGammaDomainResults(f, initialState)
    val reachability = getReachabilityConditions(f)

    assert(latticeMapApply(gammaResults(f.labelToBlock("returnBlock")), R0, LatticeSetLattice()) == LatticeSet.Bottom())
    assert(reachability(f.labelToBlock("returnBlock")) == Predicate.True)
  }

  test("branching") {
    val program = prog(
      proc("main", block("main", directCall("f"), goto("mainRet")), block("mainRet", ret)),
      proc(
        "f",
        block("branch", goto("a", "b")),
        block("a", LocalAssign(R0, BinaryExpr(BVADD, R0, R1), None), goto("returnBlock")),
        block("b", LocalAssign(R0, BinaryExpr(BVADD, R0, R2), None), goto("returnBlock")),
        block("returnBlock", ret)
      )
    )
    val f = program.nameToProcedure("f")
    val initialState = LatticeMap.BottomMap(registers.map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val constPropResults = InterProcConstantPropagation(program).analyze()
    val gammaResults = getMustGammaDomainResults(f, initialState)
    val reachability = getReachabilityConditions(f)

    assert(
      latticeMapApply(gammaResults(f.labelToBlock("returnBlock")), R0, LatticeSetLattice()) == LatticeSet
        .FiniteSet(Set(R0))
    )
    // TODO is this right?!
    assert(
      MustGammaDomain(initialState)
        .toPred(gammaResults(f.labelToBlock("returnBlock")))
        .split
        .contains(Predicate.gammaLeq(GammaTerm.Var(R0), GammaTerm.OldVar(R0)))
    )
    assert(reachability(f.labelToBlock("branch")) == Predicate.True)
  }

  test("loop") {
    val program = prog(
      proc("main", block("main", directCall("f"), goto("mainRet")), block("mainRet", ret)),
      proc(
        "f",
        block("branch", goto("a", "b")),
        block("a", LocalAssign(R0, BinaryExpr(BVADD, R0, R1), None), goto("branch")),
        block("b", LocalAssign(R0, R2, None), goto("returnBlock")),
        block("returnBlock", ret)
      )
    )
    val f = program.nameToProcedure("f")
    val initialState = LatticeMap.BottomMap(registers.map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val constPropResults = InterProcConstantPropagation(program).analyze()
    val gammaResults = getMustGammaDomainResults(f, initialState)

    assert(
      latticeMapApply(gammaResults(f.labelToBlock("returnBlock")), R0, LatticeSetLattice()) == LatticeSet
        .FiniteSet(Set(R2))
    )
    assert(
      MustGammaDomain(initialState)
        .toPred(gammaResults(f.labelToBlock("returnBlock")))
        .simplify
        .split
        .contains(Predicate.gammaLeq(GammaTerm.Var(R0), GammaTerm.OldVar(R2)))
    )
  }

  test("fApplyExpr") {
    val program = prog(
      proc("main", block("main", directCall("f"), goto("mainRet")), block("mainRet", ret)),
      proc("f", block("assign", LocalAssign(R0, FApplyExpr("__VERIFIER_nondet_uint", Seq(), BitVecType(32), true), None), goto("returnBlock")), block("returnBlock", ret))
    )
    val f = program.nameToProcedure("f")
    val wpDualResults = getWpDualResults(f)
    // should have not panicked
  }
}
