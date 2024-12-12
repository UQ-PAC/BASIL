import analysis.*
import boogie.*
import ir.transforms.{reversePostOrder, worklistSolver}
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.BASILTest

// TODO more tests + system tests

class GammaDomainTests extends AnyFunSuite, BASILTest {
  private val registers = 0.to(28).map { n => Register(s"R$n", 64): Taintable }.toSet
  private val baseRegisterMap = registers.map { r => (r, Set(r)) }.toMap

  def getMustGammaDomainResults(
    procedure: Procedure,
    initialState: VarGammaMap,
    constPropResults: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  ): Map[Block, VarGammaMap] = {
    reversePostOrder(procedure)
    val (before, after) = worklistSolver(MustGammaDomain(Map(), constPropResults)).solveProc(procedure, false)
    after
  }

  def getReachabilityConditions(procedure: Procedure): Map[Block, Boolean] = {
    reversePostOrder(procedure)
    val (before, after) = worklistSolver(ReachabilityConditions()).solveProc(procedure, false)
    before
  }

  test("constantLiteral") {
    val program = prog(
        proc("main",
          block("main",
            directCall("f"),
            goto("mainRet")
          ),
          block("mainRet", ret)
        ),
        proc("f",
          block("assign",
            LocalAssign(R0, bv64(2), None),
            goto("returnBlock"),
          ),
          block("returnBlock",
            ret
          ),
        )
      )
    val f = program.nameToProcedure("f")
    val initialState = VarGammaMap.BottomMap(registers.map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val constPropResults = InterProcConstantPropagation(program).analyze()
    val gammaResults = getMustGammaDomainResults(f, initialState, constPropResults)
    val reachability = getReachabilityConditions(f)

    assert(gammaResults(f.labelToBlock("returnBlock"))(R0) == LatticeSet.Bottom())
    assert(reachability(f.labelToBlock("returnBlock")) == true)
  }

  test("branching") {
    val program = prog(
        proc("main",
          block("main",
            directCall("f"),
            goto("mainRet")
          ),
          block("mainRet", ret)
        ),
        proc("f",
          block("branch",
            goto("a", "b"),
          ),
          block("a",
            LocalAssign(R0, BinaryExpr(BVADD, R0, R1), None),
            goto("returnBlock"),
          ),
          block("b",
            LocalAssign(R0, BinaryExpr(BVADD, R0, R2), None),
            goto("returnBlock"),
          ),
          block("returnBlock",
            ret
          ),
        ),
      )
    val f = program.nameToProcedure("f")
    val initialState = VarGammaMap.BottomMap(registers.map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val constPropResults = InterProcConstantPropagation(program).analyze()
    val gammaResults = getMustGammaDomainResults(f, initialState, constPropResults)
    val reachability = getReachabilityConditions(f)

    assert(gammaResults(f.labelToBlock("returnBlock"))(R0) == LatticeSet.FiniteSet(Set(R0)))
    assert(reachability(f.labelToBlock("branch")) == true)
  }

  test("loop") {
    val program = prog(
        proc("main",
          block("main",
            directCall("f"),
            goto("mainRet")
          ),
          block("mainRet", ret)
        ),
        proc("f",
          block("branch",
            goto("a", "b"),
          ),
          block("a",
            LocalAssign(R0, BinaryExpr(BVADD, R0, R1), None),
            goto("branch"),
          ),
          block("b",
            LocalAssign(R0, R2, None),
            goto("returnBlock"),
          ),
          block("returnBlock",
            ret
          ),
        ),
      )
    val f = program.nameToProcedure("f")
    val initialState = VarGammaMap.BottomMap(registers.map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val constPropResults = InterProcConstantPropagation(program).analyze()
    val gammaResults = getMustGammaDomainResults(f, initialState, constPropResults)

    assert(gammaResults(f.labelToBlock("returnBlock"))(R0) == LatticeSet.FiniteSet(Set(R2)))
  }
}
