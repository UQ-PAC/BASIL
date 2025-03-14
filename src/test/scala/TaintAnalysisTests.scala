import analysis.*
import boogie.*
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.BASILTest

@test_util.tags.UnitTest
class TaintAnalysisTests extends AnyFunSuite, test_util.CaptureOutput, BASILTest {
  def getTaintAnalysisResults(
    program: Program,
    taint: Map[CFGPosition, Set[Taintable]]
  ): Map[CFGPosition, Set[Taintable]] = {
    val constPropResults = InterProcConstantPropagation(program).analyze()
    TaintAnalysis(program, Map(), constPropResults, taint).analyze().map { (c, m) => (c, m.map { (v, _) => v }.toSet) }
  }

  def getVarDepResults(program: Program, procedure: Procedure): Map[CFGPosition, Map[Taintable, Set[Taintable]]] = {
    val constPropResults = InterProcConstantPropagation(program).analyze()
    val variables = registers
    ProcVariableDependencyAnalysis(program, variables, Map(), constPropResults, Map(), procedure).analyze()
  }

  private val registers = 0.to(28).map { n => Register(s"R$n", 64): Taintable }.toSet
  private val baseRegisterMap = registers.map { r => (r, Set(r)) }.toMap

  test("constantLiteral") {
    val program = prog(
      proc("main", block("main", directCall("f"), goto("mainRet")), block("mainRet", ret)),
      proc("f", block("assign", LocalAssign(R0, bv64(2), None), goto("returnBlock")), block("returnBlock", ret))
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R0))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(!taintAnalysisResults.contains(IRWalk.lastInProc(f).get))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults.get(IRWalk.lastInProc(f).get).contains(baseRegisterMap - R0))
  }

  test("arguments") {
    val program = prog(
      proc("main", block("main", directCall("f"), goto("mainRet")), block("mainRet", ret)),
      proc(
        "f",
        block("assign", LocalAssign(R0, BinaryExpr(BVADD, R0, R1), None), goto("returnBlock")),
        block("returnBlock", ret)
      )
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R0))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(taintAnalysisResults.get(IRWalk.lastInProc(f).get).contains(Set(R0)))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults.get(IRWalk.lastInProc(f).get).contains(baseRegisterMap + (R0 -> Set(R0, R1))))
  }

  test("branching") {
    val program = prog(
      proc("main", block("main", directCall("f"), goto("mainRet")), block("mainRet", ret)),
      proc(
        "f",
        block("branch", goto("a", "b")),
        block("a", LocalAssign(R0, R1, None), goto("returnBlock")),
        block("b", LocalAssign(R0, R2, None), goto("returnBlock")),
        block("returnBlock", ret)
      )
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R1))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(taintAnalysisResults.get(IRWalk.lastInProc(f).get).contains(Set(R0, R1)))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults.get(IRWalk.lastInProc(f).get) == Some(baseRegisterMap + (R0 -> Set(R1, R2))))
  }

  test("interproc") {
    val program = prog(
      proc("main", block("main", directCall("f"), goto("mainRet")), block("mainRet", ret)),
      proc(
        "f",
        block("branch", goto("a", "b")),
        block("a", LocalAssign(R1, R1, None), directCall("g"), goto("fReturnBlock")),
        block("b", LocalAssign(R1, R2, None), directCall("g"), goto("fReturnBlock")),
        block("fReturnBlock", ret)
      ),
      proc("g", block("body", LocalAssign(R0, R1, None), goto("gReturnBlock")), block("gReturnBlock", ret))
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R1))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(taintAnalysisResults.get(IRWalk.lastInProc(f).get).contains(Set(R0, R1)))

    val varDepResults = getVarDepResults(program, f)

    assert(
      varDepResults.get(IRWalk.lastInProc(f).get).contains(baseRegisterMap + (R0 -> Set(R1, R2)) + (R1 -> Set(R1, R2)))
    )
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
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R1))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(taintAnalysisResults.get(IRWalk.lastInProc(f).get).contains(Set(R1)))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults.get(IRWalk.lastInProc(f).get).contains(baseRegisterMap + (R0 -> Set(R2))))
  }
}
