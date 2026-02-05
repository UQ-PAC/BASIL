import analysis.*
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

import LatticeSet.*

@test_util.tags.UnitTest
class TaintAnalysisTests extends AnyFunSuite, CaptureOutput {
  def getTaintAnalysisResults(
    program: Program,
    taint: Map[CFGPosition, Set[Variable]]
  ): Map[CFGPosition, Set[Variable]] = {
    TaintAnalysis(program, taint).analyze().map { (c, m) => (c, m.map { (v, _) => v }.toSet) }
  }

  def getVarDepResults(
    program: Program,
    procedure: Procedure
  ): Map[CFGPosition, Map[GammaVal, LatticeSet[GammaVal]]] = {
    val variables = registers
    ProcVariableDependencyAnalysis(program, variables, Map(), procedure).analyze()
  }

  def convertReturns(program: Program): Unit = {
    val replaceReturns = transforms.ReplaceReturns()
    cilvisitor.visit_prog(replaceReturns, program)
    replaceReturns.addR30Begins()
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)
  }

  private val registers = 0.to(31).map { n => Register(s"R$n", 64): Variable }.toSet
  private val baseRegisterMap = registers.map { r => r -> FiniteSet(Set(r)) }.toMap

  test("constantLiteral") {
    val program = prog(
      proc("main", block("main", directCall("f"), goto("mainRet")), block("mainRet", ret)),
      proc("f", block("assign", LocalAssign(R0, bv64(2), None), goto("returnBlock")), block("returnBlock", ret))
    )
    convertReturns(program)

    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Variable]] = Map(f -> Set(R0))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(!taintAnalysisResults.contains(IRWalk.lastInProc(f).get))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults(IRWalk.lastInProc(f).get)(R0) == FiniteSet(Set()))
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
    convertReturns(program)
    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Variable]] = Map(f -> Set(R0))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(taintAnalysisResults(IRWalk.lastInProc(f).get) == Set(R0))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults(IRWalk.lastInProc(f).get)(R0) == FiniteSet(Set(R0, R1)))
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
    convertReturns(program)

    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Variable]] = Map(f -> Set(R1))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(taintAnalysisResults(IRWalk.lastInProc(f).get) == Set(R0, R1))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults(IRWalk.lastInProc(f).get)(R0) == FiniteSet(Set(R1, R2)))
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
    convertReturns(program)

    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Variable]] = Map(f -> Set(R1))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(taintAnalysisResults(IRWalk.lastInProc(f).get) == Set(R0, R1))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults(IRWalk.lastInProc(f).get)(R0) == FiniteSet(Set(R1, R2)))
    assert(varDepResults(IRWalk.lastInProc(f).get)(R1) == FiniteSet(Set(R1, R2)))
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
    convertReturns(program)

    val f = program.nameToProcedure("f")
    val taint: Map[CFGPosition, Set[Variable]] = Map(f -> Set(R1))
    val taintAnalysisResults = getTaintAnalysisResults(program, taint)

    assert(taintAnalysisResults(IRWalk.lastInProc(f).get) == Set(R1))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults(IRWalk.lastInProc(f).get)(R0) == FiniteSet(Set(R2)))
  }
}
