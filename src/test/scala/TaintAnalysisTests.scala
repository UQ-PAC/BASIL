import analysis.*
import boogie.*
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil

class TaintAnalysisTests extends AnyFunSuite, TestUtil {
  def getTaintAnalysisResults(program: Program, procedure: Procedure, taint: Map[CFGPosition, Set[Taintable]]): Map[CFGPosition, Set[Taintable]] = {
    val constPropResults = ConstantPropagationSolver(program).analyze()
    TaintAnalysis(program, Map(), constPropResults, taint).analyze().map { (c, m) => (c, m.map { (v, _) => v }.toSet)}
  }

  def getVarDepResults(program: Program, procedure: Procedure): Map[CFGPosition, Map[Taintable, Set[Taintable]]] = {
    val constPropResults = ConstantPropagationSolver(program).analyze()
    val variables = registers
    ProcVariableDependencyAnalysis(program, variables, Map(), constPropResults, Map(), procedure).analyze()
  }

  val registers = 0.to(28).map { n => Register(s"R$n", 64): Taintable }.toSet
  val baseRegisterMap = registers.map { r => (r, Set(r)) }.toMap

  test("constantLiteral") {
    var program = prog(
        proc("main",
          block("main",
            directCall("f"), 
            goto("mainRet")
          ),
          block("mainRet", ret)
        ),
        proc("f",
          block("assign",
            Assign(R0, bv64(2), None),
            goto("returnBlock"),
          ),
          block("returnBlock",
            ret
          ),
        )
      )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.procs("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R0))
    val taintAnalysisResults = getTaintAnalysisResults(program, f, taint)

    assert(taintAnalysisResults.get(IRWalk.lastInProc(f)) == None)

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults.get(IRWalk.lastInProc(f)) == Some(baseRegisterMap - R0))
  }

  test("arguments") {
    var program = prog(
        proc("main",
          block("main",
            directCall("f"), 
            goto("mainRet")
          ),
          block("mainRet", ret)
        ),
        proc("f",
          block("assign",
            Assign(R0, BinaryExpr(BVADD, R0, R1), None),
            goto("returnBlock"),
          ),
          block("returnBlock",
            ret
          ),
        ),
      )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.procs("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R0))
    val taintAnalysisResults = getTaintAnalysisResults(program, f, taint)

    assert(taintAnalysisResults.get(IRWalk.lastInProc(f)) == Some(Set(R0)))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults.get(IRWalk.lastInProc(f)) == Some(baseRegisterMap + (R0 -> Set(R0, R1))))
  }

  test("branching") {
    var program = prog(
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
            Assign(R0, R1, None),
            goto("returnBlock"),
          ),
          block("b",
            Assign(R0, R2, None),
            goto("returnBlock"),
          ),
          block("returnBlock",
            ret
          ),
        ),
      )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.procs("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R1))
    val taintAnalysisResults = getTaintAnalysisResults(program, f, taint)

    assert(taintAnalysisResults.get(IRWalk.lastInProc(f)) == Some(Set(R0, R1)))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults.get(IRWalk.lastInProc(f)) == Some(baseRegisterMap + (R0 -> Set(R1, R2))))
  }

  test("interproc") {
    var program = prog(
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
            Assign(R1, R1, None),
            directCall("g"),
            goto("returnBlock"),
          ),
          block("b",
            Assign(R1, R2, None),
            directCall("g"),
            goto("returnBlock"),
          ),
          block("returnBlock",
            ret
          ),
        ),
        proc("g",
          block("body",
            Assign(R0, R1, None),
            goto("returnBlock"),
          ),
          block("returnBlock",
            ret
          ),
        ),
      )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.procs("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R1))
    val taintAnalysisResults = getTaintAnalysisResults(program, f, taint)

    assert(taintAnalysisResults.get(IRWalk.lastInProc(f)) == Some(Set(R0, R1)))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults.get(IRWalk.lastInProc(f)) == Some(baseRegisterMap + (R0 -> Set(R1, R2)) + (R1 -> Set(R1, R2))))
  }

  test("loop") {
    var program = prog(
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
            Assign(R0, BinaryExpr(BVADD, R0, R1), None),
            goto("branch"),
          ),
          block("b",
            Assign(R0, R2, None),
            goto("returnBlock"),
          ),
          block("returnBlock",
            ret
          ),
        ),
      )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.procs("f")
    val taint: Map[CFGPosition, Set[Taintable]] = Map(f -> Set(R1))
    val taintAnalysisResults = getTaintAnalysisResults(program, f, taint)

    assert(taintAnalysisResults.get(IRWalk.lastInProc(f)) == Some(Set(R1)))

    val varDepResults = getVarDepResults(program, f)

    assert(varDepResults.get(IRWalk.lastInProc(f)) == Some(baseRegisterMap + (R0 -> Set(R2))))
  }
}
