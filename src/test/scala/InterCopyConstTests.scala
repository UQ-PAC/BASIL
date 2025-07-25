import analysis.*
import ir.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import cilvisitor.*
import ir.dsl.*
import ir.transforms.ConstCopyPropTransform


/**
 * Unit tests for copy-constant propagation. Note: tests of the relevant transform have not been done (idk how to test
 * it, but it seems to work)
 * */
@test_util.tags.UnitTest
class InterCopyConstTests extends AnyFunSuite, CaptureOutput {

  /**
   * Extract actual BitVecLiteral from given FlatElement of lattice. Do not use unless it is known that the FlatElement
   * contains a BitVecLiteral and not Top/Bottom
   */
  def get_bv(a: FlatElement[BitVecLiteral]): Option[BitVecLiteral] =
    a match
      case FlatEl(x) => Some(x)
      case _ => None // SHOULD BE UNREACHABLE


  def getInterCopyConstResults(program: Program, paramaterForm: Boolean): Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] = {
    InterCopyConst(program, paramaterForm).analyze()
  }

  test("intraCopyAssign") {
    val program = prog(
      proc("main", block("main", LocalAssign(R0, bv64(3)), LocalAssign(R1, R0), goto("mainRet")), block("mainRet", ret)),
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("main")
    val results = getInterCopyConstResults(program, false)

    assert(get_bv(results(IRWalk.lastInProc(f).get)(R0)).get == bv64(3))
    assert(get_bv(results(IRWalk.lastInProc(f).get)(R1)).get == bv64(3))


  }



  test("intraAssignOverride") {
    // all good
    val program = prog(
      proc("main", block("main", LocalAssign(R0, bv64(3)), LocalAssign(R0, bv64(5)), goto("mainRet")), block("mainRet", ret)),
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("main")
    val results = getInterCopyConstResults(program, false)
    assert(get_bv(results(IRWalk.lastInProc(f).get)(R0)).get == bv64(5))


  }

  test("intraNonCopyAssignOverride") {
    val program = prog(
      proc("main", block("main", LocalAssign(R0, bv64(3)), LocalAssign(R0, BinaryExpr(BVADD, R0, bv64(1))), LocalAssign(R1, R0), goto("mainRet")), block("mainRet", ret)),
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("main")
    val results = getInterCopyConstResults(program, false)
    assert((results(IRWalk.lastInProc(f).get)(R1)) == Top)

  }




  test("interAssign") {

    val program = prog(
      proc("main",
        block("main", directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("in" -> bv64(7))), goto("mainRet")),
        block("mainRet", ret)
      ),
      proc("f",
        Seq("in" -> BitVecType(64)), Seq("p_out" -> BitVecType(64)),
        block("okay", goto("f_ret")),
        block("f_ret", ret("p_out" -> bv64(5))),
      )

    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)


    val f = program.nameToProcedure("main")
    val results = getInterCopyConstResults(program, true)


    assert(get_bv(results(IRWalk.lastInProc(f).get)(LocalVar("v", BitVecType(64)))).get == bv64(5))


  }

  test("interAssignVariableParameter") {

    val program = prog(
      proc("main",
        block("main", LocalAssign(R0, bv64(3)), directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("in" -> R0)), goto("mainRet")),
        block("mainRet", ret)
      ),
      proc("f",
        Seq("in" -> BitVecType(64)), Seq("p_out" -> BitVecType(64)),
        block("okay", goto("f_ret")),
        block("f_ret", ret("p_out" -> LocalVar("in", BitVecType(64)))),
      )

    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)


    val f = program.nameToProcedure("main")
    val results = getInterCopyConstResults(program, true)
    assert(get_bv(results(IRWalk.lastInProc(f).get)(LocalVar("v", BitVecType(64)))).get == bv64(3))

  }

  test("interMultipleOutParams") {

    val program = prog(
      proc("main",
        block("main", directCall(Seq("p_out_1" -> LocalVar("v", BitVecType(64)), "p_out_2" -> LocalVar("a", BitVecType(64))), "f", Seq("in" -> bv64(7))), goto("mainRet")),
        block("mainRet", ret)
      ),
      proc("f",
        Seq("in" -> BitVecType(64)), Seq("p_out_1" -> BitVecType(64), "p_out_2" -> BitVecType(64)),
        block("okay", goto("f_ret")),
        block("f_ret", ret("p_out_1" -> bv64(5), "p_out_2" -> bv64(1))),
      )

    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("main")
    val results = getInterCopyConstResults(program, true)
    assert(get_bv(results(IRWalk.lastInProc(f).get)(LocalVar("v", BitVecType(64)))).get == bv64(5))
    assert(get_bv(results(IRWalk.lastInProc(f).get)(LocalVar("a", BitVecType(64)))).get == bv64(1))

  }





  test("interReturnOneNonConstantOneConstant") {


    val program = prog(
      proc("main",
        block("main", directCall(Seq("p_out_1" -> LocalVar("a", BitVecType(64)), "p_out_2" -> LocalVar("b", BitVecType(64))), "f", Seq("in" -> bv64(7))), goto("mainRet")),
        block("mainRet", ret)
      ),
      proc("f",
        Seq("in" -> BitVecType(64)), Seq("p_out_1" -> BitVecType(64), "p_out_2" -> BitVecType(64)),
        block("okay", goto("f_ret")),
        block("f_ret", ret("p_out_1" -> bv64(2), "p_out_2" -> BinaryExpr(BVAND, LocalVar("in", BitVecType(64)), bv64(1))))
      )

    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val f = program.nameToProcedure("main")
    val results = getInterCopyConstResults(program, true)
    assert(get_bv(results(IRWalk.lastInProc(f).get)(LocalVar("a", BitVecType(64)))).get == bv64(2))
    assert(results(IRWalk.lastInProc(f).get)(LocalVar("b", BitVecType(64))) == Top)
  }




  test("callProcTwiceDiffContext") {
    val program = prog(
      proc("main",
        block("main", directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("p_in" -> bv64(3))), goto("main2")),
        block("main2", directCall(Seq("p_out" -> LocalVar("a", BitVecType(64))), "f", Seq("p_in" -> BinaryExpr(BVAND, bv64(1), bv64(7)))), goto("mainRet")),
        block("mainRet", ret)
      ),
      proc("f",
        Seq("p_in" -> BitVecType(64)), Seq("p_out" -> BitVecType(64)),
        block("okay", goto("f_ret")),
        block("f_ret", ret("p_out" -> LocalVar("p_in", BitVecType(64))))
      )

    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)


    val f = program.nameToProcedure("main")
    val results = getInterCopyConstResults(program, true)
    assert(get_bv(results(IRWalk.lastInProc(f).get)(LocalVar("v", BitVecType(64)))).get == bv64(3))
    assert(results(IRWalk.lastInProc(f).get)(LocalVar("a", BitVecType(64))) == Top)


  }

  

}