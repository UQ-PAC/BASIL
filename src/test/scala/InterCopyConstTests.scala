import analysis.*
import ir.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import cilvisitor.*
import ir.dsl.*

@test_util.tags.UnitTest
class InterCopyConstTests extends AnyFunSuite, CaptureOutput {


  def getInterCopyConstResults(program: Program, paramaterForm: Boolean): Unit = {
    print(InterCopyConst(program, paramaterForm).analyze())
  }

  test("intraCopyAssign") {
    // all good
    val program = prog(
      proc("main", block("main", LocalAssign(R0, bv64(3)), LocalAssign(R1, R0), goto("mainRet")), block("mainRet", ret)),
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    getInterCopyConstResults(program, false)
  }

  test("intraOldTransform") {
    val program = prog(
      proc("main",
        Seq(),
        Seq("R0_out" -> BitVecType(64), "R1_out" -> BitVecType(64)),
        block("main",
          LocalAssign(LocalVar("R0", BitVecType(64), 0), bv64(3)),
          LocalAssign(LocalVar("R1", BitVecType(64), 0), LocalVar("R0", BitVecType(64), 0)),
          goto("mainRet")
        ),
        block("mainRet",
          ret(
            "R0_out" -> LocalVar("R0", BitVecType(64), 0),
            "R1_out" -> LocalVar("R1", BitVecType(64), 0)
          )
        )
      )
    )
    transforms.copyPropParamFixedPoint(program, Map())
    print(program)
  }

  test("intraNewTransform") {
    val program = prog(
      proc("main",
        Seq(),
        Seq("R0_out" -> BitVecType(64), "R1_out" -> BitVecType(64)),
        block("main",
          LocalAssign(LocalVar("R0", BitVecType(64), 0), bv64(3)),
          LocalAssign(LocalVar("R1", BitVecType(64), 0), LocalVar("R0", BitVecType(64), 0)),
          goto("mainRet")
        ),
        block("mainRet",
          ret(
            "R0_out" -> LocalVar("R0", BitVecType(64), 0),
            "R1_out" -> LocalVar("R1", BitVecType(64), 0)
          )
        )
      )
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)


    print(program)
    visit_prog(transforms.ConstCopyPropTransform(program), program)
    print(program)
  }

  test("intraAssignOverride") {
    // all good
    val program = prog(
      proc("main", block("main", LocalAssign(R0, bv64(3)), LocalAssign(R0, bv64(5)), goto("mainRet")), block("mainRet", ret)),
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    println(program)

    getInterCopyConstResults(program, false)
  }

  test("intraNonCopyAssignOverride") {
    // all good
    val program = prog(
      proc("main", block("main", LocalAssign(R0, bv64(3)), LocalAssign(R0, BinaryExpr(BVADD, R0, bv64(1))), LocalAssign(R1, R0), goto("mainRet")), block("mainRet", ret)),
    )
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)
    print(program)

    getInterCopyConstResults(program, false)

    visit_prog(transforms.ConstCopyPropTransform(program), program)
    print(program)
  }


  test("intraNonCopyAssign") {
    val program = prog(
      proc("main", block("main", LocalAssign(R1, bv64(3)), LocalAssign(R0, BinaryExpr(BVADD, R0, bv64(1))), goto("mainRet")), block("mainRet", ret)),
    )
    println(program)

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    getInterCopyConstResults(program, false)
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

    println(program)

    getInterCopyConstResults(program, true)

    //visit_prog(transforms.ConstCopyPropTransform(program), program)

    transforms.copyPropParamFixedPoint(program, Map())
    print(program)
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

    println(program)

    getInterCopyConstResults(program, true)


    transforms.copyPropParamFixedPoint(program, Map())

    //visit_prog(transforms.ConstCopyPropTransform(program), program)
    print(program)
  }


  test("interCallProcNoOutParam") {

    val program = prog(
      proc("main",
        block("main", directCall("f"), goto("mainRet")),
        block("mainRet", ret)
      ),
      proc("f",
        block("okay", LocalAssign(LocalVar("R0", bv64), bv64(3)) ,goto("f_ret")),
        block("f_ret", ret()),
      )

    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    println(program)

    getInterCopyConstResults(program, true)

    visit_prog(transforms.ConstCopyPropTransform(program), program)
    print(program)
  }


  test("interReturnNonConstant") {

    val program = prog(
      proc("main",
        block("main", directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("in" -> bv64(7))), goto("mainRet")),
        block("mainRet", ret)
      ),
      proc("f",
        Seq("in" -> BitVecType(64)), Seq("p_out" -> BitVecType(64)),
        block("okay", goto("f_ret")),
        block("f_ret", ret("p_out" -> BinaryExpr(BVADD, bv64(2), bv64(1)))),
      )

    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    println(program)


    getInterCopyConstResults(program, true)

    //cilvisitor.visit_prog(transforms.ConstCopyPropTransform(program), program)
    transforms.copyPropParamFixedPoint(program, Map())

    print(program)
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
        block("f_ret", ret("p_out_1" -> BinaryExpr(BVADD, LocalVar("in", BitVecType(64)), bv64(1)) , "p_out_2" -> bv64(1))),
      )

    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    println(program)


    getInterCopyConstResults(program, true)

    //cilvisitor.visit_prog(transforms.ConstCopyPropTransform(program), program)
    transforms.copyPropParamFixedPoint(program, Map())
    print(program)
  }

  test("interAssignOverride") {
    val program = prog(
      proc("main",
        block("main", LocalAssign(LocalVar("v", BitVecType(64)), bv64(2)), directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("in" -> bv64(7))), goto("mainRet")),
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


    println(program)


    getInterCopyConstResults(program, true)
  }


  test("interReturnLocalVar") {
    val program = prog(
      proc("main",
        block("main", directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("p_in" -> bv64(7))), goto("mainRet")),
        block("mainRet", ret)
      ),
      proc("f",
        Seq("p_in" -> BitVecType(64)), Seq("p_out" -> BitVecType(64)),
        block("okay", LocalAssign(LocalVar("bruh", BitVecType(64)), bv64(6)), goto("f_ret")),
        block("f_ret", ret("p_out" -> LocalVar("bruh", BitVecType(64))))
      )

    )


    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)


    println(program)


    getInterCopyConstResults(program, true)
  }

  test("interReturnInParam") {
    val program = prog(
      proc("main",
        block("main", directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("p_in" -> bv64(7))), goto("mainRet")),
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


    println(program)


    getInterCopyConstResults(program, true)
  }




  test("unusedLocalVar") {
    val program = prog(
      proc("main",
        block("main", LocalAssign(LocalVar("unused", BitVecType(64)), bv64(2)), directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("in" -> bv64(7))), goto("mainRet")),
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

    println(program)

    getInterCopyConstResults(program, true)
  }

  test("callProcTwiceDiffContext") {
    val program = prog(
      proc("main",
        block("main", directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("p_in" -> bv64(3))), goto("main2")),
        block("main2", directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("p_in" -> bv64(7))), goto("mainRet")),
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

    println(program)

    getInterCopyConstResults(program, true)
  }

  test("callProcWithinProc") {
    val program = prog(
      proc("main",
        block("main", directCall(Seq("p_out" -> LocalVar("v", BitVecType(64))), "f", Seq("in" -> bv64(7))), goto("mainRet")),
        block("mainRet", ret)
      ),
      proc("f",
        Seq("in" -> BitVecType(64)), Seq("p_out" -> BitVecType(64)),
        block("okay", goto("f_ret")), // add another procedure here !!!
        block("f_ret", ret("p_out" -> bv64(5))),
      )

    )


    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)


    println(program)


    getInterCopyConstResults(program, true)
  }

  test ("waht") {
    val program = prog(
      proc("main",
        Seq(
          "R0_in" -> BitVecType(64),
          "R10_in" -> BitVecType(64),
          "R11_in" -> BitVecType(64),
          "R12_in" -> BitVecType(64),
          "R13_in" -> BitVecType(64),
          "R14_in" -> BitVecType(64),
          "R15_in" -> BitVecType(64),
          "R16_in" -> BitVecType(64),
          "R17_in" -> BitVecType(64),
          "R18_in" -> BitVecType(64),
          "R1_in" -> BitVecType(64),
          "R29_in" -> BitVecType(64),
          "R2_in" -> BitVecType(64),
          "R30_in" -> BitVecType(64),
          "R31_in" -> BitVecType(64),
          "R3_in" -> BitVecType(64),
          "R4_in" -> BitVecType(64),
          "R5_in" -> BitVecType(64),
          "R6_in" -> BitVecType(64),
          "R7_in" -> BitVecType(64),
          "R8_in" -> BitVecType(64),
          "R9_in" -> BitVecType(64),
          "_PC_in" -> BitVecType(64)
        ),
        Seq(
          "R0_out" -> BitVecType(64),
          "_PC_out" -> BitVecType(64)
        ),
        block("main_entry",
          LocalAssign(LocalVar("R0", BitVecType(64), 0), LocalVar("R0_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R10", BitVecType(64), 0), LocalVar("R10_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R11", BitVecType(64), 0), LocalVar("R11_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R12", BitVecType(64), 0), LocalVar("R12_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R13", BitVecType(64), 0), LocalVar("R13_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R14", BitVecType(64), 0), LocalVar("R14_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R15", BitVecType(64), 0), LocalVar("R15_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R16", BitVecType(64), 0), LocalVar("R16_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R17", BitVecType(64), 0), LocalVar("R17_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R18", BitVecType(64), 0), LocalVar("R18_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R1", BitVecType(64), 0), LocalVar("R1_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R29", BitVecType(64), 0), LocalVar("R29_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R2", BitVecType(64), 0), LocalVar("R2_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R30", BitVecType(64), 0), LocalVar("R30_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R31", BitVecType(64), 0), LocalVar("R31_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R3", BitVecType(64), 0), LocalVar("R3_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R4", BitVecType(64), 0), LocalVar("R4_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R5", BitVecType(64), 0), LocalVar("R5_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R6", BitVecType(64), 0), LocalVar("R6_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R7", BitVecType(64), 0), LocalVar("R7_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R8", BitVecType(64), 0), LocalVar("R8_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R9", BitVecType(64), 0), LocalVar("R9_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("_PC", BitVecType(64), 0), LocalVar("_PC_in", BitVecType(64), 0), None),
          LocalAssign(LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("2"), 64), Some("4195968_0")),
          goto("main_basil_return_1")
        ),
        block("main_basil_return_1",
          ret(
            "R0_out" -> LocalVar("R0", BitVecType(64), 0),
            "_PC_out" -> LocalVar("_PC", BitVecType(64), 0)
          )
        )
      )
    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program, true) // add return to all blocks because IDE solver expects it
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    println(program)
    getInterCopyConstResults(program, true)
  }

}