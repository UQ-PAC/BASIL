import org.scalatest.funsuite.AnyFunSuite
import util.{IRLoading, ILLoadingConfig, Logger}
import ir.*
import ir.dsl.*
import ir.cilvisitor.*
import translating.{FindUninterpretedFunctions}

@test_util.tags.UnitTest
class GTIRBLoaderTest extends AnyFunSuite {

  def withLogCapture[T](body: => T): (T, String) = {
    val outStream = java.io.ByteArrayOutputStream()
    val errorLog = java.io.PrintStream(outStream)

    Logger.setOutput(errorLog)

    val x = body

    (x, outStream.toString)
  }

  def ite(c: Expr, t: Expr, f: Expr) =
    UninterpretedFunction("ite.temp", Seq(c, t, f), t.getType)

  def loadGTIRB(relfFile: String, gtsFile: String) = {
    val config =
      ILLoadingConfig(gtsFile, relfFile)
    val (symbols, externalFunctions, globals, funcEntries, globalOffsets, mainAddress) =
      IRLoading.loadReadELF(relfFile, config)

    IRLoading.loadGTIRB(gtsFile, mainAddress)
  }

  test("sdiv loads with ite expression") {
    val (prog, errs) = withLogCapture {
      loadGTIRB("src/test/correct/sdiv/gcc_O2/sdiv.relf", "src/test/correct/sdiv/gcc_O2/sdiv.gts")
    }
    // import ir.dsl.{given}
    // println(prog.mainProcedure.toScala)

    // assert(!(errs.contains("\"ite.0\"")))
    assert(errs.isEmpty)

    val assertFalses = prog.collect { case x @ Assert(FalseLiteral, _, _) => x }.toList
    assert(assertFalses.isEmpty)

    val tempif = prog.collect { case x: translating.TempIf => x }.toList
    assert(tempif.isEmpty)
    // println(prog.toList)

    val v = FindUninterpretedFunctions()
    visit_proc(v, prog.mainProcedure)
    assert(v.uninterp.isEmpty)
  }

  test("nested ite") {
    val p = prog(
      proc(
        "mainproc",
        block(
          "fdsjaio",
          LocalAssign(R0, bv64(0)),
          LocalAssign(
            LocalVar("asd", BitVecType(64)),
            ite(ite(FalseLiteral, bv64(100), bv64(110)), ite(FalseLiteral, bv64(2000), bv64(2100)), bv64(210))
          ),
          LocalAssign(
            LocalVar("asd2", BitVecType(64)),
            ite(ite(FalseLiteral, bv64(100), bv64(110)), ite(FalseLiteral, bv64(2000), bv64(2100)), bv64(210))
          ),
          ret
        )
      )
    )
    // println(p)

    val conv = translating.ConvertITEToTempIf("TEST")

    val b = p.mainProcedure.entryBlock.get
    visit_block(conv, b)

    // XXX: we use toString on statements to obtain a representation of the
    // resulting statements, because TEMPIF is a subclass of NOP most other
    // serialisers in BASIL will print it as a no-op.
    val actual = b.statements.toList.map(_.toString).mkString("\n")
    // println(actual)

    // tests several things:
    // - ite can appear in the condition of an ite and is correctly handled.
    // - ite can appear in the body of an ite
    // - consecutive ites use disjoint variables
    val expected = raw"""
Register(R0, bv64) := 0bv64
TEMPIF(false, List(LocalVar(ite_result_TEST_1, bv64) := 100bv64), List(LocalVar(ite_result_TEST_1, bv64) := 110bv64))
TEMPIF(false, List(LocalVar(ite_result_TEST_2, bv64) := 2000bv64), List(LocalVar(ite_result_TEST_2, bv64) := 2100bv64))
TEMPIF(LocalVar(ite_result_TEST_1, bv64), List(LocalVar(ite_result_TEST_3, bv64) := LocalVar(ite_result_TEST_2, bv64)), List(LocalVar(ite_result_TEST_3, bv64) := 210bv64))
LocalVar(asd, bv64) := LocalVar(ite_result_TEST_3, bv64)
TEMPIF(false, List(LocalVar(ite_result_TEST_4, bv64) := 100bv64), List(LocalVar(ite_result_TEST_4, bv64) := 110bv64))
TEMPIF(false, List(LocalVar(ite_result_TEST_5, bv64) := 2000bv64), List(LocalVar(ite_result_TEST_5, bv64) := 2100bv64))
TEMPIF(LocalVar(ite_result_TEST_4, bv64), List(LocalVar(ite_result_TEST_6, bv64) := LocalVar(ite_result_TEST_5, bv64)), List(LocalVar(ite_result_TEST_6, bv64) := 210bv64))
LocalVar(asd2, bv64) := LocalVar(ite_result_TEST_6, bv64)
    """.trim

    assertResult(expected) { actual }
  }
}
