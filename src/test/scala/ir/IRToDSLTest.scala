package ir

import ir.*
import ir.dsl.*
import org.scalactic.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.{BASILTest, CaptureOutput}
import translating.PrettyPrinter.*
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, LogLevel, Logger}

import scala.collection.immutable.*

@test_util.tags.UnitTest
class IRToDSLTest extends AnyFunSuite with CaptureOutput {

  override def withFixture(test: NoArgTest) = {
    DeepEquality.debug.withValue(true) {
      super.withFixture(test)
    }
  }

  val mainproc = proc(
    "main",
    block(
      "l_main",
      LocalAssign(R0, bv64(10)),
      LocalAssign(R1, bv64(10)).setComment(Some("comm:assign")),
      directCall("p1").copy(comment = Some("comm:direct")),
      indirectCall(R0).copy(comment = Some("comm:indirect")),
      goto("returntarget").copy(comment = Some("comm:return~"))
    ),
    block("returntarget", ret)
  ).cloneable

  val p = prog(mainproc, proc("p1", block("b1", LocalAssign(R0, bv64(10)), ret)))

  /*
   * Strongest structural equality on programs (including labels and comments)
   */
  inline def assertDeepEquality[T <: DeepEquality](expected: T)(actual: T) = {
    assert(
      expected.deepEqualsDbg(actual),
      s"deep equality ${expected.getClass.getSimpleName} != ${actual.getClass.getSimpleName}"
    )
  }

  /**
   * Compares expected and actual by first converting both to their
   * string representations.
   *
   * Used as a quick fix to get structural equality.
   */
  inline def assertPrintedEquality[T <: PrettyPrintable](expected: T)(actual: T) = {
    assert(expected.pprint == actual.pprint, s"pretty printed equality")
  }

  /**
   * Asserts structural equality on the IR via after a round-trip through the parser
   */
  inline def assertSerialisedParsedEqual(expected: Program) = {
    val expectedStr = expected.pprint
    val actual = ir.parsing.ParseBasilIL.loadILString(expectedStr).program
    val actualStr = actual.pprint
    assert(expectedStr == actualStr, "serialise-parse serialisation not equal")
    assert(
      expected.deepEqualsDbg(actual),
      s"serialise-parse deep equality ${expected.getClass.getSimpleName} != ${actual.getClass.getSimpleName}"
    )
  }

  /**
   * XXX: The assertions use /structural/ equality on the DSL's "Eventually"
   * classes. These succeed only when the precise types of all arguments are
   * equal (e.g., List vs Array). We have unified the types so the DSL
   * DSL construction and the Basil IR to DSL conversion use the same types,
   * but this is something to be aware of. In particular, the compiler may
   * change the type for varargs to something other than Array.
   */
  test("commands to dsl") {
    val lassign = LocalAssign(R0, bv64(10))
    assertDeepEquality(CloneableStatement(lassign)) {
      IRToDSL.convertStatement(lassign)
    }

    val directcallstmt = p.preOrderIterator.collectFirst { case x: DirectCall => x }.head
    assertResult(directCall("p1").copy(comment = Some("comm:direct"))) {
      IRToDSL.convertStatement(directcallstmt)
    }

    val gotostmt = p.preOrderIterator.collectFirst { case x: GoTo => x }.head
    assertResult(goto("returntarget").copy(comment = Some("comm:return~"))) {
      IRToDSL.convertJump(gotostmt)
    }

    val retstmt = p.preOrderIterator.collectFirst { case x: Return => x }.head
    assertResult(ret) {
      IRToDSL.convertJump(retstmt)
    }

    val indircall = p.preOrderIterator.collectFirst { case x: IndirectCall => x }.head
    assertResult(indirectCall(R0).copy(comment = Some("comm:indirect"))) {
      IRToDSL.convertStatement(indircall)
    }
  }

  test("proc to dsl") {
    val procedure = p.nameToProcedure("main")
    val n = IRToDSL.convertProcedure(procedure)
    assertDeepEquality(mainproc)(n)
  }

  test("prog to dsl") {
    val n = IRToDSL.convertProgram(p).resolve
    assertDeepEquality(p)(n)
    assertSerialisedParsedEqual(p)
  }

  test("function1 procs to dsl (with params)") {
    val dslprog = IRToDSLTestData.function1
    val irprog = dslprog.resolve

    // for each procedure, check that the conversion is correct,
    // i.e., is structurally equal to the original dsl procedure
    (dslprog.allProcedures zip irprog.procedures).foreach { case (dslproc, proc) =>
      assertDeepEquality(dslproc) { IRToDSL.convertProcedure(proc) }
    }
  }

  test("function1 prog to dsl (with params)") {
    val p = IRToDSLTestData.function1.resolve
    val cloned = IRToDSL.convertProgram(p).resolve
    assertDeepEquality(p)(cloned)
  }

  test("equality on loaded ir params") {
    Logger.setLevel(LogLevel.ERROR)
    val path = s"${BASILTest.rootDirectory}/src/test/correct/function1/gcc/function1"

    val loaded = util.RunUtils.loadAndTranslate(
      BASILConfig(
        loading =
          ILLoadingConfig(inputFile = path + ".adt", relfFile = Some(path + ".relf"), specFile = None, dumpIL = None),
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out.bpl",
        simplify = true
      )
    )

    val prog = loaded.ir.program
    val cloned = IRToDSL.convertProgram(prog).resolve

    for (orig <- prog) {
      for (clone <- cloned) {
        assert(orig ne cloned, "No references to control-flow objects shared between IR")
      }
    }

    // prog.sortProceduresRPO()
    // cloned.sortProceduresRPO()

    val main = prog.mainProcedure
    val clonedMain = cloned.mainProcedure

    assert(main.address.nonEmpty)
    assert(clonedMain.address == main.address)
    assert(clonedMain.procName == main.procName)
    assert(clonedMain.name == main.name)

    assert(clonedMain.formalInParam == main.formalInParam)
    assert(clonedMain.formalOutParam == main.formalOutParam)

    val clonedProcs = cloned.procedures.map(p => p.name -> p).toMap
    for (p <- prog.procedures) {
      assert(clonedProcs.contains(p.name))
      val cloned = clonedProcs(p.name)
      assert(p.formalInParam == cloned.formalInParam)
      assert(p.formalOutParam == cloned.formalOutParam)

      val clonedBlocks = cloned.blocks.map(b => b.label -> b).toMap

      for (b <- p.blocks) {
        assert(clonedBlocks.contains(b.label))
        assert(b.deepEquals(clonedBlocks(b.label)))
      }
      assertResult(p.returnBlock.map(_.label) == cloned.returnBlock.map(_.label))
    }
    assertDeepEquality(prog)(cloned)
    assertPrintedEquality(prog)(cloned)
    assertSerialisedParsedEqual(prog)

    // info(PrettyPrinter.pp_prog(cloned))
  }

  test("equality on loaded ir no params") {
    Logger.setLevel(LogLevel.ERROR)
    val path = s"${BASILTest.rootDirectory}/src/test/correct/function1/gcc/function1"

    val loaded = util.RunUtils.loadAndTranslate(
      BASILConfig(
        loading =
          ILLoadingConfig(inputFile = path + ".adt", relfFile = Some(path + ".relf"), specFile = None, dumpIL = None),
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out.bpl"
      )
    )

    val prog = loaded.ir.program
    val cloned = IRToDSL.convertProgram(prog).resolve

    for (orig <- prog) {
      for (clone <- cloned) {
        assert(orig ne cloned, "No references to control-flow objects shared between IR")
      }
    }

    prog.sortProceduresRPO()
    cloned.sortProceduresRPO()

    val main = prog.mainProcedure
    val clonedMain = cloned.mainProcedure

    assert(main.address.nonEmpty)
    assert(clonedMain.address == main.address)
    assert(clonedMain.procName == main.procName)
    assert(clonedMain.name == main.name)

    assert(clonedMain.formalInParam == main.formalInParam)
    assert(clonedMain.formalOutParam == main.formalOutParam)

    assertDeepEquality(prog)(cloned)
    assertPrintedEquality(prog)(cloned)
    assertSerialisedParsedEqual(prog)
  }

}
