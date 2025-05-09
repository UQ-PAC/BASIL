package ir

import scala.collection.immutable.*
import org.scalatest.funsuite.AnyFunSuite
import ir.dsl.*
import ir.*
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, Logger, LogLevel}
import translating.PrettyPrinter
import test_util.{BASILTest, CaptureOutput}

import org.scalactic.*

@test_util.tags.UnitTest
class IRToDSLTest extends AnyFunSuite with CaptureOutput {

  val mainproc = proc(
    "main",
    block(
      "l_main",
      LocalAssign(R0, bv64(10)),
      LocalAssign(R1, bv64(10)),
      directCall("p1"),
      indirectCall(R0),
      goto("returntarget")
    ),
    block("returntarget", ret)
  ).cloneable

  val p = prog(mainproc, proc("p1", block("b1", LocalAssign(R0, bv64(10)), ret)))

  /**
   * Compares expected and actual by first converting both to their
   * string representations.
   *
   * Used as a quick fix to get structural equality.
   */
  inline def assertResultWithToString[T](expected: T)(actual: T) = {
    assertResult(expected.toString)(actual.toString)
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
    assertResultWithToString(CloneableStatement(lassign)) {
      IRToDSL.convertStatement(lassign)
    }

    val directcallstmt = p.preOrderIterator.collectFirst { case x: DirectCall => x }.head
    assertResult(directCall("p1")) {
      IRToDSL.convertStatement(directcallstmt)
    }

    val gotostmt = p.preOrderIterator.collectFirst { case x: GoTo => x }.head
    assertResult(goto("returntarget")) {
      IRToDSL.convertJump(gotostmt)
    }

    val retstmt = p.preOrderIterator.collectFirst { case x: Return => x }.head
    assertResult(ret) {
      IRToDSL.convertJump(retstmt)
    }

    val indircall = p.preOrderIterator.collectFirst { case x: IndirectCall => x }.head
    assertResult(indirectCall(R0)) {
      IRToDSL.convertStatement(indircall)
    }
  }

  test("proc to dsl") {
    val procedure = p.nameToProcedure("main")
    assertResultWithToString(mainproc) {
      IRToDSL.convertProcedure(procedure)
    }
  }

  test("prog to dsl") {
    assertResultWithToString(p) {
      IRToDSL.convertProgram(p).resolve
    }
  }

  test("function1 procs to dsl (with params)") {
    val dslprog = IRToDSLTestData.function1
    val irprog = dslprog.resolve

    // for each procedure, check that the conversion is correct,
    // i.e., is structurally equal to the original dsl procedure
    (dslprog.allProcedures zip irprog.procedures).foreach { case (dslproc, proc) =>
      assertResultWithToString(dslproc) { IRToDSL.convertProcedure(proc) }
    }
  }

  test("function1 prog to dsl (with params)") {
    val p = IRToDSLTestData.function1.resolve
    assertResultWithToString(p) {
      IRToDSL.convertProgram(p).resolve
    }
  }

  test("equality on loaded ir params") {
    Logger.setLevel(LogLevel.ERROR)
    val path = s"${BASILTest.rootDirectory}/src/test/correct/function1/gcc/function1"

    val loaded = util.RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(inputFile = path + ".adt", relfFile = path + ".relf", specFile = None, dumpIL = None),
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

    val clonedProcs = cloned.procedures.map(p => p.name -> p).toMap
    for (p <- prog.procedures) {
      assert(clonedProcs.contains(p.name))
      assert(p.formalInParam == clonedProcs(p.name).formalInParam)
      assert(p.formalOutParam == clonedProcs(p.name).formalOutParam)

      val clonedBlocks = p.blocks.map(b => b.label -> b).toMap

      for (b <- p.blocks) {
        assert(clonedBlocks.contains(b.label))
        assert(PrettyPrinter.pp_block(b) == PrettyPrinter.pp_block(clonedBlocks(b.label)))
      }
    }

    // info(PrettyPrinter.pp_prog(cloned))
  }

  test("equality on loaded ir no params") {
    Logger.setLevel(LogLevel.ERROR)
    val path = s"${BASILTest.rootDirectory}/src/test/correct/function1/gcc/function1"

    val loaded = util.RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(inputFile = path + ".adt", relfFile = path + ".relf", specFile = None, dumpIL = None),
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

    assertResultWithToString(PrettyPrinter.pp_prog(prog))(PrettyPrinter.pp_prog(cloned))
    // info(PrettyPrinter.pp_prog(cloned))
  }

}
