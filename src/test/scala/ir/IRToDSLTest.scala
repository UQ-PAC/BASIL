package ir

import scala.collection.mutable
import scala.collection.immutable.*
import org.scalatest.funsuite.AnyFunSuite
import util.intrusive_list.*
import translating.serialiseIL
import ir.dsl.*
import ir.*

import org.scalactic.Prettifier
import org.scalactic._

class IRToDSLTest extends AnyFunSuite {

  val mainproc = proc(
    "main",
    block("l_main",
      LocalAssign(R0, bv64(10)),
      directCall("p1"),
      indirectCall(R0),
      goto("returntarget")
    ),
    block("returntarget", ret)
  )

  val p = prog(
    mainproc,
    proc("p1", block("b1", LocalAssign(R0, bv64(10)), ret)),
  )

  /**
   * Compares expected and actual by first converting both to their
   * string representations.
   *
   * Used as a quick fix to get structural equality.
   */
  inline def assertResultWithToString(expected: Any)(actual: Any) = {
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
    assertResultWithToString(ResolvableStatement(lassign)) {
      IRToDSL.convertCommand(lassign)
    }

    val directcallstmt = p.preOrderIterator.collectFirst { case x: DirectCall => x }.head
    assertResult(directCall("p1")) {
      IRToDSL.convertCommand(directcallstmt)
    }

    val gotostmt = p.preOrderIterator.collectFirst { case x: GoTo => x }.head
    assertResult(goto("returntarget")) {
      IRToDSL.convertCommand(gotostmt)
    }

    val retstmt = p.preOrderIterator.collectFirst { case x: Return => x }.head
    assertResult(ret) {
      IRToDSL.convertCommand(retstmt)
    }

    val indircall = p.preOrderIterator.collectFirst { case x: IndirectCall => x }.head
    assertResult(indirectCall(R0)) {
      IRToDSL.convertCommand(indircall)
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

  test("function parameters proc to dsl") {
    val p = IRToDSLTestData.function1
    val procedure = p.nameToProcedure("main")
    assertResultWithToString(mainproc) {
      IRToDSL.convertProcedure(procedure)
    }

  }
}



