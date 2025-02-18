package ir

import scala.collection.mutable
import scala.collection.immutable.*
import org.scalatest.funsuite.AnyFunSuite
import util.intrusive_list.*
import translating.serialiseIL
import ir.dsl.*
import ir.*

import org.scalactic.Prettifier

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

  test("commands to dsl") {
    val lassign = LocalAssign(R0, bv64(10))
    assertResult(ResolvableStatement(lassign)) {
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
    assertResult(mainproc) {
      IRToDSL.convertProcedure(procedure)
    }
  }

  test("prog to dsl") {
    assert(p != p)
    // println(p)
  }
}

class A extends Iterable[A] {
  var n = 0
  def iterator =
    n = n + 1
    if n < 1000 then List(this, this).iterator else Iterator()
  override def toString = "A.toString"
}

class BadTest extends AnyFunSuite {

  val p = prog(
    proc("p1", block("b1", ret)),
  )

  test("assert failrue") {
    println(p.getClass().getMethods.map(m => m.getName -> m.getDeclaringClass).toSeq)
    println(classOf[A].getMethods.map(m => m.getName -> m.getDeclaringClass).toSeq)
    assert(A() == A())
  }
}

