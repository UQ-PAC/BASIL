package ir

import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.concurrent.{Signaler, TimeLimitedTests, ThreadSignaler}
import org.scalatest.time.{Span, Seconds}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{TestData, BeforeAndAfterEachTestData}
import util.*
import ir.dsl.*
import ir.dsl.{given}
import ir.*

import scala.runtime.stdLibPatches.Predef.assert

class ToScalaTest extends AnyFunSuite with TimeLimitedTests with BeforeAndAfterEachTestData {

  override def timeLimit = Span(2, Seconds)
  override val defaultTestSignaler = ThreadSignaler

  private val currentTestCaseName = new ThreadLocal[String]

  override def beforeEach(testData: TestData): Unit = {
    currentTestCaseName.set(testData.name)
    super.beforeEach(testData)
  }

  val program: Program = prog(
    proc(
      "main",
      block(
        "first_call",
        LocalAssign(R0, bv64(1), None),
        LocalAssign(R1, bv64(1), None),
        directCall("callee1"),
        goto("second_call")
      ),
      block("second_call", directCall("callee2"), goto("returnBlock")),
      block("returnBlock", ret)
    ),
    proc("callee1", block("returnBlock", ret)),
    proc("callee2", block("returnBlock", ret)),
    proc("empty procedure")
  )

  val expected = """
prog(
  proc("main",
    block("first_call",
      LocalAssign(Register("R0", 64), BitVecLiteral(BigInt("1"), 64), None),
      LocalAssign(Register("R1", 64), BitVecLiteral(BigInt("1"), 64), None),
      directCall("callee1"),
      goto("second_call")
    ),
    block("second_call",
      directCall("callee2"),
      goto("returnBlock")
    ),
    block("returnBlock",
      ret
    )
  ),
  proc("callee1",
    block("returnBlock",
      ret
    )
  ),
  proc("callee2",
    block("returnBlock",
      ret
    )
  ),
  proc("empty procedure")
)
  """

  val expectedWithSplitting = """
{
  def `block:main.first_call` = block("first_call",
    LocalAssign(Register("R0", 64), BitVecLiteral(BigInt("1"), 64), None),
    LocalAssign(Register("R1", 64), BitVecLiteral(BigInt("1"), 64), None),
    directCall("callee1"),
    goto("second_call")
  )

  def `procedure:main` = proc("main",
    `block:main.first_call`,
    block("second_call",
      directCall("callee2"),
      goto("returnBlock")
    ),
    block("returnBlock",
      ret
    )
  )

  def `procedure:callee1` = proc("callee1",
    block("returnBlock",
      ret
    )
  )

  def `procedure:callee2` = proc("callee2",
    block("returnBlock",
      ret
    )
  )

  def program = prog(
    `procedure:main`,
    `procedure:callee1`,
    `procedure:callee2`,
    proc("empty procedure")
  )

  program
}
  """

  /**
   * Normalise output by removing beginning and ending whitespace, and removing
   * trailing whitespace from each line.
   */
  def cleanOutput(s: String): String = s.trim.split("\n", -1).map(_.stripTrailing()).mkString("\n")

  def checkOutput(expected: String, actual: String) = {
    if (cleanOutput(expected) != cleanOutput(actual)) {
      val name = currentTestCaseName.get()
      println(s"output for \"$name\" differs from expected. new output:")
      println(actual)
    }
    assertResult(cleanOutput(expected)) {
      cleanOutput(actual)
    }
  }

  // NOTE: if this test TIMES OUT, it is likely that the ToScala instances have
  // become recursive. make sure that all instances are provided and correctly scoped,
  // especially where multiple instances might be applicable.
  test("basil ir to scala default") {
    checkOutput(expected, program.toScala)
  }

  test("basil ir to scala with splitting") {
    import ir.dsl.ToScalaWithSplitting.given
    checkOutput(expectedWithSplitting, program.toScala)
  }

  test("procedures with no body should not be split") {
    import ir.dsl.ToScalaWithSplitting.given

    val emptyProgram = prog(proc("main"))

    val expected = """
{
  def program = prog(
    proc("main")
  )

  program
}
    """
    checkOutput(expected, emptyProgram.toScala)
  }

  test("blocks with single statement should not be split") {
    import ir.dsl.ToScalaWithSplitting.given

    val singleStatement = prog(proc("main", block("entry", ret)))

    val expected = """
{
  def `procedure:main` = proc("main",
    block("entry",
      ret
    )
  )

  def program = prog(
    `procedure:main`
  )

  program
}
    """
    checkOutput(expected, singleStatement.toScala)
  }

  test("toscala statements") {
    val expected =
      """MemoryStore(StackMemory("stack", 64, 8), BinaryExpr(BVADD, Register("R31", 64), BitVecLiteral(BigInt("15"), 64)), Extract(8, 0, Register("R0", 64)), Endian.LittleEndian, 8, Some("%0000034e"))"""
    val stmt = MemoryStore(
      StackMemory("stack", 64, 8),
      BinaryExpr(BVADD, Register("R31", 64), BitVecLiteral(BigInt("15"), 64)),
      Extract(8, 0, Register("R0", 64)),
      Endian.LittleEndian,
      8,
      Some("%0000034e")
    )

    checkOutput(expected, stmt.toScala)
  }

  test("toscala macro compilation") {
    // derive with exclusions
    assertCompiles("""
enum EAAA {
  case A
  case B
}
given ToScala[EAAA] = ToScala.deriveWithExclusions[EAAA, EAAA.A.type]((x: EAAA.A.type) => "custom" + x.toString)
      """)

    // exclusion type should be a subtype of base type
    assertTypeError("""
enum EAAA {
  case A
  case B
}
given ToScala[EAAA] = ToScala.deriveWithExclusions[EAAA, Any]((x: Any) => ???)
      """)

    // recursive
    assertCompiles("""
given ToScala[Int] = ToScala.Make(_.toString)

sealed trait Y derives ToScala

sealed trait X extends Y derives ToScala
case object X1 extends X
case class X2(x: Int, y: Int, rec: X) extends X
case class X2a(x: Int) extends X
case class X3() extends X
""")

    // missing instance for Double
    assertTypeError("""
given ToScala[Int] with
  extension (x: Int) def toScala = x.toString

sealed trait L derives ToScala
case object N extends L
case class C(x: Int, l: L, d: Double) extends L
      """)

    // as above but with Double present
    assertCompiles("""
given ToScala[Int] with
  extension (x: Int) def toScala = x.toString
given ToScala[Double] with
  extension (x: Double) def toScala = x.toString

sealed trait L derives ToScala
case object N extends L
case class C(x: Int, l: L, d: Double) extends L
      """)

    // type that appears in its own constructor
    assertTypeError("""
sealed trait T derives ToScala
case class A(a: A) extends T
      """)

    // large number of cases
    assertCompiles("""
sealed trait ASD derives ToScala
case class A() extends ASD
case class A1() extends ASD
case class A2() extends ASD
case class A3() extends ASD
case class A4() extends ASD
case class A5() extends ASD
case class A6() extends ASD
case class A7() extends ASD
case class A8() extends ASD
case class A9() extends ASD
case class A10() extends ASD
case class A11() extends ASD
case class A12() extends ASD
case class A13() extends ASD
case class A14() extends ASD
case class A15() extends ASD
case class A16() extends ASD
case class A17() extends ASD
case class A18() extends ASD
case class A19() extends ASD
case class A20() extends ASD
case class A21() extends ASD
case class A22() extends ASD
case class A23() extends ASD
case class A24() extends ASD
case class A25() extends ASD
case class A26() extends ASD
case class A27() extends ASD
case class A28() extends ASD
case class A29() extends ASD
case class A30() extends ASD
case class A31() extends ASD
case class A32() extends ASD
case class A33() extends ASD
case class A34() extends ASD
case class A35() extends ASD
case class A36() extends ASD
case class A37() extends ASD
case class A38() extends ASD
case class A39() extends ASD
""")

  }

  object TestData {

    enum EAAA {
      case A
      case B
    }
    given ToScala[EAAA] = ToScala.deriveWithExclusions[EAAA, EAAA.A.type]((x: EAAA.A.type) => s"custom$x")

    sealed trait L derives ToScala
    case class N() extends L
    case class C(t: L) extends L

    enum Color(val rgb: Int) derives ToScala {
      case Red extends Color(0xff0000)
      case Green extends Color(0x00ff00)
      case Blue extends Color(0x0000ff)
    }
  }

  test("toscala macro result") {
    import TestData.*
    assertResult("EAAA.B") { EAAA.B.toScala }
    assertResult("EAAA.customA") { EAAA.A.toScala }
    assertResult("N()") { N().toScala }
    assertResult("C(C(N()))") { C(C(N())).toScala }
    assertResult("Color.Red") { Color.Red.toScala }
  }

}
