package ir

import ir.dsl.*
import ir.dsl.given
import org.scalactic.source.Position
import org.scalatest.concurrent.{ThreadSignaler, TimeLimitedTests}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterEachTestData, TestData}
import test_util.CaptureOutput
import util.twine.Twine

@test_util.tags.UnitTest
class ToScalaTest extends AnyFunSuite with CaptureOutput with TimeLimitedTests with BeforeAndAfterEachTestData {

  override def timeLimit = Span(2, Seconds)
  override val defaultTestSignaler = ThreadSignaler

  private val currentTestCaseName = new ThreadLocal[String]

  override def beforeEach(testData: TestData): Unit = {
    currentTestCaseName.set(testData.name)
    super.beforeEach(testData)
  }

  // XXX: work around https://github.com/scala/scala3/issues/23578 / https://github.com/scala/scala3/issues/22968
  // until 3.3.7
  inline def inlineAssertCompiles(s: String) =
    assertCompiles(s)
  inline def inlineAssertTypeError(s: String) =
    assertTypeError(s)

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
  proc("main", returnBlockLabel = Some("returnBlock"))(
    block("first_call",
      LocalAssign(GlobalVar("R0", BitVecType(64)), BitVecLiteral(BigInt("1"), 64), None),
      LocalAssign(GlobalVar("R1", BitVecType(64)), BitVecLiteral(BigInt("1"), 64), None),
      directCall("callee1"),
      goto("second_call")
    ),
    block("second_call",
      directCall("callee2"),
      goto("returnBlock")
    ),
    block("returnBlock", ret)
  ),
  proc("callee1", returnBlockLabel = None)(
    block("returnBlock", ret)
  ),
  proc("callee2", returnBlockLabel = None)(
    block("returnBlock", ret)
  ),
  proc("empty procedure", returnBlockLabel = None)()
)
  """

  val expectedWithSplitting = """
{
  def `block:main.first_call` = block("first_call",
    LocalAssign(GlobalVar("R0", BitVecType(64)), BitVecLiteral(BigInt("1"), 64), None),
    LocalAssign(GlobalVar("R1", BitVecType(64)), BitVecLiteral(BigInt("1"), 64), None),
    directCall("callee1"),
    goto("second_call")
  )

  def `procedure:main` = proc("main", returnBlockLabel = Some("returnBlock"))(
    `block:main.first_call`,
    block("second_call",
      directCall("callee2"),
      goto("returnBlock")
    ),
    block("returnBlock", ret)
  )

  def `procedure:callee1` = proc("callee1", returnBlockLabel = None)(
    block("returnBlock", ret)
  )

  def `procedure:callee2` = proc("callee2", returnBlockLabel = None)(
    block("returnBlock", ret)
  )

  def program = prog(
    `procedure:main`,
    `procedure:callee1`,
    `procedure:callee2`,
    proc("empty procedure", returnBlockLabel = None)()
  )

  program
}
  """

  /**
   * Normalise output by removing beginning and ending whitespace, and removing
   * trailing whitespace from each line.
   */
  def cleanOutput(s: String): String = s.trim.split("\n", -1).map(_.stripTrailing()).mkString("\n")

  // `inline` allows the assertResult to report line numbers of the call site
  inline def checkOutput(expected: String, actual: String) = {
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
  def program = prog(proc("main", returnBlockLabel = None)())

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
  def `procedure:main` = proc("main", returnBlockLabel = None)(
    block("entry", ret)
  )

  def program = prog(`procedure:main`)

  program
}
    """
    checkOutput(expected, singleStatement.toScala)
  }

  test("toscala statements") {
    val expected =
      """MemoryStore(StackMemory("stack", 64, 8), BinaryExpr(BVADD, GlobalVar("R31", BitVecType(64)), BitVecLiteral(BigInt("15"), 64)), Extract(8, 0, GlobalVar("R0", BitVecType(64))), Endian.LittleEndian, 8, Some("%0000034e"))"""
    val stmt = MemoryStore(
      StackMemory("stack", 64, 8),
      BinaryExpr(BVADD, GlobalVar("R31", BitVecType(64)), BitVecLiteral(BigInt("15"), 64)),
      Extract(8, 0, GlobalVar("R0", BitVecType(64))),
      Endian.LittleEndian,
      8,
      Some("%0000034e")
    )

    checkOutput(expected, stmt.toScala)
  }

  test("proc params") {
    val p = prog(
      proc(
        "printf",
        Seq("R9_in" -> BitVecType(64), "R0_in" -> BitVecType(64)),
        Seq("R9_out" -> BitVecType(64), "R0_out" -> BitVecType(64))
      )()
    )

    val expected = """
prog(
  proc("printf",
    in = Seq(
      "R0_in" -> BitVecType(64),
      "R9_in" -> BitVecType(64)
    ),
    out = Seq(
      "R0_out" -> BitVecType(64),
      "R9_out" -> BitVecType(64)
    ),
    returnBlockLabel = None
  )()
)
    """

    checkOutput(expected, p.toScala)
  }

  test("return params") {
    import scala.language.implicitConversions

    val p = prog(
      proc(
        "proc",
        Seq(),
        Seq("R0_out" -> BitVecType(64), "R1_out" -> BitVecType(64), "R31_out" -> BitVecType(64)),
        block(
          "get_two_1876_basil_return",
          directCall(Seq("out" -> R0), "printf", Seq("in" -> LocalVar("R0", BitVecType(64), 0))),
          ret(
            "R0_out" -> LocalVar("R0", BitVecType(64), 0),
            "R1_out" -> LocalVar("R1", BitVecType(64), 0),
            "R31_out" -> LocalVar("R31", BitVecType(64), 0)
          )
        )
      ),
      proc("printf", Seq("in" -> BitVecType(64)), Seq("out" -> BitVecType(64)))()
    )

    val expected = """
prog(
  proc("proc",
    in = Seq(),
    out = Seq(
      "R0_out" -> BitVecType(64),
      "R1_out" -> BitVecType(64),
      "R31_out" -> BitVecType(64)
    ),
    returnBlockLabel = None
  )(
    block("get_two_1876_basil_return",
      directCall(
        Seq("out" -> GlobalVar("R0", BitVecType(64))),
        "printf",
        Seq("in" -> LocalVar("R0", BitVecType(64), 0))
      ),
      ret(
        "R0_out" -> LocalVar("R0", BitVecType(64), 0),
        "R1_out" -> LocalVar("R1", BitVecType(64), 0),
        "R31_out" -> LocalVar("R31", BitVecType(64), 0)
      )
    )
  ),
  proc("printf",
    in = Seq("in" -> BitVecType(64)),
    out = Seq("out" -> BitVecType(64)),
    returnBlockLabel = None
  )()
)
    """

    checkOutput(expected, p.toScala)
  }

  test("toscala macro compilation") {
    // derive with exclusions
    inlineAssertCompiles("""
enum EAAA {
  case A
  case B
}
given ToScala[EAAA] = ToScala.deriveWithExclusions[EAAA, EAAA.A.type](ToScala.Make(x => Twine("custom", x.toString)))
      """)

    // exclusion type should be a subtype of base type
    inlineAssertTypeError("""
enum EAAA {
  case A
  case B
}
given ToScala[EAAA] = ToScala.deriveWithExclusions[EAAA, Any](???)
      """)

    // recursive
    inlineAssertCompiles("""
given ToScala[Int] = ToScala.MakeString(_.toString)

sealed trait Y derives ToScala

sealed trait X extends Y derives ToScala
case object X1 extends X
case class X2(x: Int, y: Int, rec: X) extends X
case class X2a(x: Int) extends X
case class X3() extends X
""")

    // missing instance for Double
    inlineAssertTypeError("""
given ToScala[Int] = ToScala.MakeString(_.toString)

sealed trait L derives ToScala
case object N extends L
case class C(x: Int, l: L, d: Double) extends L
      """)

    // as above but with Double present
    inlineAssertCompiles("""
given ToScala[Int] = ToScala.MakeString(_.toString)
given ToScala[Double] = ToScala.MakeString(_.toString)

sealed trait L derives ToScala
case object N extends L
case class C(x: Int, l: L, d: Double) extends L
      """)

    // type that appears in its own constructor
    inlineAssertTypeError("""
sealed trait T derives ToScala
case class A(a: A) extends T
      """)

    // large number of cases
    inlineAssertCompiles("""
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
    given ToScala[EAAA] =
      ToScala.deriveWithExclusions[EAAA, EAAA.A.type](ToScala.Make(x => Twine("custom", x.toString)))

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

  test("prog toscala should always put main first") {

    val p = prog(proc("main"), proc("notmain"))

    // move the main procedure after notmain in the procedures array
    val main = p.procedures(0)
    p.procedures(0) = p.procedures(1)
    p.procedures(1) = main

    val s = p.toScala
    assert(s.contains("\"main\""))
    assert(s.contains("\"notmain\""))
    assert(
      s.indexOfSlice("\"main\"") < s.indexOfSlice("\"notmain\""),
      "main procedure should be the first argument to a prog() call"
    )
  }

  test("program toscala with initial memory") {

    val program =
      prog(
        Seq(
          MemorySection(
            ".interp",
            BigInt("4194872"),
            110,
            Seq(
              0x2f, 0x6e, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2f, 0x61, 0x31, 0x33, 0x61, 0x31, 0x71,
              0x69, 0x32, 0x6b, 0x6e, 0x71, 0x61, 0x30, 0x73, 0x64, 0x31, 0x67, 0x76, 0x6d, 0x35, 0x78, 0x33, 0x34,
              0x70, 0x72, 0x35, 0x33, 0x77, 0x67, 0x30, 0x69, 0x6c, 0x2d, 0x67, 0x6c, 0x69, 0x62, 0x63, 0x2d, 0x61,
              0x61, 0x72, 0x63, 0x68, 0x36, 0x34, 0x2d, 0x75, 0x6e, 0x6b, 0x6e, 0x6f, 0x77, 0x6e, 0x2d, 0x6c, 0x69,
              0x6e, 0x75, 0x78, 0x2d, 0x67, 0x6e, 0x75, 0x2d, 0x32, 0x2e, 0x33, 0x38, 0x2d, 0x34, 0x34, 0x2f, 0x6c,
              0x69, 0x62, 0x2f, 0x6c, 0x64, 0x2d, 0x6c, 0x69, 0x6e, 0x75, 0x78, 0x2d, 0x61, 0x61, 0x72, 0x63, 0x68,
              0x36, 0x34, 0x2e, 0x73, 0x6f, 0x2e, 0x31, 0x00
            ).map(BitVecLiteral(_, 8)).toSeq,
            readOnly = false,
            region = None
          )
        ),
        proc("knownBitsExample_4196164")
      )

    val expected = """
prog(
  Seq(
    MemorySection(
      ".interp", BigInt("4194872"), 110,
      Seq(
        0x2f,0x6e,0x69,0x78,0x2f,0x73,0x74,0x6f,0x72,0x65,0x2f,0x61,0x31,0x33,0x61,0x31,0x71,0x69,0x32,0x6b,0x6e,0x71,0x61,0x30,0x73,0x64,0x31,0x67,0x76,0x6d,0x35,0x78,
        0x33,0x34,0x70,0x72,0x35,0x33,0x77,0x67,0x30,0x69,0x6c,0x2d,0x67,0x6c,0x69,0x62,0x63,0x2d,0x61,0x61,0x72,0x63,0x68,0x36,0x34,0x2d,0x75,0x6e,0x6b,0x6e,0x6f,0x77,
        0x6e,0x2d,0x6c,0x69,0x6e,0x75,0x78,0x2d,0x67,0x6e,0x75,0x2d,0x32,0x2e,0x33,0x38,0x2d,0x34,0x34,0x2f,0x6c,0x69,0x62,0x2f,0x6c,0x64,0x2d,0x6c,0x69,0x6e,0x75,0x78,
        0x2d,0x61,0x61,0x72,0x63,0x68,0x36,0x34,0x2e,0x73,0x6f,0x2e,0x31,0x00
      ).map(BitVecLiteral(_, 8)).toSeq,
      readOnly = false,
      region = None
    )
  ),
  proc("knownBitsExample_4196164", returnBlockLabel = None)()
)
"""

    checkOutput(expected, new ToScalaWithInitialMemory {}.programToScala(program).mkString)
  }

}
