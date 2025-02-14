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
    proc("main",
      block("first_call",
        LocalAssign(R0, bv64(1), None),
        LocalAssign(R1, bv64(1), None),
        directCall("callee1"),
        goto("second_call")
      ),
      block("second_call",
        directCall("callee2"),
        goto("returnBlock")
      ),
      block("returnBlock",
        ret
      ),
    ),
    proc("callee1",
      block("returnBlock",
        ret
      ),
    ),
    proc("callee2",
      block("returnBlock",
        ret
      ),
    ),
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




}
