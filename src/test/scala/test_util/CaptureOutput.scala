package test_util

import org.scalatest.{Informing, TestSuite}
import java.io.PrintStream;

trait CaptureOutput extends TestSuite {
  this: Informing =>

  private val newSystemOut = ThreadOutputStream(System.out)
  private val newSystemErr = ThreadOutputStream(System.err)

  override def withFixture(test: NoArgTest) = {
    Console.withOut(newSystemOut) {
      Console.withErr(newSystemErr) {
        val functionOutputStream = PrintStream(FunctionOutputStream(x => info(x.stripLineEnd)))
        newSystemOut.setThreadStream(functionOutputStream)
        newSystemErr.setThreadStream(functionOutputStream)
        super.withFixture(test)
      }
    }
  }
}

