package test_util

import org.scalatest.{Informing, TestSuite}
import java.io.PrintStream

/**
 * A mixin which redirects stdout/stderr of test cases to the scalatest
 * info() method. This helps organise the test output, especially when
 * running in parallel. With this change, stdout/stderr get recorded
 * and printed directly underneath the test case name.
 */
trait CaptureOutput extends TestSuite {
  this: Informing =>

  override def withFixture(test: NoArgTest) = {

    val functionOutputStream = PrintStream(FunctionOutputStream(x => info(x.stripLineEnd)))
    val newSystemOut = ThreadOutputStream(functionOutputStream)
    val newSystemErr = ThreadOutputStream(functionOutputStream)

    Console.withOut(newSystemOut) {
      Console.withErr(newSystemErr) {
        super.withFixture(test)
      }
    }
  }
}
