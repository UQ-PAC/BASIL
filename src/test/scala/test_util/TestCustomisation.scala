package test_util

import org.scalatest.{TestSuite, Informing, Retries, Failed, Exceptional, Pending, Canceled, Succeeded}

/**
 * A mixin for TestSuite (including AnyFunSuite) which allows
 * for customisation of expected test outcomes, based on the string
 * name of a test case. This is most useful in conjunction with
 * dynamically-generated test cases. For manually-written tests, it is
 * usually easier and clearer to make the modifications directly in
 * the test case.
 *
 * Users should implement the customiseTestsByName method (see doc
 * comment below).
 *
 */
trait TestCustomisation extends TestSuite with Retries {
  this: Informing =>

  enum Mode(val reason: Option[String]):
    /** Test should execute normally and succeed (no customisation). */
    case Normal extends Mode(None)

    /** Test should be retried once if it fails. */
    case Retry(s: String) extends Mode(Some(s))

    /** Test is expected to fail due to temporary issues which should be fixed soon. */
    case TempFailure(s: String) extends Mode(Some(s))

    /** Test is expected to fail due to not-yet-implemented features. */
    case NotImplemented(s: String) extends Mode(Some(s))

    /** Test is not run. This should be used very sparingly. */
    case Disabled(s: String) extends Mode(Some(s))

    /**
     * Simple chaining of customisation modes. The current
     * mode is returned if it is abnormal, otherwise the second (given)
     * mode is returned.
     */
    def orElse(other: => Mode): Mode = this match {
      case Normal => other
      case _ => this
    }

  /**
   * This method is called with a test case name and it should return the behaviour
   * of that test case (use Mode.Normal for unremarkable tests).
   */
  def customiseTestsByName(name: String): Mode

  override def withFixture(test: NoArgTest) = {

    val mode = customiseTestsByName(test.name)

    def invokeTest() = super.withFixture(test)

    if (mode != Mode.Normal) {
      info(s"NOTE: Test case is customised with: \"$mode\"")
    }

    mode match {
      case Mode.Normal => invokeTest()
      case Mode.Retry(s) => withRetry { invokeTest() }
      case Mode.TempFailure(_) | Mode.NotImplemented(_) => {
        val s = mode.reason.get
        val res = invokeTest()
        res match {
          case Succeeded => fail(s"Expected failure, but no exception/assertion was thrown")
          case Exceptional(_) | Failed(_) => {
            info("Current outcome: " + res)
            Pending
          }
          case Canceled(_) | Pending => res
        }
      }
      case Mode.Disabled(s) => cancel(s"Test has been explicitly disabled")
    }
  }
}
