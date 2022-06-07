import org.scalatest.funsuite.AnyFunSuite
import utils.TestUtils


class FunctionTests extends AnyFunSuite {
  test("function") {
    // This test passing seems to indicate that it can infer an ensures
    // It would be good to check that this is the case (and that its not just a bug)
    val result = TestUtils.processBoogieFile("function/function")
    assert(result.numFailure == 1)
    assert(result.numSuccess == 0)
  }

  test("function_ensures") {
    val result = TestUtils.processBoogieFile("function/function_ensures", Some("function/function.elf"))
    assert(result.numFailure == 0)
    assert(result.numSuccess == 2)
  }
}
