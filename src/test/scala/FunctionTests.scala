import org.scalatest.funsuite.AnyFunSuite


class FunctionTests extends AnyFunSuite {
  test("function") {
    val result = TestUtils.processBoogieFile("function/function")
    assert(result.failures == List(237))
  }

  test("function_ensures") {
    val result = TestUtils.processBoogieFile("function/function_ensures", Some("function/function.elf"))
    assert(result.numFailure == 0)
    assert(result.numSuccess == 2)
  }
}
