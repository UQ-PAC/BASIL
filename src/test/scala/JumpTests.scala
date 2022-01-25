import org.scalatest.funsuite.AnyFunSuite

class JumpTests extends AnyFunSuite {
  ignore("iflocal") {
    val result = TestUtils.processBoogieFile("if/iflocal")
    assert(result.numFailure == 0)
    assert(result.numSuccess == 1)
  }

  test("ifglobal") {
    val result = TestUtils.processBoogieFile("if/ifglobal")
    assert(result.numFailure == 1)
    assert(result.numSuccess == 0)
  }

  test("ifglobal_safe") {
    val result = TestUtils.processBoogieFile("if/ifglobal_safe", Some("if/ifglobal.elf"))
    assert(result.numFailure == 0)
    assert(result.numSuccess == 1)
  }
}
