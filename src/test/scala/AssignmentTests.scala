import org.scalatest.funsuite.AnyFunSuite


/**
  * TODO: write tests
  *   - tests for typing
  *   - tests for correctness
  *   - tests for bv to mem conversions
  *   - tests for bv (e.g. for extract 0[0:8] == 0bv8)
  *   - test axioms (for the moment just for bool <-> bv1)
  */

class AssignmentTests extends AnyFunSuite {
  test("basicassign") {
    val result = TestUtils.processBoogieFile("basicassign/basicassign")
    assert(result.failures == List(261))
  }

  test("basicassign1") {
    val result = TestUtils.processBoogieFile("basicassign/basicassign1")
    assert(result.failures == List(234))
  }

  test("basicassign2") {
    val result = TestUtils.processBoogieFile("basicassign/basicassign2")
    assert(result.failures == List(206))
  }

  test("basicassign3") {
    val result = TestUtils.processBoogieFile("basicassign/basicassign3")
    assert(result.failures == List(234))
  }

  test("basicassign_gamma0") {
    val result = TestUtils.processBoogieFile("basicassign/basicassign_gamma0")
    assert(result.numFailure == 0)
    assert(result.numSuccess == 1)
  }
}
