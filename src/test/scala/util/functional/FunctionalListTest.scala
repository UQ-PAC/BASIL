package util.functional

import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

@test_util.tags.UnitTest
class FunctionalListTest extends AnyFunSuite with CaptureOutput {

  test("sequence as cartesian product") {
    assertResult(Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))) {
      sequence(List(Vector(1, 2), Vector(3, 4)))
    }
    assertResult(Vector(List())) {
      sequence(List[Vector[?]]())
    }
  }

  test("sequence on list of options") {
    assertResult(None) {
      sequence(List(Some(1), Some(2), None))
    }
    assertResult(Some(List())) {
      sequence(List[Option[?]]())
    }
    assertResult(Some(List(1, 2))) {
      sequence(List(Some(1), Some(2)))
    }
  }

  test("sequence on list of eithers") {
    assertResult(Left(List("b", "a"))) {
      sequence(List(Right(1), Left("b"), Right(2), Left("a")))
    }
    assertResult(Some(List(1, 2))) {
      sequence(List(Some(1), Some(2)))
    }
    assertResult(Right(List())) {
      sequence(List[Either[?, ?]]())
    }
  }
}
