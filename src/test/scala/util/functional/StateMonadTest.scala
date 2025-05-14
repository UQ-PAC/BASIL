package util.functional

import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

def add: State[Int, Unit, Unit] = State(s => (s + 1, Right(())))

@test_util.tags.UnitTest
class StateMonadTest extends AnyFunSuite with CaptureOutput {

  test("forcompre") {
    val s = for {
      _ <- add
      _ <- add
      _ <- add
    } yield ()

    val res = State.execute(0, s)
    assert(res == 3)
  }
}
