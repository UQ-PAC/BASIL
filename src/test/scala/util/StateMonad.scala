import ir._
import util.functional._

import ir.eval._
import ir.dsl._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import translating.BAPToIR
import util.{LogLevel, Logger}
import util.IRLoading.{loadBAP, loadReadELF}
import util.ILLoadingConfig

def add: State[Int, Unit, Unit] = State(s => (s + 1, Right(())))

@test_util.tags.UnitTest
class StateMonadTest extends AnyFunSuite with test_util.CaptureOutput {

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
