import ir._
import util.functional._

import ir.eval._
import ir.dsl._
import munit.FunSuite
import translating.BAPToIR
import util.{LogLevel, Logger}
import util.IRLoading.{loadBAP, loadReadELF}
import util.ILLoadingConfig

def add: State[Int, Unit, Unit] = State(s => (s + 1, Right(())))

import org.junit.experimental.categories.Category
import test_util.UnitTest

@Category(Array(classOf[UnitTest]))
class StateMonadTest extends FunSuite {

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
