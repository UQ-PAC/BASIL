import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

import ir.shrinking.*

@test_util.tags.UnitTest
class ShrinkerTest extends AnyFunSuite with CaptureOutput {

  extension [T](x: Iterable[T]) {
    def it = x.iterator
  }

  test("boop") {
    println("x")
    println(roundRobin(
      List(
        List(1, 2, 3, 4).it,
        List(11, 12).it,
        List(21).it,
        List(31, 32, 33, 34).it,
      ).it
    ).toList)
  }

  test("shrink") {

    val s = ShrinkBasil[String](4, List(
      Shrinker.one[String](_.toLowerCase()).ifChanged(x => x),
      Shrinker.one[String](_.drop(1)).ifChanged(_.length)
    ))

    println(s.shrink(_ => true)(List("ASDJFIA"), 0))

  }

}
