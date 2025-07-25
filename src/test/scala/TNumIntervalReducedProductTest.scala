import analysis.Interval.ConcreteInterval
import analysis.{TNum, TNumIntervalReducedProduct}
import ir.BitVecLiteral
import org.scalatest.funsuite.AnyFunSuiteLike

@test_util.tags.UnitTest
class TNumIntervalReducedProductTest extends AnyFunSuiteLike {

  val domain = TNumIntervalReducedProduct()
  val interval = ConcreteInterval(6, 10, 4)
  val tnum = TNum(BitVecLiteral(0, 4), BitVecLiteral(9, 4))

  test("Refine lower bound - simple example") {
    val expected = 8
    assert(expected == domain.refineLowerBound(6, tnum))
  }

  test("Refine lower bound - 0") {
    val expected = 0
    assert(expected == domain.refineLowerBound(0, tnum))
  }

  test("Refine upper bound - simple example") {
    val expected = 9
    assert(expected == domain.refineUpperBound(10, tnum))
  }

  test("Refine upper bound - 0") {
    val expected = 0
    assert(expected == domain.refineUpperBound(0, tnum))
  }

  test("Refine tnum - simple example") {
    val expected = TNum(BitVecLiteral(8, 4), BitVecLiteral(1, 4))
    assert(expected == domain.refineTnum(8, 9, tnum))
  }

  test("Refine tnum - 0") {
    val expected = TNum(BitVecLiteral(0, 4), BitVecLiteral(0, 4))
    assert(expected == domain.refineTnum(0, 0, tnum))
  }

}
