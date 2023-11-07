package ir
import org.scalatest.funsuite.AnyFunSuite
import intrusiveList.{IntrusiveList, IntrusiveListElement}

case class Elem(val t: Float) extends IntrusiveListElement

class IntrusiveListPublicInterfaceTest extends AnyFunSuite {

  test("general stuff") {
    val x = IntrusiveList[Elem]()
    val toInsert = Elem(10)
    val f = x.append(toInsert)
    assert(x.size == 1)
    assert(f eq toInsert)

    x.append(Elem(11))
    x.append(Elem(12))
    x.append(Elem(13))
    x.append(Elem(14))

    var y = x.head()
    assert(y.t == 10)
    assert(x.back().t == 14)


  }

}
