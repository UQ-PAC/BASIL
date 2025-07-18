package util.intrusive_list

import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

@test_util.tags.UnitTest
class IntrusiveListTest extends AnyFunSuite with CaptureOutput {
  case class Elem(t: Float) extends IntrusiveListElement[Elem]

  test("basic") {
    val x = IntrusiveList[Elem]()
    val toInsert = Elem(10)
    val f: IntrusiveListElement[Elem] = x.append(toInsert)
    assert(x.size == 1)
    assert(f.last() == f)
    assert(f eq toInsert)
  }

  test("ListElem traversal") {
    val x = Elem(10)
    val p1 = x.insertBefore(Elem(9))
    assert(p1.hasNext)
    assert(x.hasPrev)
    assert(p1.getNext eq x)

    assert(x.getPrev eq p1)
    assert(x.getPrev == Elem(9))
    val p2 = p1.insertBefore(Elem(8))
    assert(p2.getNext eq p1)
    assert(p1.getPrev eq p2)
    val p3 = p2.insertBefore(Elem(7))

    assert(x.last() == x)
    assert(p3.first() == p3)

    assert(p3.getNext eq p2)
    assert(p2.getPrev eq p3)

    assert(x.first() eq p3)
    assert(p3.last() eq x)

    p2.insertAfter(Elem(8.5))
    assert(p3.last() eq x)

    p3.insertAfter(Elem(7.5))
    assert(x.first() eq p3)
  }

  test("ListElem remove") {
    val p0 = Elem(10)
    val p1 = p0.insertAfter(Elem(9))
    val p2 = p1.insertAfter(Elem(8))
    val p3 = p2.insertAfter(Elem(7))

    p2.remove()
    assert(p3.first() == p0)
    assert(p0.last() == p3)
  }

  test("ListElem remove2") {
    val p0 = Elem(10)
    val p1 = p0.insertAfter(Elem(9))
    val p2 = p1.insertAfter(Elem(8))
    val p3 = p2.insertAfter(Elem(7))

    p2.remove()
    assert(p3.first() == p0)
    assert(p0.last() == p3)
  }

  test("ListElem prepend") {
    val p0 = Elem(10)
    val p1 = Elem(9)

    val (x, y, z) = (Elem(1), Elem(2), Elem(3))

    val ps = List(x, y, z)
    val lst = IntrusiveList[Elem]()

    lst.append(p0)
    lst.append(p1)
    lst.prependAll(ps)

    assert(lst.toList == List(x, y, z, p0, p1))

  }

  // test("IntrusiveList insertRemove")
}
