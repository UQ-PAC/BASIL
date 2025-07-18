package util.intrusive_list
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

import scala.collection.mutable

@test_util.tags.UnitTest
class IntrusiveListPublicInterfaceTest extends AnyFunSuite with CaptureOutput {
  class Elem(val t: Int) extends IntrusiveListElement[Elem]

  def getSequentialList(elems: Int = 15): IntrusiveList[Elem] = {
    val x = IntrusiveList[Elem]()

    for (i <- 0 until elems) {
      x.append(Elem(i))
    }
    x
  }

  test("append size iter vals") {

    val x = IntrusiveList[Elem]()

    for (i <- 0 until 15) {
      assert(x.size == i)
      x.append(Elem(i))
      assert(x.size == i + 1)
    }

    var j = 0
    for (v <- x) {
      assert(v.t == j)
      j += 1
    }

    assert(x(0).t == 0)
    assert(x(1).t == 1)
    assert(x(5).t == 5)

  }

  test("Append") {
    val x = IntrusiveList[Elem]()
    val toInsert = Elem(10)
    val f = x.append(toInsert)
    assert(x.size == 1)
    assert(f eq toInsert)

    x.append(Elem(11))
    x.append(Elem(12))
    x.append(Elem(13))
    x.append(Elem(14))

//    x.foreach(println(_))

    val y = x.head
    assert(y.t == 10)
    assert(x.back.t == 14)
  }

  test("Clear") {
    val l = getSequentialList(15)
    assert(l.size == 15)
    l.clear()
    assert(l.size == 0)

    for (e <- l) {
      assert(false)
    }
  }

  test("Replace diff") {
    val l = getSequentialList(5)
    val old = Array.from(l)
    assert(l(2).t == 2)
    assert(l.size == 5)
    l.replace(l(2), Elem(100))
    assert(l.size == 5)
    assert(l(2).t == 100)

    for (i <- 0 until 5) {
      if (i != 2) {
        assert(l(i) == old(i))
      }
    }
  }

  test("Replace same") {
    val l = getSequentialList(5)
    val old = Array.from(l)

    assert(l(2).t == 2)
    l.replace(l(2), l(2))
    assert(l.size == 5)

    for (i <- 0 until 5) {
      assert(l(i) == old(i))
    }
  }

  test("Swap list elems remove/insertAfter") {
    val l = getSequentialList(5)
    val old = Array.from(l)

    assert(l(2).t == 2)
    val removed = l.remove(l(2))
    l.insertAfter(l(2), removed)
    assert(l.size == 5)
    assert(l(2).t == 3)
    assert(l(3).t == 2)

    for (i <- 0 until 5) {
      if (i != 2 && i != 3) {
        assert(l(i) == old(i))
      }
    }
  }

  test("Prepend") {
    val l = IntrusiveList[Elem]()
    for (i <- 0 until 5) {
      l.prepend(Elem(i))
    }
    assert(l.size == 5)

    for (i <- 0 until 5) {
      assert(l(i).t == 4 - i)
    }
  }

  test("prep/append") {
    val l = IntrusiveList[Elem]()
    for (i <- 0 until 5) {
      l.prepend(Elem(i))
      l.append(Elem(i))
    }
    assert(l.size == 10)

    for (i <- 0 until 5) {
      assert(l(i).t == 4 - i)
    }
    for (i <- 0 until 5) {
      assert(l(5 + i).t == i)
    }
  }

  test("iteratorFrom") {
    val l = getSequentialList(10)
    val it = l.iteratorFrom(l(2))
    assert(it.hasNext)
    assert(it.next().t == 2)
    assert(l.iteratorFrom(l(2)).length == 8)
    assert(it.length == 7)
  }

  test("splitat") {
    val l = IntrusiveList[Elem]()

    l.addOne(Elem(1))

    val e = Elem(15)
    val toAdd = List(e, Elem(16), Elem(17))

    l.addAll(toAdd)
    assert(l.size == 4)
    assert(l.contains(e))

    val l2 = l.splitOn(e)
    assert(l2.size == 2)
    assert(l.size == 2)
    assert(l.exists(_.t == 1))
    assert(l.exists(_.t == 15))
    assert(l2.exists(_.t == 16))
    assert(l2.exists(_.t == 17))
  }

  test("addAll") {
    val l = getSequentialList(3)
    val toAdd = List(Elem(3), Elem(4), Elem(5))
    l.addAll(toAdd)

    for (i <- 0 until 6) {
      assert(l(i).t == i)
    }
  }

  test("construct") {
    val l3 = mutable.ArrayBuffer(Elem(1), Elem(2), Elem(3))

    val l4 = IntrusiveList().addAll(l3)

    assert(l3 ne l4)
    assert(l3.forall(x => l4.contains(x)))

    assert(l4.size == 3)

  }

  test("insertAllAfter") {
    val x = IntrusiveList[Elem]()

    x.append(Elem(9))
    val first = Elem(10)
    val f = x.append(first)
    x.append(Elem(13))
    // 9 10 13
    assert(x.toList.map(_.t) == List(9, 10, 13))

    val n = Elem(225)
    val toInsert = List(Elem(11), Elem(12), n)

    val r = x.insertAllAfter(Some(first), toInsert)
    assert(r.get eq n)
    assert(x.toList.map(_.t) == List(9, 10, 11, 12, 225, 13))

    val l = Range(1, 4).map(x => Elem(x))
    val rr = x.insertAllAfter(None, l)
    assert(x.toList.map(_.t) == List(1, 2, 3, 9, 10, 11, 12, 225, 13))

  }

  test("insertAllBefore") {
    val x = IntrusiveList[Elem]()

    x.append(Elem(9))
    val first = Elem(10)
    val f = x.append(first)
    x.append(Elem(13))
    // 9 10 13
    assert(x.toList.map(_.t) == List(9, 10, 13))

    val n = Elem(11)
    val toInsert = List(n, Elem(12), Elem(255))

    val r = x.insertAllBefore(Some(first), toInsert)
    assert(r.get eq n)
    assert(x.toList.map(_.t) == List(9, 11, 12, 255, 10, 13))

    val l = Range(1, 4).map(x => Elem(x))
    val rr = x.insertAllBefore(None, l)
    assert(x.toList.map(_.t) == List(9, 11, 12, 255, 10, 13, 1, 2, 3))
  }

}
