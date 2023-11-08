package ir
import org.scalatest.funsuite.AnyFunSuite
import intrusiveList.{IntrusiveList, IntrusiveListElement}

case class Elem(val t: Float) extends IntrusiveListElement

class IntrusiveListPublicInterfaceTest extends AnyFunSuite {

  def getSequentialList(elems: Int = 15) : IntrusiveList[Elem] = {
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

    x.foreach(println(_))

    val y = x.head()
    assert(y.t == 10)
    assert(x.back().t == 14)
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


}
