package intrusiveList
import scala.collection.mutable

// TODO: implement IterableOps
//   So need iterablefactory https://docs.scala-lang.org/overviews/core/custom-collections.html

final class IntrusiveList[T <: IntrusiveListElement] private (var numElems: Int, var firstElem: Option[T],
                                                              var lastElem: Option[T])
  extends mutable.Iterable[T], mutable.Growable[T]:
  var onInsert: T => Unit = x => ()
  var onRemove: T => Unit  = x => ()

  // invariant:
  //    numElems == first.length()

  //    if size == 0 then first == last == None
  //    else if size == 1 then first.get == last.get
  //    else first = last.get.first() && last == first.get.last()

  // Growable
  override def knownSize: Int = numElems

  override def addOne(elem: T): IntrusiveList.this.type = {
    append(elem)
    this
  }

  override def clear(): Unit = {
    while (size > 0) {
      this.remove(lastElem.get)
    }
  }

  override def addAll(xs: IterableOnce[T]): IntrusiveList.this.type = {
    xs.iterator.foreach(append)
    this
  }
  // end Growable

  def this() = this(0, None, None)


  // Iterable

  /*
   * O(n); get nth element
   */
  def apply(i: Int): T = {
    // TODO: cache?
    assert(i < size)
    var elem = firstElem.get
    for (c <- 0 until i)  {
      elem = elem.getNext.asInstanceOf[T]
    }
    elem
  }

  class IntrusiveListIterator(var elem: Option[IntrusiveListElement]) extends Iterator[T] {
    override def hasNext: Boolean = elem.isDefined
    override def next: T = {
      val t = elem.get.asInstanceOf[T]
      elem = t.next
      t
    }
  }

  def iterator: Iterator[T] = IntrusiveListIterator(firstElem)

  // end Iterable


  def iteratorFrom(elem: IntrusiveListElement): Iterator[T] = {
    assert(elem.first() == firstElem.get)
    IntrusiveListIterator(Some(elem))
  }

  // Implementation

  override def size: Int = numElems

  override def head(): T = firstElem.get

  override def headOption(): Option[T] = firstElem


  def begin(): T = firstElem.get

  private def containsRef(elem: T): Boolean = {
    if (size == 0) {
      false
    } else {
      firstElem.get.containsRef(elem)
    }
  }

  def contains(elem: T): Boolean = {
    if (size == 0) {
      false
    } else {
      firstElem.get.contains(elem)
    }
  }

  def back(): T = lastElem.get

  def prepend(newElem: T): T = {
    assert(newElem.unitary)
    assert(!containsRef(newElem))
    onInsert(newElem)
    if (size > 0) {
      insertBefore(firstElem.get, newElem)
    } else {
      firstElem = Some(newElem)
      lastElem = Some(newElem)
      numElems = 1
    }
    newElem
  }

  def append(newElem : T): T = {
    assert(newElem.unitary)
    assert(!containsRef(newElem))
    onInsert(newElem)
    if (size > 0) {
      insertAfter(lastElem.get, newElem)
    } else {
      firstElem = Some(newElem)
      lastElem = Some(newElem)
      numElems = 1
    }
    newElem
  }

  def replace(elem: T, withElem: T): T = {
    assert(containsRef(elem))
    if (elem ne withElem) {
      assert(withElem.unitary)
      assert(!containsRef(withElem))
      val newElem: T = insertAfter(elem, withElem)
      val removed = remove(elem)
      newElem
    } else {
      withElem
    }
  }

  def remove(intrusiveListElement: T): T = {
    assert(size >= 0)
    assert(containsRef(intrusiveListElement))
    numElems -= 1
    if (intrusiveListElement == lastElem.get) {
      lastElem = intrusiveListElement.prev.asInstanceOf[Option[T]]
    }
    if (intrusiveListElement == firstElem.get) {
      firstElem = intrusiveListElement.next.asInstanceOf[Option[T]]
    }
    onRemove(intrusiveListElement)
    intrusiveListElement.remove().asInstanceOf[T]
  }


  def insertAfter(intrusiveListElement: T, newElem: T): T = {
    assert(size >= 1)
    assert(containsRef(intrusiveListElement))
    assert(!containsRef(newElem))
    assert(newElem.unitary)
    numElems += 1
    if (intrusiveListElement == lastElem.get) {
      lastElem = Some(newElem)
    }

    onInsert(newElem)
    intrusiveListElement.insertAfter(newElem).asInstanceOf[T]
  }

  def insertBefore(intrusiveListElement: T, newElem: T): T = {
    assert(size >= 1)
    assert(containsRef(intrusiveListElement))
    assert(!containsRef(newElem))
    assert(newElem.unitary)
    numElems += 1
    if (intrusiveListElement == firstElem.get) {
      firstElem = Some(newElem)
    }
    onInsert(newElem)
    intrusiveListElement.insertBefore(newElem).asInstanceOf[T]
  }

  def getNext(elem: T): T = {
    elem.getNext.asInstanceOf[T]
  }

  def hasNext(elem: T): Boolean = {
    elem.hasNext
  }

  def hasPrev(elem: T): Boolean = {
    elem.hasPrev
  }

  def getPrev(elem: T): T = {
    elem.getPrev.asInstanceOf[T]
  }

object IntrusiveList {
  def from[T <: IntrusiveListElement](it: IterableOnce[T]): IntrusiveList[T] = {
    val l = new IntrusiveList[T]()
    l.addAll(it)
    l
  }
  def empty[T <: IntrusiveListElement] : IntrusiveList[T] =  new IntrusiveList[T]()

}

trait IntrusiveListElement:
  private[intrusiveList] var next: Option[IntrusiveListElement] = None
  private[intrusiveList] var prev: Option[IntrusiveListElement] = None
  private[intrusiveList] final def insertBefore(elem: IntrusiveListElement): IntrusiveListElement = {
    elem.prev = prev
    if (prev.isDefined) {
      prev.get.next = Some(elem)
    }
    prev = Some(elem)
    elem.next = Some(this)
    elem
  }

  private[intrusiveList] final def unitary: Boolean = next.isEmpty && prev.isEmpty


  private[intrusiveList] final def insertAfter(elem: IntrusiveListElement): IntrusiveListElement = {
    if (next.isDefined) {
      next.get.prev = Some(elem)
    }
    elem.next = next
    next = Some(elem)
    elem.prev = Some(this)
    elem
  }

  private[intrusiveList] final def replace(elem: IntrusiveListElement): IntrusiveListElement = {
    insertAfter(elem)
    remove()
    elem
  }

  private[intrusiveList] final def remove(): IntrusiveListElement = {
    if (next.isDefined) {
      next.get.prev = prev
    }
    if (prev.isDefined) {
      prev.get.next = next
    }
    this.next = None
    this.prev = None
    this
  }

  private[intrusiveList] final def append(elem: IntrusiveListElement): IntrusiveListElement = {
    last().insertAfter(elem)
  }

  private[intrusiveList] final def prepend(elem: IntrusiveListElement): IntrusiveListElement = {
    first().insertBefore(elem)
  }

  private[intrusiveList] final def getNext: IntrusiveListElement = next.get

  private[intrusiveList] final def getPrev: IntrusiveListElement = prev.get

  private[intrusiveList] final def hasNext: Boolean = next.isDefined
  private[intrusiveList] final def hasPrev: Boolean = prev.isDefined

  private[intrusiveList] final def last(): IntrusiveListElement = {
    next match  {
      case Some(n) => n.last()
      case None => this
    }
  }

  private[intrusiveList] final def first(): IntrusiveListElement = {
    prev match {
      case Some(n) => n.first()
      case None => this
    }
  }

  private[intrusiveList] final def splice(at: IntrusiveListElement, insertBegin: IntrusiveListElement,
                                          insertEnd: IntrusiveListElement): Unit = {
    assert(insertEnd.last() == insertEnd)
    assert(insertBegin.last() == insertEnd)
    assert(insertBegin.first() == insertBegin)
    assert(insertEnd.first() == insertBegin)
    assert(!at.contains(insertBegin))

    at.next.foreach(_.prev = Some(insertEnd))
    insertBegin.prev = Some(at)
    at.next = Some(insertBegin)

  }

  private[intrusiveList] final def contains(elem: IntrusiveListElement): Boolean = {
    elem.first() == first()
  }

  private[intrusiveList] final def containsRef(elem: IntrusiveListElement): Boolean = {
    elem.first() eq first()
  }


