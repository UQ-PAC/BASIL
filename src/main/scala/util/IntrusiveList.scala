package intrusivelist
import scala.collection.mutable



// TODO: implement IterableOps
//   So need iterablefactory https://docs.scala-lang.org/overviews/core/custom-collections.html

/**
 * A simple intrusive list implementation.
 *
 * This is a linked list with the links stored as fields within the elements contained in the list, rather
 * than boxing the elements in an external list structure.
 *
 * Therefore this structure can hold any elements that inherit the IntrusiveListElement trait and an intrusive list
 * element can only be a member of a single list at a time.
 *
 * However, this allows us to create an iterator, or simply get the next or previous element from any point in the list,
 * as well as insert or remove anywhere in the list without invalidating the iterator.
 *
 * Insert or remove before or after any element: O(1)
 * Create iterator: O(1)
 * Find element: O(n)
 *
 * @param numElems The size of the list
 * @param firstElem The first list element if nonempty or none if empty.
 * @param lastElem THe last list element if nonempty or none if empty.
 * @tparam T
 */
final class IntrusiveList[T <: IntrusiveListElement[T]] private (var numElems: Int, var firstElem: Option[T],
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
      elem = elem.getNext
    }
    elem
  }

  class IntrusiveListIterator(var elem: Option[T]) extends Iterator[T] {
    override def hasNext: Boolean = elem.isDefined
    override def next: T = {
      val t = elem.get
      elem = t.next
      t
    }
  }

  def iterator: Iterator[T] = IntrusiveListIterator(firstElem)

  // end Iterable


  def iteratorFrom(elem: T): Iterator[T] = {
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
      lastElem = intrusiveListElement.prev
    }
    if (intrusiveListElement == firstElem.get) {
      firstElem = intrusiveListElement.next
    }
    onRemove(intrusiveListElement)
    intrusiveListElement.remove()
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
    intrusiveListElement.insertAfter(newElem)
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
    intrusiveListElement.insertBefore(newElem)
  }

  def getNext(elem: T): T = {
    elem.getNext
  }

  def hasNext(elem: T): Boolean = {
    elem.hasNext
  }

  def hasPrev(elem: T): Boolean = {
    elem.hasPrev
  }

  def getPrev(elem: T): T = {
    elem.getPrev
  }

  def nextOption(elem: T): Option[T] = {
    elem.next
  }

  def prevOption(elem: T): Option[T] = {
    elem.prev
  }

object IntrusiveList {
  def from[T <: IntrusiveListElement[T]](it: IterableOnce[T]): IntrusiveList[T] = {
    val l = new IntrusiveList[T]()
    l.addAll(it)
    l
  }
  def empty[T <: IntrusiveListElement[T]] : IntrusiveList[T] =  new IntrusiveList[T]()
}

trait IntrusiveListElement[T <: IntrusiveListElement[T]]:
  private[intrusivelist] var next: Option[T] = None
  private[intrusivelist] var prev: Option[T] = None
  private[intrusivelist] final def insertBefore(elem: T): T = {
    elem.prev = prev
    if (prev.isDefined) {
      prev.get.next = Some(elem)
    }
    prev = Some(elem)
    elem.next = Some(this.asInstanceOf[T])
    elem
  }

  private[intrusivelist] final def unitary: Boolean = next.isEmpty && prev.isEmpty


  private[intrusivelist] final def insertAfter(elem: T):  T = {
    if (next.isDefined) {
      next.get.prev = Some(elem)
    }
    elem.next = next
    next = Some(elem)
    elem.prev = Some(this.asInstanceOf[T])
    elem
  }

  private[intrusivelist] final def replace(elem: T): T = {
    insertAfter(elem)
    remove()
    elem
  }

  private[intrusivelist] final def remove(): T = {
    if (next.isDefined) {
      next.get.prev = prev
    }
    if (prev.isDefined) {
      prev.get.next = next
    }
    this.next = None
    this.prev = None
    this.asInstanceOf[T]
  }

  def succ(): Option[this.type] = {
    next.map(_.asInstanceOf[this.type])
  }

  def pred(): Option[this.type] = {
    prev.map(_.asInstanceOf[this.type])
  }

  private[intrusivelist] final def append(elem: T): T = {
    last().insertAfter(elem)
  }

  private[intrusivelist] final def prepend(elem: T): T = {
    first().insertBefore(elem)
  }

  private[intrusivelist] final def getNext: T = next.get

  private[intrusivelist] final def getPrev: T = prev.get

  private[intrusivelist] final def hasNext: Boolean = next.isDefined
  private[intrusivelist] final def hasPrev: Boolean = prev.isDefined

  private[intrusivelist] final def last(): T = {
    next match  {
      case Some(n) => n.last()
      case None => this.asInstanceOf[T]
    }
  }

  private[intrusivelist] final def first(): T = {
    prev match {
      case Some(n) => n.first()
      case None => this.asInstanceOf[T]
    }
  }

  private[intrusivelist] final def splice(at: T, insertBegin: T,
                                          insertEnd: T): Unit = {
    assert(insertEnd.last() == insertEnd)
    assert(insertBegin.last() == insertEnd)
    assert(insertBegin.first() == insertBegin)
    assert(insertEnd.first() == insertBegin)
    assert(!at.contains(insertBegin))

    at.next.foreach(_.prev = Some(insertEnd))
    insertBegin.prev = Some(at)
    at.next = Some(insertBegin)

  }

  private[intrusivelist] final def contains(elem: T): Boolean = {
    elem.first() == first()
  }

  private[intrusivelist] final def containsRef(elem: T): Boolean = {
    elem.first() eq first()
  }


