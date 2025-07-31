package util.intrusive_list
import util.assertion.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// TODO: implement IterableOps
//   So need iterablefactory https://docs.scala-lang.org/overviews/core/custom-collections.html

/** A simple intrusive list implementation.
  *
  * This is a linked list with the links stored as fields within the elements contained in the list, rather than boxing
  * the elements in an external list structure. This means an IntrusiveListElement can only be a member of one intrusive
  * list at a time.
  *
  * Therefore this structure can hold any elements that inherit the IntrusiveListElement trait and an intrusive list
  * element can only be a member of a single list at a time.
  *
  * However, this allows us to create an iterator, or simply get the next or previous element from any point in the
  * list, as well as insert or remove anywhere in the list without invalidating the iterator.
  *
  * Insert or remove before or after any element: O(1) Create iterator: O(1) Find element: O(n)
  *
  * @param numElems
  *   The size of the list
  * @param firstElem
  *   The first list element if nonempty or none if empty.
  * @param lastElem
  *   THe last list element if nonempty or none if empty.
  * @tparam T
  */
final class IntrusiveList[T <: IntrusiveListElement[T]] private (
  var numElems: Int,
  var firstElem: Option[T],
  var lastElem: Option[T]
) extends mutable.Iterable[T],
      mutable.Growable[T]:
  /* Method called on the element whenever it is inserted to this list. */
  var onInsert: T => Unit = x => ()
  /* Method called on the element whenever it is removed from this list. */
  var onRemove: T => Unit = x => ()

  // invariant:
  //    numElems == first.length()

  //    if size == 0 then first == last == None
  //    else if size == 1 then first.get == last.get
  //    else first = last.get.first() && last == first.get.last()

  // for Growable
  override def knownSize: Int = numElems

  override def addOne(elem: T): IntrusiveList.this.type = {
    append(elem)
    this
  }

  /** Remove all elements, O(n).
    */
  override def clear(): Unit = {
    while (size > 0) {
      this.remove(lastElem.get)
    }
  }

  /** Add all elements from the iterator. The elements must already be in any other IntrusiveList.
    * @param xs
    *   Elements to add from.
    * @return
    */
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
    debugAssert(i < size)
    var elem = firstElem.get
    for (c <- 0 until i) {
      elem = elem.getNext
    }
    elem
  }

  private class IntrusiveListIterator(var elem: Option[T], forward: Boolean) extends Iterator[T] {
    override def hasNext: Boolean = elem.isDefined
    override def next: T = {
      val t = elem.get
      elem = if forward then t.next else t.prev
      t
    }
  }

  /*
   * Return a forward iterator from the beginning of the list. The iterator stores the next element, so it is
   * Safe to remove elements returned by the iterator from the list.
   * O(1)
   */
  override def iterator: Iterator[T] = IntrusiveListIterator(firstElem, true)

  /*
   * Return a reverse iterator from the end of the list. The iterator stores the next element, so it is
   * Safe to remove elements returned by the iterator from the list.
   * O(1)
   */
  def reverseIterator: Iterator[T] = IntrusiveListIterator(lastElem, false)

  /** Return an iterator beginning at a specific element in the list. O(1) It is safe to modify or remove elements
    * returned by the iterator.
    * @param elem
    *   The elemenet to begin at
    * @param forward
    *   Iterate forwards (defualt) or backwards.
    * @return
    *   The iterator
    */
  def iteratorFrom(elem: T, forward: Boolean = true): Iterator[T] = {
    debugAssert(elem.first() == firstElem.get)
    IntrusiveListIterator(Some(elem), forward)
  }

  // Implementation

  override def size: Int = numElems

  /** Unsafely return the first element of the list.
    */
  override def head: T = firstElem.get

  override def headOption: Option[T] = firstElem

  /** Unsafely return the first element of the list.
    */
  def begin: T = firstElem.get

  /** Check whether the list contains the given element (by reference) by linear scan. O(n)
    */
  private def containsRef(elem: T): Boolean = {
    if (size == 0) {
      false
    } else {
      firstElem.get.containsRef(elem)
    }
  }

  /** Check whether the list contains the given element (by value) by linear scan. O(n)
    */
  def contains(elem: T): Boolean = {
    if (size == 0) {
      false
    } else {
      firstElem.get.contains(elem)
    }
  }

  /** Unsafely return the last element of the list.
    */
  def back: T = lastElem.get

  /** Add an element to the beginning of the list. The element must not be a member of any other IntrusiveList.
    * @param newElem
    *   The element to add
    * @return
    *   The element
    */
  def prepend(newElem: T): T = {
    debugAssert(newElem.unitary)
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

  def prependAll(elems: Iterable[T]) = {
    require(elems.toSet.size == elems.size)
    // first == None ==> empty list
    insertAllBefore(firstElem, elems)
  }

  def appendAll(elems: Iterable[T]) = {
    // last == None ==> empty list
    insertAllAfter(lastElem, elems)
  }

  /** Add an element to the end of the list. The element must not be a member of any other IntrusiveList.
    *
    * @param newElem
    *   The element to add
    * @return
    *   The element
    */
  def append(newElem: T): T = {
    debugAssert(newElem.unitary)
    onInsert(newElem)
    if (size > 0) {

      // HERE
      insertAfter(lastElem.get, newElem)
    } else {
      firstElem = Some(newElem)
      lastElem = Some(newElem)
      numElems = 1
    }
    newElem
  }

  /** Replace an element with another, at the same position in the list.
    * @param elem
    *   The element to remove.
    * @param withElem
    *   The element to add, must not be a member of any other IntrusiveList.
    * @return
    *   The added element
    */
  def replace(elem: T, withElem: T): T = {
    debugAssert(containsRef(elem), "elem is not an element of this list, replace() call could mangle start and end")
    if (elem ne withElem) {
      debugAssert(withElem.unitary)
      val newElem: T = insertAfter(elem, withElem)
      val removed = remove(elem)
      newElem
    } else {
      withElem
    }
  }

  /** Removes all elements after the provided element n and returns an ArrayBuffer containing the removed elements,
    * maintaining the ordering.
    *
    * @param n
    *   The element to split on, remains in the first list.
    * @return
    *   An ArrayBuffer containing all elements after n.
    */
  def splitOn(n: T): ArrayBuffer[T] = {
    debugAssert(containsRef(n), "Cannot split on element not in this list")

    val newlist = ArrayBuffer[T]()
    var next = n.next
    while (next.isDefined) {
      remove(next.get)
      newlist.addOne(next.get)

      next = n.next
    }

    newlist
  }

  /** Remove an element from the list.
    *
    * @param intrusiveListElement
    *   the element to remove
    * @return
    *   The removed element
    */
  def remove(intrusiveListElement: T): T = {
    debugAssert(size >= 0)
    debugAssert(containsRef(intrusiveListElement), "Cannot remove element not in this list")
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

  /** Insert an element after another element in the list.
    * @param intrusiveListElement
    *   The element in the list to insert after.
    * @param newElem
    *   The element to insert. Must not be a member of any other intrusive list.
    * @return
    *   the inserted element
    */
  def insertAfter(intrusiveListElement: T, newElem: T): T = {
    debugAssert(size >= 1)
    // debugAssert(
    //  containsRef(intrusiveListElement),
    //  "element is not a member of this list, insertAfter could mangle start and end tracking"
    // )
    debugAssert(newElem.unitary)
    numElems += 1
    if (intrusiveListElement == lastElem.get) {
      lastElem = Some(newElem)
    }

    onInsert(newElem)
    intrusiveListElement.insertAfter(newElem)
  }

  /** Insert an element after another element in the list.
    * @param intrusiveListElement
    *   The element in the list to insert after, or None to indicate the beginning.
    * @param newElems
    *   The elements to insert. Must not be members of any other intrusive list(s).
    * @return
    *   the last inserted element, or the reference element
    */
  def insertAllAfter(intrusiveListElement: Option[T], newElems: Iterable[T]): Option[T] = {
    intrusiveListElement match {
      case None =>
        newElems.toList.reverse.map(prepend).headOption.orElse(intrusiveListElement)
      case Some(n) =>
        var p = n
        for (i <- newElems) {
          p = insertAfter(p, i)
        }
        Some(p)
    }
  }

  /** Insert an element before another element in the list.
    * @param intrusiveListElement
    *   The element in the list to insert before, or None to indicate the end of the list.
    * @param newElems
    *   The elements to insert. Must not be members of any other intrusive list(s).
    * @return
    *   the last inserted element, or the reference element
    */
  def insertAllBefore(intrusiveListElement: Option[T], newElems: Iterable[T]): Option[T] = {
    intrusiveListElement match {
      case None =>
        appendAll(newElems)
        lastElem
      case Some(n) =>
        var p = n
        for (i <- newElems.toList.reverse) {
          p = insertBefore(p, i)
        }
        Some(p)
    }
  }

  /** Insert an element before another element in the list.
    *
    * @param intrusiveListElement
    *   The element in the list to insert before.
    * @param newElem
    *   The element to insert. Must not be a member of any other intrusive list.
    * @return
    *   the inserted element
    */
  def insertBefore(intrusiveListElement: T, newElem: T): T = {
    debugAssert(size >= 1)
    debugAssert(
      containsRef(intrusiveListElement),
      "Element is not in this list, insert before could mangle start and end tracking."
    )
    debugAssert(newElem.unitary)
    numElems += 1
    if (intrusiveListElement == firstElem.get) {
      firstElem = Some(newElem)
    }
    onInsert(newElem)
    intrusiveListElement.insertBefore(newElem)
  }

  /** Unsafely return the element after a given element.
    */
  def getNext(elem: T): T = {
    elem.getNext
  }

  /** Return whether \elem has a successor.
    */
  def hasNext(elem: T): Boolean = {
    elem.hasNext
  }

  /** Return whether \elem has a predecessor.
    */
  def hasPrev(elem: T): Boolean = {
    elem.hasPrev
  }

  /** Unsafely return the element after the provided element.
    */
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
  def empty[T <: IntrusiveListElement[T]]: IntrusiveList[T] = IntrusiveList[T]()
}

/** Internal implementation of the intrusive list. This stores the inter-element links, but is only accessed by the
  * containing IntrusiveList implementation, so that the size, beginning and end are cached.
  * @tparam T
  *   The elements own type.
  */
trait IntrusiveListElement[T <: IntrusiveListElement[T]]:
  private[intrusive_list] var next: Option[T] = None
  private[intrusive_list] var prev: Option[T] = None
  private[intrusive_list] final def insertBefore(elem: T): T = {
    require(elem != this)
    elem.prev = prev
    if (prev.isDefined) {
      prev.get.next = Some(elem)
    }
    prev = Some(elem)
    elem.next = Some(this.asInstanceOf[T])
    elem
  }

  private[intrusive_list] final def unitary: Boolean = next.isEmpty && prev.isEmpty

  private[intrusive_list] final def insertAfter(elem: T): T = {
    require(elem != this)
    if (next.isDefined) {
      next.get.prev = Some(elem)
    }
    elem.next = next
    next = Some(elem)
    elem.prev = Some(this.asInstanceOf[T])
    elem
  }

  private[intrusive_list] final def replace(elem: T): T = {
    insertAfter(elem)
    remove()
    elem
  }

  private[intrusive_list] final def remove(): T = {
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

  /** @return
    *   Some(next) where next is the next element in the list this belongs to. None when this is the last element.
    */
  def succ(): Option[this.type] = {
    next.map(_.asInstanceOf[this.type])
  }

  /** @return
    *   Some(prev) where prev is the previous element in the list this belongs to. None when this is the first element.
    */
  def pred(): Option[this.type] = {
    prev.map(_.asInstanceOf[this.type])
  }

  private[intrusive_list] final def append(elem: T): T = {
    last().insertAfter(elem)
  }

  private[intrusive_list] final def prepend(elem: T): T = {
    first().insertBefore(elem)
  }

  private[intrusive_list] final def getNext: T =
    val n = next.get
    assert(n != this)
    n

  private[intrusive_list] final def getPrev: T =
    val p = prev.get
    assert(p != this)
    p

  private[intrusive_list] final def hasNext: Boolean = next.isDefined
  private[intrusive_list] final def hasPrev: Boolean = prev.isDefined

  private[intrusive_list] final def last(): T = {
    next match {
      case Some(n) if n == next => throw Exception(s"IntrusiveList self loop $this")
      case Some(n) => n.last()
      case None => this.asInstanceOf[T]
    }
  }

  private[intrusive_list] final def first(): T = {
    prev match {
      case Some(n) if n == prev => throw Exception(s"IntrusiveList self loop $this")
      case Some(n) => n.first()
      case None => this.asInstanceOf[T]
    }
  }

  private[intrusive_list] final def splice(at: T, insertBegin: T, insertEnd: T): Unit = {
    debugAssert(insertEnd.last() == insertEnd)
    debugAssert(insertBegin.last() == insertEnd)
    debugAssert(insertBegin.first() == insertBegin)
    debugAssert(insertEnd.first() == insertBegin)
    debugAssert(!at.contains(insertBegin))

    at.next.foreach(_.prev = Some(insertEnd))
    insertBegin.prev = Some(at)
    at.next = Some(insertBegin)

  }

  private[intrusive_list] final def contains(elem: T): Boolean = {
    elem.first() == first()
  }

  private[intrusive_list] final def containsRef(elem: T): Boolean = {
    elem.first() eq first()
  }
