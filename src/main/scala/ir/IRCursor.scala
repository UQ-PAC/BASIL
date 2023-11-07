package intrusiveList

import collection.immutable.Seq
import collection.mutable





class IntrusiveList[T <: IntrusiveListElement]():

  private val list: mutable.HashSet[T] = mutable.HashSet.empty
  // if size == 1 then first.get == last.get
  private var first: Option[T] = None
  private var last: Option[T] = None

  def head(): T = first.get

  def back(): T = last.get

  def prepend(newElem: T): T = {
    if (first.isDefined) {
      first.get.insertBefore(newElem)
    }
    first = Some(newElem)
    if (last.isEmpty) {
      last = Some(newElem)
    }
    list.add(newElem)
    newElem
  }

  def append(newElem : T): T = {
    if (last.isDefined) {
      last.get.insertAfter(newElem)
    }
    last = Some(newElem)
    if (first.isEmpty) {
      first = Some(newElem)
    }
    assert(list.add(newElem))
    newElem
  }

  def remove(intrusiveListElement: T): T = {
    list.remove(intrusiveListElement)
    intrusiveListElement.remove().asInstanceOf[T]
  }

  def insertAfter(intrusiveListElement: T, newElem: T): Unit = {
    assert(list.contains(intrusiveListElement))
    list.add(intrusiveListElement.insertAfter(newElem).asInstanceOf[T])
    if (intrusiveListElement == last) {
      last = Some(newElem)
    }
  }
  def insertBefore(intrusiveListElement: T, newElem: T): Unit = {
    assert(list.contains(intrusiveListElement))
    list.add(intrusiveListElement.insertBefore(newElem).asInstanceOf[T])

    if (intrusiveListElement == first) {
      first = Some(newElem)
    }
  }

  def size: Int = list.size

trait IntrusiveListElement:
  private var next: Option[IntrusiveListElement] = None
  private var prev: Option[IntrusiveListElement] = None
  private[intrusiveList] def insertBefore(elem: IntrusiveListElement): IntrusiveListElement = {
    elem.prev = prev
    if (prev.isDefined) {
      prev.get.next = Some(elem)
    }
    prev = Some(elem)
    elem.next = Some(this)
    elem
  }

  private[intrusiveList] def insertAfter(elem: IntrusiveListElement): IntrusiveListElement = {
    if (next.isDefined) {
      next.get.prev = Some(elem)
    }
    elem.next = next
    next = Some(elem)
    elem.prev = Some(this)
    elem
  }

  private[intrusiveList] def remove(): IntrusiveListElement = {
    if (next.isDefined) {
      next.get.prev = prev
    }
    if (prev.isDefined) {
      prev.get.next = next
    }
    this
  }

  private[intrusiveList] def append(elem: IntrusiveListElement): IntrusiveListElement = {
    last().insertAfter(elem)
  }

  private[intrusiveList] def prepend(elem: IntrusiveListElement): IntrusiveListElement = {
    first().insertBefore(elem)
  }

  private[intrusiveList] def getNext: IntrusiveListElement = next.get

  private[intrusiveList] def getPrev: IntrusiveListElement = prev.get

  private[intrusiveList] def hasNext: Boolean = next.isDefined
  private[intrusiveList] def hasPrev: Boolean = prev.isDefined

  private[intrusiveList] def last(): IntrusiveListElement = {
      next match  {
        case Some(n) => n.last()
        case None => this
      }
    }

  private[intrusiveList] def first(): IntrusiveListElement = {
    prev match {
      case Some(n) => n.first()
      case None => this
    }
  }




//type Position = Procedure | Block | Statement
//class IRCursor(val position: Position) {
//
//  //def succ() : Seq(IRCursor) = {
//  //  position match {
//  //    case p: Procedure => Seq(IRCursor(p.blocks.head))
//  //    case b: Block => Seq(IRCursor(b.statements.head))
//  //    case s: Statement => Seq(s.parent.statements.ne())
//
//  //  }
//  //}
//
//
//}
