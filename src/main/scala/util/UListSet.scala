/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 *
 * WITH MODIFICATIONS (C) UQ
 *
 */

package scala
package collection
package immutable

import mutable.{Builder, ImmutableBuilder}
import scala.annotation.tailrec
import scala.collection.generic.DefaultSerializable

/**
  * This class implements immutable sets using a list-based data structure. List set iterators and
  * traversal methods visit elements in the order they were first inserted.
  *
  * Elements are stored internally in reversed insertion order, which means the newest element is at
  * the head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and
  * `init` are O(1). Other operations, such as inserting or removing entries, are also O(n), which
  * makes this collection suitable only for a small number of elements.
  *
  * Instances of `UListSet` represent empty sets; they can be either created by calling the
  * constructor directly, or by applying the function `UListSet.empty`.
  *
  * @tparam A the type of the elements contained in this list set
  *
  * @define Coll UListSet
  * @define coll list set
  * @define mayNotTerminateInf
  * @define willNotTerminateInf
  */
sealed class UListSet[A]
  extends AbstractSet[A]
    with StrictOptimizedSetOps[A, UListSet, UListSet[A]]
    with IterableFactoryDefaults[A, UListSet]
    with DefaultSerializable {

  override protected[this] def className: String = "UListSet"

  override def size: Int = 0
  override def knownSize: Int = 0
  override def isEmpty: Boolean = true

  def contains(elem: A): Boolean = false

  def inclUnsafe(e: A): UListSet[A] = new Node(e)
  def incl(elem: A): UListSet[A] = new Node(elem)
  def excl(elem: A): UListSet[A] = this


  def iterator: scala.collection.Iterator[A] = {
    var curr: UListSet[A] = this
    var res: List[A] = Nil
    while (!curr.isEmpty) {
      res = curr.elem :: res
      curr = curr.next
    }
    res.iterator
  }

  protected def elem: A = throw new NoSuchElementException("elem of empty set")
  protected def next: UListSet[A] = throw new NoSuchElementException("next of empty set")

  override def iterableFactory: IterableFactory[UListSet] = UListSet


  /**
    * Represents an entry in the `UListSet`.
    */
  protected class Node(override protected val elem: A) extends UListSet[A] {

    override def size = sizeInternal(this, 0)
    override def knownSize: Int = -1
    @tailrec private[this] def sizeInternal(n: UListSet[A], acc: Int): Int =
      if (n.isEmpty) acc
      else sizeInternal(n.next, acc + 1)

    override def isEmpty: Boolean = false

    override def contains(e: A): Boolean = containsInternal(this, e)

    @tailrec private[this] def containsInternal(n: UListSet[A], e: A): Boolean =
      !n.isEmpty && (n.elem == e || containsInternal(n.next, e))

    override def incl(e: A): UListSet[A] = if (contains(e)) this else new Node(e)

    override def inclUnsafe(e: A): UListSet[A] = new Node(e)

    override def excl(e: A): UListSet[A] = removeInternal(e, this, Nil)

    @tailrec private[this] def removeInternal(k: A, cur: UListSet[A], acc: List[UListSet[A]]): UListSet[A] =
      if (cur.isEmpty) acc.last
      else if (k == cur.elem) acc.foldLeft(cur.next)((t, h) => new t.Node(h.elem))
      else removeInternal(k, cur.next, cur :: acc)

    override protected def next: UListSet[A] = UListSet.this

    override def last: A = elem

    override def init: UListSet[A] = next
  }
}

/**
  * $factoryInfo
  *
  * Note that each element insertion takes O(n) time, which means that creating a list set with
  * n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
  * elements.
  *
  * @define Coll UListSet
  * @define coll list set
  */
@SerialVersionUID(3L)
object UListSet extends IterableFactory[UListSet] {

  def fromDistinct[E](it: scala.collection.IterableOnce[E]): UListSet[E] = {
    it match {
      case ls: UListSet[E] => ls
      case _ if it.knownSize == 0 => empty[E]
      case _ => (newUnsafeBuilder[E] ++= it).result()
    }
  }

  def from[E](it: scala.collection.IterableOnce[E]): UListSet[E] =
    it match {
      case ls: UListSet[E] => ls
      case _ if it.knownSize == 0 => empty[E]
      case _ => (newBuilder[E] ++= it).result()
    }

  private object EmptyUListSet extends UListSet[Any] {
    override def knownSize: Int = 0
  }
  private[collection] def emptyInstance: UListSet[Any] = EmptyUListSet

  def empty[A]: UListSet[A] = EmptyUListSet.asInstanceOf[UListSet[A]]

  def newUnsafeBuilder[A]: Builder[A, UListSet[A]] =
    new ImmutableBuilder[A, UListSet[A]](empty) {
      def addOne(elem: A): this.type = { elems = elems.inclUnsafe(elem); this }
    }

  def newBuilder[A]: Builder[A, UListSet[A]] =
    new ImmutableBuilder[A, UListSet[A]](empty) {
      def addOne(elem: A): this.type = { elems = elems + elem; this }
    }
}
