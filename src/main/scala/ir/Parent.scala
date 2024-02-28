package ir
import util.Logger

import scala.io.AnsiColor

trait HasParent[T]:
  /*
      If a node is reachable from the IL then it *must* have a parent defined. This will only be null until
      the object is fully initialised.

      All IL structures must set the parent of the child to itself, when a child is added to itself.
   */
  private var _parent: Option[T] = None
  private var last_parent: Option[(T, String, String, Int)] = None
  def parent: T = {
    _parent match {
      case Some(p) => p
      case None =>
        val msg = s"Trying to get the parent of a node that is detached from the progam: $this. " +
          s"Node was last attached to: ${last_parent.map(_._1)}, detached at ${last_parent.map(x => x._2 + x._3 + "@" + x._4)} "
        Logger.error(msg)
        throw new RuntimeException(msg)
    }
  }

  def hasParent: Boolean = _parent.isDefined

  def parent_=(value: T): Unit = setParent(value)


  /**
   * Update any IL control-flow links implied by this relation.
   * NOT necessarily idempotent.
   *  For example;
   *    - Registering calls with their target procedure
   *    - Registering jumps with their target block
   *
   * TODO: consider making abstract to force implementers to consider the linking.
   * @param p The new parent
   */
  protected[this] def linkParent(p: T): Unit = ()

  /**
   * Update any IL control-flow links implied by this relation.
   * NOT necessarily idempotent.
   */
  protected[this] def unlinkParent(): Unit = ()


  /**
   * Remove this element's parent and update any IL control-flow links implied by this relation.
   * Is idempotent.
   */
  final def deParent()(implicit line: sourcecode.Line, file: sourcecode.FileName, name: sourcecode.Name): Unit = {
    if _parent.isDefined then {
      last_parent = _parent.map(x => (x, file.value, name.value, line.value))
      unlinkParent()
      _parent = None
    }
  }

  /**
   * Set this element's parent and update any IL control-flow links implied by this relation.
   * If another parent is already set then it will be de-parented and unlinked from that first.
   * Is idempotent.
   */
  final def setParent(p: T): Unit = {
    _parent match
      case Some(existing) if existing == p => ()
      case None =>
        _parent = Some(p)
        linkParent(p)
      case Some(_) =>
        deParent()
        _parent = Some(p)
        linkParent(p)
  }

