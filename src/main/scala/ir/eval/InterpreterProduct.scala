
package ir.eval
import ir._
import ir.eval.BitVectorEval.*
import ir.*
import util.Logger
import util.functional.*
import util.functional.State.*
import boogie.Scope
import scala.collection.WithFilter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.util.control.Breaks.{break, breakable}


def doLeft[L, T, V](f: State[L, V]) : State[(L, T), V] = for {
  f <- State[(L, T), V]((s: (L, T)) => {
    val r = f.f(s._1)
    ((r._1, s._2), r._2)
  })
} yield (f)

def doRight[L, T, V](f: State[T, V]) : State[(L, T), V] = for {
  f <- State[(L, T), V]((s: (L, T)) => {
    val r = f.f(s._2)
    ((s._1, r._1), r._2)
  })
} yield (f)

/**
 * Runs two interpreters "inner" and "before" simultaneously, returning the value from inner, and ignoring before
 */
case class ProductInterpreter[L, T](val inner: Effects[L], val before: Effects[T]) extends Effects[(L, T)] {
  def interpretOne = for {
    n <- doRight(before.interpretOne)
    f <- doLeft(inner.interpretOne)
  } yield ()

  def loadVar(v: String) = for {
    n <- doRight(before.loadVar(v))
    f <- doLeft(inner.loadVar(v))
  } yield (f)

  def loadMem(v: String, addrs: List[BasilValue]) = for {
    n <- doRight(before.loadMem(v, addrs))
    f <- doLeft(inner.loadMem(v, addrs))
  } yield (f)

  def evalAddrToProc(addr: Int) = for {
    n <- doRight(before.evalAddrToProc(addr: Int))
    f <- doLeft(inner.evalAddrToProc(addr))
  } yield(f)

  def getNext = for {
    n <- doRight(before.getNext)
    f <- doLeft(inner.getNext)
  } yield(f)

  /** state effects */
  def setNext(c: ExecutionContinuation) = for {
    n <- doRight(before.setNext(c))
    f <- doLeft(inner.setNext(c))
  } yield (f)

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = for {
    n <- doRight(before.call(target, beginFrom, returnTo))
    f <- doLeft(inner.call(target, beginFrom, returnTo))
  } yield (f)

  def doReturn() = for {
    n <- doRight(before.doReturn())
    f <- doLeft(inner.doReturn())
  } yield (f)

  def storeVar(v: String, scope: Scope, value: BasilValue) = for {
    n <- doRight(before.storeVar(v, scope, value))
    f <- doLeft(inner.storeVar(v, scope, value))
  } yield(f)

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = for {
    n <- doRight(before.storeMem(vname,update))
    f <- doLeft(inner.storeMem(vname, update))
  } yield(f)
}


case class LayerInterpreter[L, T](val inner: Effects[L], val before: Effects[(L, T)]) extends Effects[(L, T)] {

  def interpretOne = for {
    n <- (before.interpretOne)
    f <- doLeft(inner.interpretOne)
  } yield ()

  def loadVar(v: String) = for {
    n <- (before.loadVar(v))
    f <- doLeft(inner.loadVar(v))
  } yield (f)

  def loadMem(v: String, addrs: List[BasilValue]) = for {
    n <- (before.loadMem(v, addrs))
    f <- doLeft(inner.loadMem(v, addrs))
  } yield (f)

  def evalAddrToProc(addr: Int) = for {
    n <- (before.evalAddrToProc(addr: Int))
    f <- doLeft(inner.evalAddrToProc(addr))
  } yield(f)

  def getNext = for {
    n <- (before.getNext)
    f <- doLeft(inner.getNext)
  } yield(f)

  /** state effects */
  def setNext(c: ExecutionContinuation) = for {
    n <- (before.setNext(c))
    f <- doLeft(inner.setNext(c))
  } yield (f)

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = for {
    n <- (before.call(target, beginFrom, returnTo))
    f <- doLeft(inner.call(target, beginFrom, returnTo))
  } yield (f)

  def doReturn() = for {
    n <- (before.doReturn())
    f <- doLeft(inner.doReturn())
  } yield (f)

  def storeVar(v: String, scope: Scope, value: BasilValue) = for {
    n <- (before.storeVar(v, scope, value))
    f <- doLeft(inner.storeVar(v, scope, value))
  } yield(f)

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = for {
    n <- (before.storeMem(vname,update))
    f <- doLeft(inner.storeMem(vname, update))
  } yield(f)
}

