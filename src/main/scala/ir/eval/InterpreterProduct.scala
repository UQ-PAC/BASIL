package ir.eval

import boogie.Scope
import ir.*
import util.functional.*

def doLeft[L, T, V, E](f: State[L, V, E]): State[(L, T), V, E] = for {
  n <- State[(L, T), V, E]((s: (L, T)) => {
    val r = f.f(s._1)
    ((r._1, s._2), r._2)
  })
} yield (n)

def doRight[L, T, V, E](f: State[T, V, E]): State[(L, T), V, E] = for {
  n <- State[(L, T), V, E]((s: (L, T)) => {
    val r = f.f(s._2)
    ((s._1, r._1), r._2)
  })
} yield (n)

/** Runs two interpreters "inner" and "before" simultaneously, returning the value from inner, and ignoring before
  */
case class ProductInterpreter[L, T, E](inner: Effects[L, E], before: Effects[T, E]) extends Effects[(L, T), E] {

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
  } yield (f)

  def getNext = for {
    n <- doRight(before.getNext)
    f <- doLeft(inner.getNext)
  } yield (f)

  /** state effects */
  def setNext(c: ExecutionContinuation) = for {
    n <- doRight(before.setNext(c))
    f <- doLeft(inner.setNext(c))
  } yield (f)

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = for {
    n <- doRight(before.call(target, beginFrom, returnTo))
    f <- doLeft(inner.call(target, beginFrom, returnTo))
  } yield (f)

  def callIntrinsic(name: String, args: List[BasilValue]) = for {
    n <- doRight(before.callIntrinsic(name, args))
    f <- doLeft(inner.callIntrinsic(name, args))
  } yield (f)

  def doReturn() = for {
    n <- doRight(before.doReturn())
    f <- doLeft(inner.doReturn())
  } yield (f)

  def storeVar(v: String, scope: Scope, value: BasilValue) = for {
    n <- doRight(before.storeVar(v, scope, value))
    f <- doLeft(inner.storeVar(v, scope, value))
  } yield (f)

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = for {
    n <- doRight(before.storeMem(vname, update))
    f <- doLeft(inner.storeMem(vname, update))
  } yield (f)
}

case class LayerInterpreter[L, T, E](inner: Effects[L, E], before: Effects[(L, T), E]) extends Effects[(L, T), E] {

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
  } yield (f)

  def getNext = for {
    n <- (before.getNext)
    f <- doLeft(inner.getNext)
  } yield (f)

  /** state effects */
  def setNext(c: ExecutionContinuation) = for {
    n <- (before.setNext(c))
    f <- doLeft(inner.setNext(c))
  } yield (f)

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = for {
    n <- (before.call(target, beginFrom, returnTo))
    f <- doLeft(inner.call(target, beginFrom, returnTo))
  } yield (f)

  def callIntrinsic(name: String, args: List[BasilValue]) = for {
    n <- before.callIntrinsic(name, args)
    f <- doLeft(inner.callIntrinsic(name, args))
  } yield (f)

  def doReturn() = for {
    n <- (before.doReturn())
    f <- doLeft(inner.doReturn())
  } yield (f)

  def storeVar(v: String, scope: Scope, value: BasilValue) = for {
    n <- (before.storeVar(v, scope, value))
    f <- doLeft(inner.storeVar(v, scope, value))
  } yield (f)

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = for {
    n <- (before.storeMem(vname, update))
    f <- doLeft(inner.storeMem(vname, update))
  } yield (f)
}
