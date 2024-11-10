package ir.eval
import ir.*
import util.Logger
import util.functional.*
import boogie.Scope

def doLeft[L, T, V, E](f: State[L, V, E]): State[(L, T), V, E] = for {
  n <- State[(L, T), V, E]((s: (L, T)) => {
    val r = f.f(s(0))
    ((r(0), s(1)), r(1))
  })
} yield n

def doRight[L, T, V, E](f: State[T, V, E]): State[(L, T), V, E] = for {
  n <- State[(L, T), V, E]((s: (L, T)) => {
    val r = f.f(s(1))
    ((s(0), r(0)), r(1))
  })
} yield n

/** Runs two interpreters "inner" and "before" simultaneously, returning the value from inner, and ignoring before
  */
case class ProductInterpreter[L, T, E](inner: Effects[L, E], before: Effects[T, E]) extends Effects[(L, T), E] {

  def loadVar(v: String): State[(L, T), BasilValue, E] = for {
    n <- doRight(before.loadVar(v))
    f <- doLeft(inner.loadVar(v))
  } yield f

  def loadMem(v: String, addrs: List[BasilValue]): State[(L, T), List[BasilValue], E] = for {
    n <- doRight(before.loadMem(v, addrs))
    f <- doLeft(inner.loadMem(v, addrs))
  } yield f

  def evalAddrToProc(addr: Int): State[(L, T), Option[FunPointer], E] = for {
    n <- doRight(before.evalAddrToProc(addr: Int))
    f <- doLeft(inner.evalAddrToProc(addr))
  } yield f

  def getNext: State[(L, T), ExecutionContinuation, E] = for {
    n <- doRight(before.getNext)
    f <- doLeft(inner.getNext)
  } yield f

  /** state effects */
  def setNext(c: ExecutionContinuation): State[(L, T), Unit, E] = for {
    n <- doRight(before.setNext(c))
    f <- doLeft(inner.setNext(c))
  } yield f

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation): State[(L, T), Unit, E] = for {
    n <- doRight(before.call(target, beginFrom, returnTo))
    f <- doLeft(inner.call(target, beginFrom, returnTo))
  } yield f

  def callIntrinsic(name: String, args: List[BasilValue]): State[(L, T), Option[BasilValue], E] = for {
    n <- doRight(before.callIntrinsic(name, args))
    f <- doLeft(inner.callIntrinsic(name, args))
  } yield f

  def doReturn(): State[(L, T), Unit, E] = for {
    n <- doRight(before.doReturn())
    f <- doLeft(inner.doReturn())
  } yield f

  def storeVar(v: String, scope: Scope, value: BasilValue): State[(L, T), Unit, E] = for {
    n <- doRight(before.storeVar(v, scope, value))
    f <- doLeft(inner.storeVar(v, scope, value))
  } yield f

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]): State[(L, T), Unit, E] = for {
    n <- doRight(before.storeMem(vname, update))
    f <- doLeft(inner.storeMem(vname, update))
  } yield f
}

case class LayerInterpreter[L, T, E](inner: Effects[L, E], before: Effects[(L, T), E])
    extends Effects[(L, T), E] {

  def loadVar(v: String): State[(L, T), BasilValue, E] = for {
    n <- before.loadVar(v)
    f <- doLeft(inner.loadVar(v))
  } yield f

  def loadMem(v: String, addrs: List[BasilValue]): State[(L, T), List[BasilValue], E] = for {
    n <- before.loadMem(v, addrs)
    f <- doLeft(inner.loadMem(v, addrs))
  } yield f

  def evalAddrToProc(addr: Int): State[(L, T), Option[FunPointer], E] = for {
    n <- before.evalAddrToProc(addr)
    f <- doLeft(inner.evalAddrToProc(addr))
  } yield f

  def getNext: State[(L, T), ExecutionContinuation, E] = for {
    n <- before.getNext
    f <- doLeft(inner.getNext)
  } yield f

  /** state effects */
  def setNext(c: ExecutionContinuation): State[(L, T), Unit, E] = for {
    n <- before.setNext(c)
    f <- doLeft(inner.setNext(c))
  } yield f

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation): State[(L, T), Unit, E] = for {
    n <- before.call(target, beginFrom, returnTo)
    f <- doLeft(inner.call(target, beginFrom, returnTo))
  } yield f

  def callIntrinsic(name: String, args: List[BasilValue]): State[(L, T), Option[BasilValue], E] = for {
    n <- before.callIntrinsic(name, args)
    f <- doLeft(inner.callIntrinsic(name, args))
  } yield f

  def doReturn(): State[(L, T), Unit, E] = for {
    n <- before.doReturn()
    f <- doLeft(inner.doReturn())
  } yield f

  def storeVar(v: String, scope: Scope, value: BasilValue): State[(L, T), Unit, E] = for {
    n <- before.storeVar(v, scope, value)
    f <- doLeft(inner.storeVar(v, scope, value))
  } yield f

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]): State[(L, T), Unit, E] = for {
    n <- before.storeMem(vname, update)
    f <- doLeft(inner.storeMem(vname, update))
  } yield f
}
