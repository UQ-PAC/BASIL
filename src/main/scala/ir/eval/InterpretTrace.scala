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


enum ExecEffect:
  case Call(target: String, begin: ExecutionContinuation, returnTo: ExecutionContinuation)
  case Return
  case StoreVar(v: String, s: Scope, value: BasilValue)
  case LoadVar(v: String)
  case StoreMem(vname: String, update: Map[BasilValue, BasilValue])
  case LoadMem(vname: String, addrs: List[BasilValue])
  case FindProc(addr: Int)

case class Trace(val t: List[ExecEffect])

case object Trace {
  def add(e: ExecEffect) : State[Trace, Unit] = {
    State.modify ((t: Trace) => Trace(t.t.appended(e)))
  }
}

object TraceGen extends Effects[Trace] {
  /** Values are discarded by ProductInterpreter so do not matter */
  def evalBV(e: Expr) = State.pure(BitVecLiteral(0,0))

  def evalInt(e: Expr) = State.pure(BigInt(0))

  def evalBool(e: Expr) = State.pure(false)

  def loadVar(v: String) = for {
    s <- Trace.add(ExecEffect.LoadVar(v))
  } yield (Scalar(FalseLiteral))

  def loadMem(v: String, addrs: List[BasilValue])  = for {
    s <- Trace.add(ExecEffect.LoadMem(v, addrs))
  } yield (List())

  def evalAddrToProc(addr: Int) = for {
    s <- Trace.add(ExecEffect.FindProc(addr))
  } yield (None)

  def getNext = State.pure(Stopped())

  def setNext(c: ExecutionContinuation)  = State.pure(())

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = for {
    s <- Trace.add(ExecEffect.Call(target, beginFrom, returnTo))
  } yield (())

  def doReturn() = for {
    s <- Trace.add(ExecEffect.Return)
  } yield (())

  def storeVar(v: String, scope: Scope, value: BasilValue) = for {
    s <- Trace.add(ExecEffect.StoreVar(v, scope, value))
  } yield (())

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = for {
    s <- Trace.add(ExecEffect.StoreMem(vname, update))
  } yield (())

  def interpretOne = State.pure(())
}

def tracingInterpreter = ProductInterpreter(NormalInterpreter, TraceGen)

def interpretTrace(p: Program) : (InterpreterState, Trace) = {
  InterpFuns.interpretProg(tracingInterpreter)(p, (InterpreterState(), Trace(List())))
}


