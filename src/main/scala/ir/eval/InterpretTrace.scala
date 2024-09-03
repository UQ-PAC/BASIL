package ir.eval
import ir._
import ir.eval.BitVectorEval.*
import ir.*
import util.IRContext
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
  def add[E](e: ExecEffect) : State[Trace, Unit, E] = {
    State.modify ((t: Trace) => Trace(t.t.appended(e)))
  }
}

case class TraceGen[E]() extends NopEffects[Trace, E] {
  /** Values are discarded by ProductInterpreter so do not matter */

  // override def loadVar(v: String) = for {
  //   s <- Trace.add(ExecEffect.LoadVar(v))
  // } yield (Scalar(FalseLiteral))

  override def loadMem(v: String, addrs: List[BasilValue])  = for {
    s <- Trace.add(ExecEffect.LoadMem(v, addrs))
  } yield (List())

  override def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = for {
    s <- Trace.add(ExecEffect.Call(target, beginFrom, returnTo))
  } yield (())

  override def doReturn() = for {
    s <- Trace.add(ExecEffect.Return)
  } yield (())

  override def storeVar(v: String, scope: Scope, value: BasilValue) = for {
    s <- if (!v.startsWith("ghost")) Trace.add(ExecEffect.StoreVar(v, scope, value)) else State.pure(())
  } yield (())

  override def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = for {
    s <- if (!vname.startsWith("ghost")) Trace.add(ExecEffect.StoreMem(vname, update)) else State.pure(())
  } yield (())

}

def tracingInterpreter = ProductInterpreter(NormalInterpreter, TraceGen())

def interpretTrace(p: Program) : (InterpreterState, Trace) = {
  InterpFuns.interpretProg(tracingInterpreter)(p, (InterpreterState(), Trace(List())))
}

def interpretTrace(p: IRContext) : (InterpreterState, Trace) = {
  InterpFuns.interpretProg(tracingInterpreter)(p, (InterpreterState(), Trace(List())))
}

