package ir.eval
import ir.*
import util.IRContext
import util.Logger
import util.functional.*
import boogie.Scope

enum ExecEffect:
  case Call(target: String, begin: ExecutionContinuation, returnTo: ExecutionContinuation)
  case Return
  case StoreVar(v: String, s: Scope, value: BasilValue)
  case LoadVar(v: String)
  case StoreMem(vname: String, update: Map[BasilValue, BasilValue])
  case LoadMem(vname: String, addrs: List[BasilValue])
  case FindProc(addr: Int)

case class Trace(t: List[ExecEffect])

case object Trace {
  def add[E](e: ExecEffect): State[Trace, Unit, E] = {
    State.modify((t: Trace) => Trace(t.t.appended(e)))
  }
}

case class TraceGen[E]() extends NopEffects[Trace, E] {

  /** Values are discarded by ProductInterpreter so do not matter */
  override def loadMem(v: String, addrs: List[BasilValue]): State[Trace, List[BasilValue], E] = for {
    s <- Trace.add(ExecEffect.LoadMem(v, addrs))
  } yield List()

  override def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation): State[Trace, Unit, E] = for {
    s <- Trace.add(ExecEffect.Call(target, beginFrom, returnTo))
  } yield ()

  override def doReturn(): State[Trace, Unit, E] = for {
    s <- Trace.add(ExecEffect.Return)
  } yield ()

  override def storeVar(v: String, scope: Scope, value: BasilValue): State[Trace, Unit, E] = for {
    s <- if !v.startsWith("ghost") then Trace.add(ExecEffect.StoreVar(v, scope, value)) else State.pure(())
  } yield ()

  override def storeMem(vname: String, update: Map[BasilValue, BasilValue]): State[Trace, Unit, E] = for {
    s <- if !vname.startsWith("ghost") then Trace.add(ExecEffect.StoreMem(vname, update)) else State.pure(())
  } yield ()

}

def tracingInterpreter[I, E](innerInterpreter: Effects[I, E]) = ProductInterpreter(innerInterpreter, TraceGen())

def interpretWithTrace[I](p: Program, innerInterpreter: Effects[I, InterpreterError], innerInitialState: I): (I, Trace) = {
  InterpFuns.interpretProg(tracingInterpreter(innerInterpreter))(p, (innerInitialState, Trace(List())))
}

def interpretWithTrace[I](p: IRContext, innerInterpreter: Effects[I, InterpreterError], innerInitialState: I): (I, Trace) = {
  val tracingInterpreter = ProductInterpreter(innerInterpreter, TraceGen())
  val (begin, _) = InterpFuns.initProgState(tracingInterpreter)(p, (innerInitialState, Trace(List())))
  // throw away initialisation trace
  BASILInterpreter(tracingInterpreter).run((begin, Trace(List())))
}

def interpretTrace(p: Program) = {
  interpretWithTrace(p, NormalInterpreter, InterpreterState())
}

def interpretTrace(p: IRContext) = {
  interpretWithTrace(p, NormalInterpreter, InterpreterState())
}
