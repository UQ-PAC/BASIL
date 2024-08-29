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

enum BreakPointLoc:
  case CMD(c: Command)
  case CMDCond(c: Command, condition: Expr)

case class BreakPointAction(saveState: Boolean = true, stop: Boolean = false, evalExprs: List[Expr] = List(), log: Boolean = false)

case class BreakPoint(name: String = "", location: BreakPointLoc, action: BreakPointAction)

case class RememberBreakpoints[T, I <: Effects[T]](val f: I, val breaks: List[BreakPoint]) extends NopEffects[(T, List[(BreakPoint, Option[T], List[(Expr, Expr)])])] {


  def findBreaks[R](c: Command) : State[(T,R), List[BreakPoint]]  = {
    State.filterM[BreakPoint, (T,R)](b => b.location match {
      case BreakPointLoc.CMD(bc) if (bc == c) => State.pure(true)
      case BreakPointLoc.CMDCond(bc, e) if bc == c => doLeft(Eval.evalBool(f)(e))
      case _ => State.pure(false)
    }, breaks)
  }

  override def interpretOne : State[(T, List[(BreakPoint, Option[T], List[(Expr, Expr)])]), Unit] = for {
    v : ExecutionContinuation <- doLeft(f.getNext)
    n <- v match {
      case Run(s) => for {
        breaks : List[BreakPoint] <- findBreaks(s)
        res <- State.sequence[(T, List[(BreakPoint, Option[T], List[(Expr, Expr)])]), Unit](State.pure(()), 
          breaks.map((breakpoint: BreakPoint) => (breakpoint match {
            case breakpoint @ BreakPoint(name, stopcond, action) => (for {
                saved <- doLeft(if action.saveState then State.getS[T].map(s => Some(s)) else State.pure(None))
                evals <- (State.mapM((e:Expr) => for {
                  ev <- doLeft(Eval.evalExpr(f)(e))
                  } yield (e, ev)
                , action.evalExprs))
                _ <- if action.stop then doLeft(f.setNext(Errored(s"Stopped at breakpoint ${name}"))) else doLeft(State.pure(()))
                _ <- State.pure({
                  if (action.log) {
                    val bpn = breakpoint.name
                    val bpcond = breakpoint.location match {
                      case BreakPointLoc.CMD(c) => c.toString
                      case BreakPointLoc.CMDCond(c, e) => s"$c when $e"
                    }
                    val saving = if action.saveState then " stashing state, " else ""
                    val stopping = if action.stop then " stopping. " else ""
                    val evalstr = evals.map(e => s"\n  eval(${e._1}) = ${e._2}").mkString("")
                    Logger.warn(s"Breakpoint $bpn@$bpcond.$saving$stopping$evalstr")
                  }
                })
                _ <-  State.modify ((istate:(T, List[(BreakPoint, Option[T], List[(Expr, Expr)])])) => 
                    (istate._1, ((breakpoint, saved, evals)::istate._2)))
              } yield ()
              )
            })))
      } yield ()
      case _ => State.pure(())
      }
    } yield ()

}

