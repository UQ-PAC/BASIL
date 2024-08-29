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


enum BreakPointLoc:
  case CMD(c: Command)
  case CMDCond(c: Command, condition: Expr)

case class BreakPointAction(saveState: Boolean = true, stop: Boolean = false, evalExprs: List[Expr] = List(), log: Boolean = false)

case class BreakPoint(name: String = "", location: BreakPointLoc, action: BreakPointAction)

case class RememberBreakpoints[T, E, I <: Effects[T, E]](val f: I, val breaks: List[BreakPoint]) extends NopEffects[(T, List[(BreakPoint, Option[T], List[(Expr, Expr)])]), E] {


  def findBreaks[R](c: Command) : State[(T,R), List[BreakPoint], E]  = {
    State.filterM(b => b.location match {
      case BreakPointLoc.CMD(bc) if (bc == c) => State.pure(true)
      case BreakPointLoc.CMDCond(bc, e) if bc == c => doLeft(Eval.evalBool(f)(e))
      case _ => State.pure(false)
    }, breaks)
  }

  override def interpretOne : State[(T, List[(BreakPoint, Option[T], List[(Expr, Expr)])]), Unit, E] = for {
    v : ExecutionContinuation <- doLeft(f.getNext)
    n <- v match {
      case Run(s) => for {
        breaks : List[BreakPoint] <- findBreaks(s)
        res <- State.sequence[(T, List[(BreakPoint, Option[T], List[(Expr, Expr)])]), Unit, E](State.pure(()), 
          breaks.map((breakpoint: BreakPoint) => (breakpoint match {
            case breakpoint @ BreakPoint(name, stopcond, action) => (for {
                saved <- doLeft(if action.saveState then State.getS[T, E].map(s => Some(s)) else State.pure(None))
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


def interpretWithBreakPoints[I, E](p: Program, breakpoints: List[BreakPoint], innerInterpreter: Effects[I, E], innerInitialState: I) : (I, List[(BreakPoint, Option[I], List[(Expr, Expr)])]) = {
   val interp = LayerInterpreter(innerInterpreter, RememberBreakpoints(innerInterpreter, breakpoints))
   val res = InterpFuns.interpretProg(interp)(p, (innerInitialState, List()))
   res
}
