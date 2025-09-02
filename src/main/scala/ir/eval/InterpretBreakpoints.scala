package ir.eval

import ir.*
import util.functional.*
import util.{IRContext, Logger}

enum BreakPointLoc:
  case CMD(c: Command)
  case CMDCond(c: Command, condition: Expr)

case class BreakPointAction(
  saveState: Boolean = true,
  stop: Boolean = false,
  evalExprs: List[(String, Expr)] = List(),
  log: Boolean = false
)

case class BreakPoint(name: String = "", location: BreakPointLoc, action: BreakPointAction)

case class RememberBreakpoints[T, I <: Effects[T, InterpreterError]](f: I, breaks: List[BreakPoint])
    extends NopEffects[(T, List[(BreakPoint, Option[T], List[(String, Expr, Option[Expr])])]), InterpreterError] {

  def findBreaks[R](c: Command): State[(T, R), List[BreakPoint], InterpreterError] = {
    State.filterM(
      b =>
        b.location match {
          case BreakPointLoc.CMD(bc) if (bc == c) => State.pure(true)
          case BreakPointLoc.CMDCond(bc, e) if bc == c => doLeft(Eval.evalBool(f)(e))
          case _ => State.pure(false)
        },
      breaks
    )
  }

  override def getNext: State[
    (T, List[(BreakPoint, Option[T], List[(String, Expr, Option[Expr])])]),
    ExecutionContinuation,
    InterpreterError
  ] = {
    for {
      v: ExecutionContinuation <- doLeft(f.getNext)
      n <- v match {
        case Run(s) =>
          for {
            breaks: List[BreakPoint] <- findBreaks(s)
            res <- State
              .sequence[(T, List[(BreakPoint, Option[T], List[(String, Expr, Option[Expr])])]), Unit, InterpreterError](
                State.pure(()),
                breaks.map((breakpoint: BreakPoint) =>
                  (breakpoint match {
                    case breakpoint @ BreakPoint(name, stopcond, action) => (
                      for {
                        saved <- doLeft(
                          if action.saveState then State.getS[T, InterpreterError].map(s => Some(s))
                          else State.pure(None)
                        )
                        evals <- (State.mapM(
                          (e: (String, Expr)) =>
                            for {
                              ev <- doLeft(Eval.evalExpr(f)(e._2)).catchE {
                                case Left(l) => State.pure(None)
                                case Right(l) => State.pure(Some(l))
                              }
                            } yield (e._1, e._2, ev),
                          action.evalExprs
                        ))
                        _ <- State.pure({
                          if (action.log) {
                            val bpn = breakpoint.name
                            val bpcond = breakpoint.location match {
                              case BreakPointLoc.CMD(c) => s"${c.parent.label}:$c"
                              case BreakPointLoc.CMDCond(c, e) => s"${c.parent.label}:$c when $e"
                            }
                            val saving = if action.saveState then " stashing state, " else ""
                            val stopping = if action.stop then " stopping. " else ""
                            val evalstr = evals.map(e => s"\n  ${e._1} : eval(${e._2}) = ${e._3}").mkString("")
                            Logger.warn(s"Breakpoint $bpn@$bpcond.$saving$stopping$evalstr")
                          }
                        })
                        _ <-
                          if action.stop then doLeft(f.setNext(ErrorStop(Errored(s"Stopped at breakpoint ${name}"))))
                          else doLeft(State.pure(()))
                        _ <- State.modify(
                          (istate: (T, List[(BreakPoint, Option[T], List[(String, Expr, Option[Expr])])])) =>
                            (istate._1, ((breakpoint, saved, evals) :: istate._2))
                        )
                      } yield ()
                    )
                  })
                )
              )
          } yield ()
        case _ => State.pure(())
      }
    } yield (v)
  }
}

def interpretWithBreakPoints[I](
  p: IRContext,
  breakpoints: List[BreakPoint],
  innerInterpreter: Effects[I, InterpreterError],
  innerInitialState: I
): (I, List[(BreakPoint, Option[I], List[(String, Expr, Option[Expr])])]) = {
  val interp = LayerInterpreter(innerInterpreter, RememberBreakpoints(innerInterpreter, breakpoints))
  val res = InterpFuns.interpretProg(interp)(p, (innerInitialState, List()))
  res
}

def interpretWithBreakPoints[I](
  p: Program,
  breakpoints: List[BreakPoint],
  innerInterpreter: Effects[I, InterpreterError],
  innerInitialState: I
): (I, List[(BreakPoint, Option[I], List[(String, Expr, Option[Expr])])]) = {
  val interp = LayerInterpreter(innerInterpreter, RememberBreakpoints(innerInterpreter, breakpoints))
  val res = InterpFuns.interpretProg(interp)(p, (innerInitialState, List()))
  res
}

def interpretBreakPoints(p: IRContext, breakpoints: List[BreakPoint]) = {
  interpretWithBreakPoints(p, breakpoints, NormalInterpreter, InterpreterState())
}

def interpretBreakPoints(p: Program, breakpoints: List[BreakPoint]) = {
  interpretWithBreakPoints(p, breakpoints, NormalInterpreter, InterpreterState())
}
