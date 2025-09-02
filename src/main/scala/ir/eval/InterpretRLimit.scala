package ir.eval

import ir.*
import util.IRContext
import util.functional.*

case class EffectsRLimit[T, E, I <: Effects[T, InterpreterError]](val limit: Int)
    extends NopEffects[(T, Int), InterpreterError] {

  override def getNext: State[(T, Int), ExecutionContinuation, InterpreterError] = {
    for {
      c: (T, Int) <- State.getS
      (is, resources) = c
      _ <-
        if (resources >= limit && limit >= 0) {
          State.setError(Errored(s"Resource limit $limit reached"))
        } else {
          State.modify((s: (T, Int)) => (s._1, s._2 + 1))
        }
    } yield (Stopped()) // thrown away by LayerInterpreter
  }
}

def interpretWithRLimit[I](
  p: Program,
  instructionLimit: Int,
  innerInterpreter: Effects[I, InterpreterError],
  innerInitialState: I
): (I, Int) = {
  val rlimitInterpreter = LayerInterpreter(innerInterpreter, EffectsRLimit(instructionLimit))
  InterpFuns.interpretProg(rlimitInterpreter)(p, (innerInitialState, 0))
}

def interpretWithRLimit[I](
  p: IRContext,
  instructionLimit: Int,
  innerInterpreter: Effects[I, InterpreterError],
  innerInitialState: I
): (I, Int) = {
  val rlimitInterpreter = LayerInterpreter(innerInterpreter, EffectsRLimit(instructionLimit))
  val begin = InterpFuns.initProgState(rlimitInterpreter)(p, (innerInitialState, 0))
  // throw away initialisation trace
  InterpFuns.interpretEvalProgSkipInit(rlimitInterpreter)(p.program, (begin._1, 0))._1
}

def interpretRLimit(p: Program, instructionLimit: Int): (InterpreterState, Int) = {
  interpretWithRLimit(p, instructionLimit, NormalInterpreter, InterpreterState())
}

def interpretRLimit(p: IRContext, instructionLimit: Int): (InterpreterState, Int) = {
  interpretWithRLimit(p, instructionLimit, NormalInterpreter, InterpreterState())
}
