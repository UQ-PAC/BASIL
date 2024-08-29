package ir.eval
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

/** Interpreter status type, either stopped, run next command or error
  */
sealed trait ExecutionContinuation
case class FailedAssertion(a: Assert) extends ExecutionContinuation


case class Stopped() extends ExecutionContinuation /* normal program stop  */
case class Run(val next: Command) extends ExecutionContinuation /* continue by executing next command */

case class EscapedControlFlow(val call: Jump | Call)
    extends ExecutionContinuation /* controlflow has reached somewhere eunrecoverable */

case class Errored(val message: String = "") extends ExecutionContinuation
case class TypeError(val message: String = "") extends ExecutionContinuation /* type mismatch appeared */
case class EvalError(val message: String = "")
    extends ExecutionContinuation /* failed to evaluate an expression to a concrete value */
case class MemoryError(val message: String = "") extends ExecutionContinuation /* An error to do with memory */


// type InterpreterError = EscapedControlFlow | Errored | TypeError | EvalError | MemoryError

/** TODO: errors should be encapsualted in error monad, rather than mapping exceptions back into state transitions at
  * State.execute() */
case class InterpreterError(continue: ExecutionContinuation) extends Exception()

/* Concrete value type of the interpreter. */
sealed trait BasilValue(val irType: IRType)
case class Scalar(val value: Literal) extends BasilValue(value.getType) {
  override def toString = value match {
    case b: BitVecLiteral => "0x%x:bv%d".format(b.value, b.size)
    case c                => c.toString
  }
}

/** Slightly hacky way of mapping addresses to function calls within the interpreter dynamic state */
case class FunPointer(val addr: BitVecLiteral, val name: String, val call: ExecutionContinuation)
    extends BasilValue(addr.getType)

// Erase the type of basil values and enforce the invariant that
// \exists i . \forall v \in value.keys , v.irType = i  and
// \exists j . \forall v \in value.values, v.irType = j
case class MapValue(val value: Map[BasilValue, BasilValue], override val irType: MapType) extends BasilValue(irType) {
  override def toString = s"MapValue : $irType"
}

case object BasilValue {

  def size(v: IRType): Int = {
    v match {
      case BitVecType(sz) => sz
      case _              => 1
    }
  }

  def size(v: BasilValue): Int = size(v.irType)

  def unsafeAdd(l: BasilValue, vr: Int): BasilValue = {
    l match {
      case _ if vr == 0              => l
      case Scalar(IntLiteral(vl))    => Scalar(IntLiteral(vl + vr))
      case Scalar(b1: BitVecLiteral) => Scalar(eval.evalBVBinExpr(BVADD, b1, BitVecLiteral(vr, b1.size)))
      case _                         => throw InterpreterError(TypeError(s"Operation add $vr undefined on $l"))
    }
  }

  def add(l: BasilValue, r: BasilValue): BasilValue = {
    (l, r) match {
      case (Scalar(IntLiteral(vl)), Scalar(IntLiteral(vr)))       => Scalar(IntLiteral(vl + vr))
      case (Scalar(b1: BitVecLiteral), Scalar(b2: BitVecLiteral)) => Scalar(eval.evalBVBinExpr(BVADD, b1, b2))
      case (Scalar(b1: BoolLit), Scalar(b2: BoolLit)) =>
        Scalar(if (b2.value || b2.value) then TrueLiteral else FalseLiteral)
      case _ => throw InterpreterError(TypeError(s"Operation add undefined on $l $r"))
    }
  }
}

/**
 * Minimal language defining all state transitions in the interpreter, 
 * defined for the interpreter's concrete state T.
 */
trait Effects[T, E] {

  // perform an execution step
  def interpretOne: State[T, Unit, E]

  def loadVar(v: String): State[T, BasilValue, E]

  def loadMem(v: String, addrs: List[BasilValue]): State[T, List[BasilValue], E]

  def evalAddrToProc(addr: Int): State[T, Option[FunPointer], E]

  def getNext: State[T, ExecutionContinuation, E]

  /** state effects */

  /* High-level implementation of a program counter that leverages the intrusive CFG. */
  def setNext(c: ExecutionContinuation): State[T, Unit, E]

  /* Perform a call:
   *  target: arbitrary target name
   *  beginFrom: ExecutionContinuation which begins executing the procedure
   *  returnTo: ExecutionContinuation which begins executing after procedure return
   */
  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation): State[T, Unit, E]

  def doReturn(): State[T, Unit, E]

  def storeVar(v: String, scope: Scope, value: BasilValue): State[T, Unit, E]

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]): State[T, Unit, E]
}



trait NopEffects[T, E] extends Effects[T, E] {
  def interpretOne = State.pure(())
  def loadVar(v: String) = State.pure(Scalar(FalseLiteral))
  def loadMem(v: String, addrs: List[BasilValue])  = State.pure(List())
  def evalAddrToProc(addr: Int) = State.pure(None)
  def getNext = State.pure(Stopped())
  def setNext(c: ExecutionContinuation)  = State.pure(())

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = State.pure(())
  def doReturn() = State.pure(())

  def storeVar(v: String, scope: Scope, value: BasilValue) = State.pure(())
  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = State.pure(())
}

/** -------------------------------------------------------------------------------- 
 * Definition of concrete state
 * -------------------------------------------------------------------------------- */


type StackFrameID = String
val globalFrame: StackFrameID = "GLOBAL"

case class MemoryState(
    /* We have a very permissive value reprsentation and store all dynamic state in `stackFrames`.
     * - activations is the call stack, the top of which indicates the current stackFrame.
     * - activationCount: (procedurename -> int) is used to create uniquely-named stackframes.
     */
    val stackFrames: Map[StackFrameID, Map[String, BasilValue]] = Map((globalFrame -> Map.empty)),
    val activations: List[StackFrameID] = List.empty,
    val activationCount: Map[String, Int] = Map.empty.withDefault(_ => 0)
) {

  /** Debug return useful values * */

  def getGlobalVals: Map[String, BitVecLiteral] = {
    stackFrames(globalFrame).collect { case (k, Scalar(b: BitVecLiteral)) =>
      k -> b
    }
  }

  def getMem(name: String): Map[BitVecLiteral, BitVecLiteral] = {
    stackFrames(globalFrame)(name) match {
      case MapValue(innerMap, MapType(BitVecType(ks), BitVecType(vs))) => {
        def unwrap(v: BasilValue): BitVecLiteral = v match {
          case Scalar(b: BitVecLiteral) => b
          case v => throw Exception(s"Failed to convert map value to bitvector: $v (interpreter type error somewhere)")
        }
        innerMap.map((k, v) => unwrap(k) -> unwrap(v))
      }
      case v => throw Exception(s"$name not a bitvec map variable: ${v.irType}")
    }
  }

  /** Local Variable Stack * */

  def pushStackFrame(function: String): MemoryState = {
    val counts = activationCount + (function -> (activationCount(function) + 1))
    val frameName: StackFrameID = s"AR_${function}_${activationCount(function)}"
    val frames = stackFrames + (frameName -> Map.empty)
    MemoryState(frames, frameName :: activations, counts)
  }

  def popStackFrame(): MemoryState = {
    val (frame, remactivs) = activations match {
      case Nil                          => throw InterpreterError(Errored("No stack frame to pop"))
      case h :: Nil if h == globalFrame => throw InterpreterError(Errored("tried to pop global scope"))
      case h :: tl                      => (h, tl)
    }
    val frames = stackFrames.removed(frame)
    MemoryState(frames, remactivs, activationCount)
  }

  /* Variable retrieval and setting */

  /* Set variable in a given frame */
  def setVar(frame: StackFrameID, varname: String, value: BasilValue): MemoryState = {
    val nv = stackFrames + (frame -> (stackFrames(frame) + (varname -> value)))
    MemoryState(nv, activations, activationCount)
  }

  /* Find variable definition scope and set it in the correct frame  */
  def setVar(v: String, value: BasilValue): MemoryState = {
    val frame = findVarOpt(v).map(_._1).getOrElse(activations.head)
    setVar(frame, v, value)
  }

  /* Define a variable in the scope specified
   * ignoring whether it may already be defined
   */
  def defVar(name: String, s: Scope, value: BasilValue): MemoryState = {
    val frame = s match {
      case Scope.Global => globalFrame
      case _            => activations.head
    }
    setVar(frame, name, value)
  }

  /* Lookup the value of a variable */
  def findVarOpt(name: String): Option[(StackFrameID, BasilValue)] = {
    val searchScopes = globalFrame :: activations.headOption.toList
    searchScopes.foldRight(None: Option[(StackFrameID, BasilValue)])((r, acc) =>
      acc match {
        case None => stackFrames(r).get(name).map(v => (r, v))
        case s    => s
      }
    )
  }

  def findVar(name: String): (StackFrameID, BasilValue) = {
    findVarOpt(name: String).getOrElse(throw InterpreterError(Errored(s"Access to undefined variable $name")))
  }

  def getVarOpt(name: String): Option[BasilValue] = findVarOpt(name).map(_._2)

  def getVar(name: String): BasilValue = {
    getVarOpt(name).getOrElse(throw InterpreterError(Errored(s"Access undefined variable $name")))
  }

  def getVar(v: Variable): BasilValue = {
    val value = getVar(v.name)
    value match {
      case dv: BasilValue if v.getType != dv.irType =>
        throw InterpreterError(
          Errored(s"Type mismatch on variable definition and load: defined ${dv.irType}, variable ${v.getType}")
        )
      case o => o
    }
  }

  def getVarLiteralOpt(v: Variable): Option[Literal] = {
    getVar(v) match {
      case Scalar(v) => Some(v)
      case _         => None
    }
  }

  /* Map variable accessing ; load and store operations */
  def doLoadOpt(vname: String, addr: List[BasilValue]): Option[List[BasilValue]] = {
    val (frame, mem) = findVar(vname)
    val mapv: MapValue = mem match {
      case m @ MapValue(innerMap, ty) => m
      case m                          => throw InterpreterError(TypeError(s"Load from nonmap ${m.irType}"))
    }

    val rs = addr.map(k => mapv.value.get(k))
    if (rs.forall(_.isDefined)) {
      Some(rs.map(_.get))
    } else {
      None
    }
  }
  def doLoad(vname: String, addr: List[BasilValue]): List[BasilValue] = {
    doLoadOpt(vname, addr) match {
      case Some(vs) => vs
      case None => {
        throw InterpreterError(MemoryError(s"Read from uninitialised $vname[${addr.head} .. ${addr.last}]"))
      }
    }
  }

  /** typecheck and some fields of a map variable */
  def doStore(vname: String, values: Map[BasilValue, BasilValue]) = {
    val (frame, mem) = findVar(vname)

    val (mapval, keytype, valtype) = mem match {
      case m @ MapValue(_, MapType(kt, vt)) => (m, kt, vt)
      case v => throw InterpreterError(TypeError(s"Invalid map store operation to $vname : ${v.irType}"))
    }

    (values.find((k, v) => k.irType != keytype || v.irType != valtype)) match {
      case Some(v) =>
        throw InterpreterError(
          TypeError(
            s"Invalid addr or value type (${v._1.irType}, ${v._2.irType}) does not match map type $vname : ($keytype, $valtype)"
          )
        )
      case None => ()
    }

    val nmap = MapValue(mapval.value ++ values, mapval.irType)
    setVar(frame, vname, nmap)
  }
}


case class InterpreterState(
    val nextCmd: ExecutionContinuation = Stopped(),
    val callStack: List[ExecutionContinuation] = List.empty,
    val memoryState: MemoryState = MemoryState()
)

/** Implementation of Effects for InterpreterState concrete state representation.
  */
object NormalInterpreter extends Effects[InterpreterState, InterpreterError] {


  def loadVar(v: String) = {
    State.get((s: InterpreterState) => {
      s.memoryState.getVar(v)
    })
  }

  def evalAddrToProc(addr: Int) =
    Logger.debug(s"    eff : FIND PROC $addr")
    for {
      res <- get((s: InterpreterState) => s.memoryState.doLoadOpt("funtable", List(Scalar(BitVecLiteral(addr, 64)))))
    } yield {
      res match {
        case Some((f: FunPointer) :: Nil) => Some(f)
        case _                            => None
      }
    }

  def formatStore(varname: String, update: Map[BasilValue, BasilValue]) : String = {
    val ks = update.toList.sortWith((x, y) => {
      def conv(v: BasilValue): BigInt = v match {
        case (Scalar(b: BitVecLiteral)) => b.value
        case (Scalar(b: IntLiteral))    => b.value
        case _                          => BigInt(0)
      }
      conv(x._1) <= conv(y._1)
    })

    val rs = ks.foldLeft(Some((None, List[BitVecLiteral]())): Option[(Option[BigInt], List[BitVecLiteral])])((acc, v) =>
      v match {
        case (Scalar(bv: BitVecLiteral), Scalar(bv2: BitVecLiteral)) => {
          acc match {
            case None                                  => None
            case Some(None, l)                         => Some(Some(bv.value), bv2 :: l)
            case Some(Some(v), l) if bv.value == v + 1 => Some(Some(bv.value), bv2 :: l)
            case Some(Some(v), l) => {
              None
            }
          }
        }
        case (bv, bv2) => None
      }
    )

    rs match {
      case Some(_, l) => {
        val vs = Scalar(l.foldLeft(BitVecLiteral(0, 0))((acc, r) => eval.evalBVBinExpr(BVCONCAT, acc, r))).toString
        s"$varname[${ks.head._1}] := $vs"
      }
      case None if ks.length < 8 => s"$varname[${ks.map(_._1).mkString(",")}] := ${ks.map(_._2).mkString(",")}"
      case None => s"$varname[${ks.map(_._1).take(8).mkString(",")}...] := ${ks.map(_._2).take(8).mkString(", ")}... "
    }

  }

  def loadMem(v: String, addrs: List[BasilValue]) = {
    State.get((s: InterpreterState) => {
      val r = s.memoryState.doLoad(v, addrs)
      Logger.debug(s"    eff : LOAD ${addrs.head} x ${addrs.size}")
      r
    })
  }

  def getNext = State.get((s: InterpreterState) => s.nextCmd)

  /** effects * */
  def setNext(c: ExecutionContinuation) = State.modify((s: InterpreterState) => {
    // Logger.debug(s"    eff : setNext $c")
    s.copy(nextCmd = c)
  })

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) =
    modify((s: InterpreterState) => {
      Logger.debug(s"    eff : CALL $target")
      s.copy(
        nextCmd = beginFrom,
        callStack = returnTo :: s.callStack,
        memoryState = s.memoryState.pushStackFrame(target)
      )
    })

  def doReturn() = {
    Logger.debug(s"    eff : RETURN")
    modify((s: InterpreterState) => {
      s.callStack match {
        case Nil     => s.copy(nextCmd = Stopped())
        case h :: tl => s.copy(nextCmd = h, callStack = tl, memoryState = s.memoryState.popStackFrame())
      }
    })
  }

  def storeVar(v: String, scope: Scope, value: BasilValue): State[InterpreterState, Unit, InterpreterError] = {
    Logger.debug(s"    eff : SET $v := $value")
    State.modify((s: InterpreterState) => s.copy(memoryState = s.memoryState.defVar(v, scope, value)))
  }

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = State.modify((s: InterpreterState) => {
    Logger.debug(s"    eff : STORE ${formatStore(vname, update)}")
    s.copy(memoryState = s.memoryState.doStore(vname, update))
  })

  def interpretOne: State[InterpreterState, Unit, InterpreterError] = for {
    next <- getNext
    _ <- try {
      next match {
        case Run(c: Statement) => InterpFuns.interpretStatement(this)(c)
        case Run(c: Jump) => InterpFuns.interpretJump(this)(c)
        case Stopped() => State.pure (()) 
        case errorstop => State.pure (())
      }
    } catch {
      case InterpreterError(e) => setNext(e)
      case e: java.lang.IllegalArgumentException => setNext(Errored(e.getStackTrace.take(5).mkString("\n")))
    }
  } yield ()

}


// def interpretTrace(IRProgram: Program): TracingInterpreter = {
//   val s: TracingInterpreter = InterpFuns.interpretProg(IRProgram, TracingInterpreter(InterpreterState(), List()))
//   s
//e
