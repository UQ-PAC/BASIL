package ir.eval
import ir.eval.BitVectorEval.*
import ir.*
import util.Logger
import util.functional.*
import util.functional.State.*
import boogie.Scope
import scala.collection.WithFilter
import translating.PrettyPrinter.*
import util.RegionTimer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.util.control.Breaks.{break, breakable}

/** Interpreter status type, either stopped, run next command or error
  */
sealed trait ExecutionContinuation
case class Stopped() extends ExecutionContinuation /* normal program stop  */
case class ErrorStop(error: InterpreterError) extends ExecutionContinuation /* program stop in error state */
case class Run(next: Command) extends ExecutionContinuation /* continue by executing next command */
case class ReturnTo(call: DirectCall) extends ExecutionContinuation /* continue by executing next command */
case class Intrinsic(name: String) extends ExecutionContinuation /* a named intrinsic instruction */

sealed trait InterpreterError
case class FailedAssertion(a: Assert) extends InterpreterError
case class EscapedControlFlow(call: Jump | Call)
    extends InterpreterError /* controlflow has reached somewhere eunrecoverable */
case class Errored(message: String = "") extends InterpreterError
case class TypeError(message: String = "") extends InterpreterError /* type mismatch appeared */
case class EvalError(message: String = "")
    extends InterpreterError /* failed to evaluate an expression to a concrete value */
case class MemoryError(message: String = "") extends InterpreterError /* An error to do with memory */

/* Concrete value type of the interpreter. */
sealed trait BasilValue(val irType: Option[IRType])
case class Scalar(value: Literal) extends BasilValue(Some(value.getType)) {
  override def toString = pp_expr(value)
}

/* Abstract callable function address */
case class FunPointer(addr: BitVecLiteral, name: String, call: ExecutionContinuation)
    extends BasilValue(Some(addr.getType))

sealed trait MapValue {
  def value: Map[BasilValue, BasilValue]
}

/* We erase the type of basil values and enforce the invariant that
   \exists i . \forall v \in value.keys , v.irType = i  and
   \exists j . \forall v \in value.values, v.irType = j
 */
case class BasilMapValue(value: Map[BasilValue, BasilValue], mapType: MapType)
    extends MapValue
    with BasilValue(Some(mapType)) {
  override def toString = s"MapValue : $irType"
}

case class GenMapValue(val value: Map[BasilValue, BasilValue]) extends BasilValue(None) with MapValue {
  override def toString = s"GenMapValue : $irType"
}

case class Symbol(val value: String) extends BasilValue(None)

case object BasilValue {

  def size(v: IRType): Int = {
    v match {
      case BitVecType(sz) => sz
      case _ => 1
    }
  }

  def toBV[S, E](l: BasilValue): Either[InterpreterError, BitVecLiteral] = {
    l match {
      case Scalar(b1: BitVecLiteral) => Right(b1)
      case _ => Left((TypeError(s"Not a bitvector add $l")))
    }
  }

  def unsafeAdd[S, E](l: BasilValue, vr: Int): Either[InterpreterError, BasilValue] = {
    l match {
      case _ if vr == 0 => Right(l)
      case Scalar(IntLiteral(vl)) => Right(Scalar(IntLiteral(vl + vr)))
      case Scalar(b1: BitVecLiteral) => Right(Scalar(eval.evalBVBinExpr(BVADD, b1, BitVecLiteral(vr, b1.size))))
      case _ => Left((TypeError(s"Operation add $vr undefined on $l")))
    }
  }

  def add[S, E](l: BasilValue, r: BasilValue): Either[InterpreterError, BasilValue] = {
    (l, r) match {
      case (Scalar(IntLiteral(vl)), Scalar(IntLiteral(vr))) => Right(Scalar(IntLiteral(vl + vr)))
      case (Scalar(b1: BitVecLiteral), Scalar(b2: BitVecLiteral)) => Right(Scalar(eval.evalBVBinExpr(BVADD, b1, b2)))
      case _ => Left((TypeError(s"Operation add undefined  $l + $r")))
    }
  }

}

/** Minimal language defining all state transitions in the interpreter, defined for the interpreter's concrete state T.
  */
trait Effects[T, E] {
  /* expression eval */

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

  def callIntrinsic(name: String, args: List[BasilValue]): State[T, Option[BasilValue], E]

  def doReturn(): State[T, Unit, E]

  def storeVar(v: String, scope: Scope, value: BasilValue): State[T, Unit, E]

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]): State[T, Unit, E]
}

trait NopEffects[T, E] extends Effects[T, E] {
  def loadVar(v: String) = State.pure(Scalar(FalseLiteral))
  def loadMem(v: String, addrs: List[BasilValue]) = State.pure(List())
  def evalAddrToProc(addr: Int) = State.pure(None)
  def getNext = State.pure(Stopped())
  def setNext(c: ExecutionContinuation) = State.pure(())

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = State.pure(())
  def callIntrinsic(name: String, args: List[BasilValue]) = State.pure(None)
  def doReturn() = State.pure(())

  def storeVar(v: String, scope: Scope, value: BasilValue) = State.pure(())
  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = State.pure(())
}

/*--------------------------------------------------------------------------------
 * Definition of concrete state
 *--------------------------------------------------------------------------------*/

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
      case BasilMapValue(innerMap, MapType(BitVecType(ks), BitVecType(vs))) => {
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

  def popStackFrame(): Either[InterpreterError, MemoryState] = {
    val hv = activations match {
      case Nil => Left((Errored("No stack frame to pop")))
      case h :: Nil if h == globalFrame => Left((Errored("tried to pop global scope")))
      case h :: tl => Right((h, tl))
    }
    hv.map((hv) => {
      val (frame, remactivs) = hv
      val frames = stackFrames.removed(frame)
      MemoryState(frames, remactivs, activationCount)
    })
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
      case _ => activations.head
    }
    setVar(frame, name, value)
  }

  /* Lookup the value of a variable */
  def findVarOpt(name: String): Option[(StackFrameID, BasilValue)] = {
    val searchScopes = globalFrame :: activations.headOption.toList
    searchScopes.foldRight(None: Option[(StackFrameID, BasilValue)])((r, acc) =>
      acc match {
        case None => stackFrames(r).get(name).map(v => (r, v))
        case s => s
      }
    )
  }

  def findVar(name: String): Either[InterpreterError, (StackFrameID, BasilValue)] = {
    findVarOpt(name: String)
      .map(Right(_))
      .getOrElse(Left((Errored(s"Access to undefined variable $name"))))
  }

  def getVarOpt(name: String): Option[BasilValue] = findVarOpt(name).map(_._2)

  def getVar(name: String): Either[InterpreterError, BasilValue] = {
    getVarOpt(name).map(Right(_)).getOrElse(Left((Errored(s"Access undefined variable $name"))))
  }

  def getVar(v: Variable): Either[InterpreterError, BasilValue] = {
    val value = getVar(v.name)
    value match {
      case Right(dv: BasilValue) if Some(v.getType) != dv.irType =>
        Left(Errored(s"Type mismatch on variable definition and load: defined ${dv.irType}, variable ${v.getType}"))
      case Right(o) => Right(o)
      case o => o
    }
  }

  /* Map variable accessing ; load and store operations */
  def doLoad(vname: String, addr: List[BasilValue]): Either[InterpreterError, List[BasilValue]] = for {
    v <- findVar(vname)
    mapv: MapValue <- v._2 match {
      case m: MapValue => Right(m)
      case m => Left((TypeError(s"Load from nonmap ${m.irType}")))
    }
    rs: List[Option[BasilValue]] = addr.map(k => mapv.value.get(k))
    xs <-
      (if (rs.forall(_.isDefined)) {
         Right(rs.map(_.get))
       } else {
         Left((MemoryError(s"Read from uninitialised $vname[${addr.head} .. ${addr.last}]")))
       })
  } yield (xs)

  /** typecheck and some fields of a map variable */
  def doStore(vname: String, values: Map[BasilValue, BasilValue]): Either[InterpreterError, MemoryState] = for {

    _ <- if (values.size == 0) then Left(MemoryError("Tried to store size 0")) else Right(())
    v <- findVar(vname)
    (frame, mem) = v
    mapval <- mem match {
      case m @ BasilMapValue(_, MapType(kt, vt)) =>
        for {
          m <- (values.find((k, v) => k.irType != Some(kt) || v.irType != Some(vt))) match {
            case Some(v) =>
              Left(
                TypeError(
                  s"Invalid addr or value type (${v._1.irType}, ${v._2.irType}) does not match map type $vname : ($kt, $vt)"
                )
              )
            case None => Right(m)
          }
          nm = BasilMapValue(m.value ++ values, m.mapType)
        } yield (nm)
      case m @ GenMapValue(_) => {
        Right(GenMapValue(m.value ++ values))
      }
      case v => Left((TypeError(s"Invalid map store operation to $vname : ${v.irType}")))
    }

    ms <- Right(setVar(frame, vname, mapval))
  } yield (ms)
}

object LibcIntrinsic {
  // TODO: make parameter passing work

  /** Part of the intrinsics implementation that lives above the Effects interface (i.e. we are defining the observable
    * part of the intrinsics behaviour)
    */

  def singleArg[S, E, T <: Effects[S, E]](name: String)(s: T): State[S, Unit, E] = for {
    c <- s.loadVar("R0")
    res <- s.callIntrinsic(name, List(c))
    _ <- if res.isDefined then s.storeVar("R0", Scope.Global, res.get) else State.pure(())
    _ <- s.doReturn()
  } yield (())

  def twoArg[S, E, T <: Effects[S, E]](name: String)(s: T): State[S, Unit, E] = for {
    c1 <- s.loadVar("R0")
    c2 <- s.loadVar("R1")
    res <- s.callIntrinsic(name, List(c1, c2))
    _ <- if res.isDefined then s.storeVar("R0", Scope.Global, res.get) else State.pure(())
    _ <- s.doReturn()
  } yield (())

  def calloc[S, T <: Effects[S, InterpreterError]](s: T): State[S, Unit, InterpreterError] = for {
    size <- s.loadVar("R0")
    res <- s.callIntrinsic("malloc", List(size))
    ptr = res.get
    isize <- size match {
      case Scalar(b: BitVecLiteral) => State.pure(b.value * 8)
      case _ => State.setError(Errored("programmer error"))
    }
    cl <- Eval.storeBV(s)("mem", ptr, BitVecLiteral(0, isize.toInt), Endian.LittleEndian)
    _ <- s.doReturn()
  } yield (())

  def intrinsics[S, T <: Effects[S, InterpreterError]] =
    Map[String, T => State[S, Unit, InterpreterError]](
      "putc" -> singleArg("putc"),
      "putchar" -> singleArg("putc"),
      "puts" -> singleArg("puts"),
      "printf" -> singleArg("print"),
      "write" -> twoArg("write"),
      "malloc" -> singleArg("malloc"),
      "__libc_malloc_impl" -> singleArg("malloc"),
      "free" -> singleArg("free"),
      "#free" -> singleArg("free"),
      "calloc" -> calloc
    )

}

object IntrinsicImpl {

  /** state initialisation for file modelling */
  def initFileGhostRegions[S, E, T <: Effects[S, E]](f: T): State[S, Unit, E] = for {
    _ <- f.storeVar("ghost-file-bookkeeping", Scope.Global, GenMapValue(Map.empty))
    _ <- f.storeVar("ghost-fd-mapping", Scope.Global, GenMapValue(Map.empty))
    _ <- f.storeMem("ghost-file-bookkeeping", Map(Symbol("$$filecount") -> Scalar(BitVecLiteral(0, 64))))
    _ <- f.callIntrinsic("fopen", List(Symbol("stderr")))
    _ <- f.callIntrinsic("fopen", List(Symbol("stdout")))
  } yield (())

  /** Intrinsics defined over arbitrary effects
    *
    * We call these from Effects[T, E] rather than the Interpreter so their implementation does not appear in the trace.
    */
  def putc[S, T <: Effects[S, InterpreterError]](
    f: T
  )(arg: BasilValue): State[S, Option[BasilValue], InterpreterError] = {
    for {
      addr <- f.loadMem("ghost-file-bookkeeping", List(Symbol("stdout-ptr")))
      byte <- State.pureE(BasilValue.toBV(arg))
      c <- Eval.evalBV(f)(Extract(8, 0, byte))
      _ <- f.storeMem("stdout", Map(addr.head -> Scalar(c)))
      naddr <- State.pureE(BasilValue.unsafeAdd(addr.head, 1))
      _ <- f.storeMem("ghost-file-bookkeeping", Map(Symbol("stdout-ptr") -> naddr))
    } yield (None)
  }

  def fopen[S, T <: Effects[S, InterpreterError]](
    f: T
  )(file: BasilValue): State[S, Option[BasilValue], InterpreterError] = {
    for {
      fname <- file match {
        case Symbol(name) => State.pure(name)
        case _ => State.setError(Errored("Intrinsic fopen open not given filename"))
      }
      _ <- f.storeVar(fname, Scope.Global, BasilMapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
      filecount <- f.loadMem("ghost-file-bookkeeping", List(Symbol("$$filecount")))
      _ <- f.storeMem("ghost-file-bookkeeping", Map(Symbol(fname + "-ptr") -> Scalar(BitVecLiteral(0, 64))))
      _ <- f.storeMem("ghost-fd-mapping", Map(filecount.head -> Symbol(fname + "-ptr")))
      _ <- f.storeVar("R0", Scope.Global, filecount.head)
      nfilecount <- State.pureE(BasilValue.unsafeAdd(filecount.head, 1))
      _ <- f.storeMem("ghost-file-bookkeeping", Map(Symbol("$$filecount") -> nfilecount))
    } yield (Some(filecount.head))
  }

  def write[S, T <: Effects[S, InterpreterError]](
    f: T
  )(fd: BasilValue, strptr: BasilValue): State[S, Option[BasilValue], InterpreterError] = {
    for {
      str <- Eval.getNullTerminatedString(f)("mem", strptr)
      // TODO: fd mapping in state
      file = fd match {
        case Scalar(BitVecLiteral(1, 64)) => "stdout"
        case Scalar(BitVecLiteral(2, 64)) => "stderr"
        case _ => "unknown"
      }
      baseptr: List[BasilValue] <- f.loadMem("ghost-file-bookkeeping", List(Symbol(s"${file}-ptr")))
      offs: List[BasilValue] <- State.mapM(
        ((i: Int) => State.pureE(BasilValue.unsafeAdd(baseptr.head, i))),
        (0 until (str.size + 1))
      )
      _ <- f.storeMem(file, offs.zip(str.map(Scalar(_))).toMap)
      naddr <- State.pureE(BasilValue.unsafeAdd(baseptr.head, str.size))
      _ <- f.storeMem("ghost-file-bookkeeping", Map(Symbol(s"${file}-ptr") -> naddr))
    } yield (None)
  }

  def print[S, T <: Effects[S, InterpreterError]](
    f: T
  )(strptr: BasilValue): State[S, Option[BasilValue], InterpreterError] = {
    for {
      str <- Eval.getNullTerminatedString(f)("mem", strptr)
      baseptr: List[BasilValue] <- f.loadMem("ghost-file-bookkeeping", List(Symbol("stdout-ptr")))
      offs: List[BasilValue] <- State.mapM(
        ((i: Int) => State.pureE(BasilValue.unsafeAdd(baseptr.head, i))),
        (0 until (str.size + 1))
      )
      _ <- f.storeMem("stdout", offs.zip(str.map(Scalar(_))).toMap)
      naddr <- State.pureE(BasilValue.unsafeAdd(baseptr.head, str.size))
      _ <- f.storeMem("ghost-file-bookkeeping", Map(Symbol("stdout-ptr") -> naddr))
    } yield (None)
  }

  def malloc[S, T <: Effects[S, InterpreterError]](
    f: T
  )(size: BasilValue): State[S, Option[BasilValue], InterpreterError] = {
    for {
      size <- (size match {
        case (x @ Scalar(_: BitVecLiteral)) => State.pure(x)
        case (Scalar(x: IntLiteral)) => State.pure(Scalar(BitVecLiteral(x.value, 64)))
        case _ => State.setError(Errored("illegal prim arg"))
      })
      x <- f.loadVar("ghost_malloc_top")
      x_gap <- State.pureE(BasilValue.unsafeAdd(x, 128)) // put a gap around allocations to catch buffer overflows
      x_end <- State.pureE(BasilValue.add(x_gap, size))
      _ <- f.storeVar("ghost_malloc_top", Scope.Global, x_end)
      _ <- f.storeVar("R0", Scope.Global, x_gap)
    } yield (Some(x_gap))
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

  val tIntrinsic = RegionTimer("intrinsic")
  val tLoadMem = RegionTimer("loadMem")
  val tLoadVar = RegionTimer("loadVar")
  val tStoreMem = RegionTimer("storeMem")
  val tStoreVar = RegionTimer("storeVar")
  val tReturn = RegionTimer("return")
  val tCall = RegionTimer("call")

  def getTimes() = {
    val t = List(tIntrinsic, tLoadMem, tLoadVar, tStoreMem, tStoreVar, tReturn, tCall)
    t.sortBy(_.getTotal())
  }

  def callIntrinsic(
    name: String,
    args: List[BasilValue]
  ): State[InterpreterState, Option[BasilValue], InterpreterError] = {
    name match {
      case "free" => State.pure(None)
      case "malloc" => IntrinsicImpl.malloc(this)(args.head)
      case "fopen" => IntrinsicImpl.fopen(this)(args.head)
      case "putc" => IntrinsicImpl.putc(this)(args.head)
      case "strlen" =>
        for {
          str <- Eval.getNullTerminatedString(this)("mem", args.head)
          r = Scalar(BitVecLiteral(str.length, 64))
          _ <- storeVar("R0", Scope.Global, r)
        } yield (Some(r))
      case "print" => IntrinsicImpl.print(this)(args.head)
      case "puts" =>
        IntrinsicImpl.print(this)(args.head) >> IntrinsicImpl.putc(this)(Scalar(BitVecLiteral('\n'.toInt, 64)))
      case "write" => IntrinsicImpl.write(this)(args(1), args(2))
      case _ => State.setError(Errored(s"Call undefined intrinsic $name"))
    }
  }

  def loadVar(v: String) = {
    State.getE((s: InterpreterState) =>
      tLoadVar.within {
        s.memoryState.getVar(v)
      }
    )
  }

  def evalAddrToProc(addr: Int) =
    // Logger.debug(s"    eff : FIND PROC $addr")
    for {
      res: List[BasilValue] <- getE((s: InterpreterState) =>
        s.memoryState.doLoad("ghost-funtable", List(Scalar(BitVecLiteral(addr, 64))))
      )
    } yield {
      res match {
        case ((f: FunPointer) :: Nil) => Some(f)
        case _ => None
      }
    }

  def formatStore(varname: String, update: Map[BasilValue, BasilValue]): String = {
    val ks = update.toList.sortWith((x, y) => {
      def conv(v: BasilValue): BigInt = v match {
        case (Scalar(b: BitVecLiteral)) => b.value
        case (Scalar(b: IntLiteral)) => b.value
        case _ => BigInt(0)
      }
      conv(x._1) <= conv(y._1)
    })

    val rs = ks.foldLeft(Some((None, List[BitVecLiteral]())): Option[(Option[BigInt], List[BitVecLiteral])])((acc, v) =>
      v match {
        case (Scalar(bv: BitVecLiteral), Scalar(bv2: BitVecLiteral)) => {
          acc match {
            case None => None
            case Some(None, l) => Some(Some(bv.value), bv2 :: l)
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
        s"$varname[${ks.headOption.map(_._1).getOrElse("null")}] := $vs"
      }
      case None if ks.length < 8 => s"$varname[${ks.map(_._1).mkString(",")}] := ${ks.map(_._2).mkString(",")}"
      case None => s"$varname[${ks.map(_._1).take(8).mkString(",")}...] := ${ks.map(_._2).take(8).mkString(", ")}... "
    }

  }

  def loadMem(v: String, addrs: List[BasilValue]) = {
    State.getE((s: InterpreterState) =>
      tLoadMem.within {
        // Logger.debug(s"    eff : LOAD ${addrs.head} x ${addrs.size}")
        s.memoryState.doLoad(v, addrs)
      }
    )
  }

  def getNext = State.get((s: InterpreterState) => s.nextCmd)

  /** effects * */
  def setNext(c: ExecutionContinuation) = State.modify((s: InterpreterState) => {
    s.copy(nextCmd = c)
  })

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) =
    modify((s: InterpreterState) =>
      tCall.within {
        // Logger.debug(s"    eff : CALL $target")
        s.copy(
          nextCmd = beginFrom,
          callStack = returnTo :: s.callStack,
          memoryState = s.memoryState.pushStackFrame(target)
        )
      }
    )

  def doReturn() = {
    // Logger.debug(s"    eff : RETURN")
    modifyE((s: InterpreterState) =>
      tReturn.within {
        s.callStack match {
          case Nil => Right(s.copy(nextCmd = Stopped()))
          case h :: tl =>
            for {
              ms <- s.memoryState.popStackFrame()
            } yield (s.copy(nextCmd = h, callStack = tl, memoryState = ms))
        }
      }
    )
  }

  def storeVar(v: String, scope: Scope, value: BasilValue): State[InterpreterState, Unit, InterpreterError] = {
    // Logger.debug(s"    eff : SET $v := $value")
    State.modify((s: InterpreterState) =>
      tStoreVar.within {
        s.copy(memoryState = s.memoryState.defVar(v, scope, value))
      }
    )
  }

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) =
    State.modifyE((s: InterpreterState) =>
      tStoreMem.within {
        // Logger.debug(s"    eff : STORE ${formatStore(vname, update)}")
        for {
          ms <- s.memoryState.doStore(vname, update)
        } yield (s.copy(memoryState = ms))
      }
    )
}

trait Interpreter[S, E](val f: Effects[S, E]) {

  /*
   * Returns value deciding whether to continue.
   */
  def interpretOne: State[S, Boolean, E]

  @tailrec
  final def run(begin: S): S = {
    val (fs, cont) = interpretOne.f(begin)

    if (cont.contains(true)) then {
      run(fs)
    } else {
      fs
    }
  }
}
