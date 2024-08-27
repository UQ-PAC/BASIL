package ir.eval
import ir.eval.BitVectorEval.*
import ir._
import util.Logger
import boogie.Scope
import scala.collection.WithFilter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.util.control.Breaks.{break, breakable}


case class State[S, +A](f: S => (S, A)) {// extends WithFilter[A, ({ type l[x] = State[S, x] })#l] {

  def unit[A](a: A): State[S, A] = State(s => (s, a))

  def foreach[U](f: A => U): Unit = ()

  // def flatMap[B](f: A => IterableOnce[B]): ir.eval.State[S, B] = ???
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (s2, a) = this.f(s)
    f(a).f(s2)
  })

  def map[B](f: A => B): State[S, B] = {
    State(s => {
      val (s2, a) = this.f(s)
      (s2, f(a))
    })
  }

  def withFilter(q : A => Boolean) = {
    this
  }
}


def stateCompose[S, A, B](s1: State[S, A], s2: State[S, B]) : State[S, B] = {
  State((s: S) => s1.f(s) match {
    case (s, _) => s2.f(s)
  })
}

def pure[S, A](a: A) : State[S, A] = State((s:S) => (s, a))

def sequence[S, V](xs: Iterable[State[S,V]]) : State[S,V] = {
  xs.reduceRight(stateCompose) 
}

def sequence[V](xs: Iterable[Option[V]]) : Option[V] = {
  xs.reduceRight((a, b) => a match {
    case Some(x) => Some(x)
    case None =>  b
  }) 
}

def filterM[A, S](m : (A => State[S, Boolean]), xs: Iterable[A]): State[S, List[A]] = {
  xs.foldRight(pure(List[A]()))((b,acc) => acc.flatMap(c => m(b).map(v => if v then b::c else c)))
}


def get[S,A](f: S => A) : State[S, A] = State(s => (s, f(s)))
def modify[S](f: S => S) : State[S, Unit] = State(s => (f(s), ()))
def execute[S, A](s: S, c: State[S,A]) = c.f(s)

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

case class InterpreterError(continue: ExecutionContinuation) extends Exception()

case class InterpreterSummary(
    val exitState: ExecutionContinuation,
    val regs: Map[Variable, BitVecLiteral],
    val memory: Map[Int, BitVecLiteral]
)

sealed trait BasilValue(val irType: IRType)
case class Scalar(val value: Literal) extends BasilValue(value.getType) {
  override def toString = value match {
    case b: BitVecLiteral => "0x%x:bv%d".format(b.value, b.size)
    case c                => c.toString
  }
}
// Erase the type of basil values and enforce the invariant that
// \exists i . \forall v \in value.keys , v.irType = i  and
// \exists j . \forall v \in value.values, v.irType = j
case class MapValue(val value: Map[BasilValue, BasilValue], override val irType: MapType) extends BasilValue(irType) {
  override def toString = s"MapValue : $irType"
}

case object BasilValue:

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

  def concat(l: BasilValue, r: BasilValue): BasilValue = {
    (l, r) match {
      case (Scalar(b1: BitVecLiteral), Scalar(b2: BitVecLiteral)) => Scalar(eval.evalBVBinExpr(BVCONCAT, b1, b2))
      case _ => throw InterpreterError(TypeError(s"Operation concat undefined on $l $r"))
    }
  }

  def extract(l: BasilValue, high: Int, low: Int): BasilValue = {
    (l) match {
      case Scalar(b: BitVecLiteral) => Scalar(eval.BitVectorEval.boogie_extract(high, low, b))
      case _ => throw InterpreterError(TypeError(s"Operation extract($high, $low) undefined on $l"))
    }
  }

  def fromIR(e: Expr) = {
    e match {
      case t: IntLiteral    => Scalar(t)
      case v: BitVecLiteral => Scalar(v)
      case b: BoolLit       => Scalar(b)
      case _                => throw InterpreterError(EvalError(s"Failed to get value from non-literal expr $e"))

    }
  }

export BasilValue._


sealed trait Effects[T] {
  /* evaluation (may side-effect via InterpreterException on evaluation failure) */
  def evalBV(e: Expr): State[T, BitVecLiteral]

  def evalInt(e: Expr): State[T, BigInt]

  def evalBool(e: Expr): State[T, Boolean]

  def loadVar(v: String): State[T, BasilValue]

  def loadMem(v: String, addrs: List[BasilValue]): State[T, List[BasilValue]]

  def getNext: State[T, ExecutionContinuation]

  /** effects * */
  def setNext(c: ExecutionContinuation): State[T, Unit]

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation): State[T, Unit]

  def doReturn(): State[T, Unit]

  def storeVar(v: String, scope: Scope, value: BasilValue): State[T, Unit]

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]): State[T, Unit]
}

 

def evalToConst(
    to: IRType,
    exp: Expr,
    variable: Variable => Option[Expr],
    load: (Memory, Expr, Endian, Int) => Option[Literal]
): BasilValue = {

  val res: Expr = ir.eval.partialEvalExpr(exp, variable, load)
  res match {
    case e: Literal if e.getType == to => Scalar(e)
    case res => throw InterpreterError(EvalError(s"Failed to evaluate expr to constant ${to} literal: residual $res"))
  }
}

// case class BasilConstant(val basilType: BasilValue, val value: basilType.ReprType)

type StackFrameID = String
val globalFrame: StackFrameID = "GLOBAL"

case class MemoryState(
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
  def doLoad(vname: String, addr: List[BasilValue]): List[BasilValue] = {
    val (frame, mem) = findVar(vname)
    val mapv: MapValue = mem match {
      case m @ MapValue(innerMap, ty) => m
      case m                          => throw InterpreterError(TypeError(s"Load from nonmap ${m.irType}"))
    }

    addr.map(k =>
      mapv.value.get(k).getOrElse(throw InterpreterError(MemoryError(s"Read from uninitialised $vname[$k]")))
    )
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

case class StVarLoader[S, F <: Effects[S]](f : F) extends Loader[S] {

  /** Load helpers * */
  def load(vname: String, addr: Scalar, endian: Endian, count: Int): State[S, List[BasilValue]] = {
    if (count == 0) {
      throw InterpreterError(Errored(s"Attempted fractional load"))
    }
    val keys = (0 until count).map(i => BasilValue.unsafeAdd(addr, i))
    for {
      values <- f.loadMem(vname, keys.toList)
      vals = endian match {
        case Endian.LittleEndian => values.reverse
        case Endian.BigEndian    => values
      }
    }
    yield (vals.toList)
  }


  /** Load and concat bitvectors */
  def loadBV( vname: String, addr: Scalar, endian: Endian, size: Int): State[S, BitVecLiteral] = for {
      mem <- f.loadVar(vname)
      (valsize, mapv) = mem match {
        case mapv @ MapValue(_, MapType(_, BitVecType(sz))) => (sz, mapv)
        case _ => throw InterpreterError(Errored("Trued to load-concat non bv"))
      }

      cells = size / valsize

      res <- load(vname, addr, endian, cells)
      bvs: List[BitVecLiteral] = {
        val rr = res.map {
          case Scalar(bv @ BitVecLiteral(v, sz)) if sz == valsize => bv
          case c =>
            throw InterpreterError(TypeError(s"Loaded value of type ${c.irType} did not match expected type bv$valsize"))
        }
        rr
      }

      bvres = bvs.foldLeft(BitVecLiteral(0, 0))((acc, r) => eval.evalBVBinExpr(BVCONCAT, acc, r))
      _ = {assert(bvres.size == size)}
    } yield(bvres)

  def loadSingle(vname: String, addr: Scalar): State[S, BasilValue] = {
    for {
      m <- load(vname, addr, Endian.LittleEndian, 1)
    } yield (m.head)
  }

  def getVariable(v: Variable) : State[S, Option[Expr]] = {
    for {
      v <- f.loadVar(v.name)
    } yield (
    v match {
    case Scalar(l) => Some(l)
    case _         => None
    })
  }

  override def loadMemory(m: Memory, addr: Expr, endian: Endian, size: Int) : State[S, Option[Literal]] = {
    for {
      r <-  addr match {
        case l: Literal if size== 1 => loadSingle(m.name, Scalar(l)).map((v : BasilValue) => v match {
          case Scalar(l) => Some(l)
          case _ => None
        })
        case l: Literal => loadBV(m.name, Scalar(l), endian, size).map(Some(_))
        case _          => get((s:S) => None)
    }
    } yield (r)
  }

}


case object Eval {
  //def getVar[S, F <: Effects[S]](f: F)(s: S)(v: Variable): Option[Literal] = 
  //  f.loadVar(v.name).f(s) match {
  //  case Scalar(l) => Some(l)
  //  case _         => None
  //}

  //def doLoad[S, T <: Effects[S]](f: T)(s: S)(m: Memory, addr: Expr, endian: Endian, sz: Int): Option[Literal] = {
  //  addr match {
  //    case l: Literal if sz == 1 => (
  //      loadSingle(f)(s)(m.name, Scalar(l)) match {
  //        case Scalar(v) => Some(v)
  //        case _         => None
  //      }
  //    )
  //    case l: Literal => Some(loadBV(f)(s)(m.name, Scalar(l), endian, sz))
  //    case _          => None
  //  }
  //}

  def evalBV[S, T <: Effects[S]](f: T)(e: Expr): State[S, BitVecLiteral] = {
    val ldr = StVarLoader[S, T](f)
    for {
      res <- ir.eval.statePartialEvalExpr[S](ldr)(e)
    } yield (
      e match {
        case l: BitVecLiteral => l
        case _  => throw InterpreterError(Errored(s"Eval BV residual $e"))
      })
  }

  def evalInt[S, T <: Effects[S]](f: T)(e: Expr): State[S, BigInt] = {
    val ldr = StVarLoader[S, T](f)
    for {
      res <- ir.eval.statePartialEvalExpr[S](ldr)(e)
    } yield (
      e match {
        case l: IntLiteral => l.value
        case _  => throw InterpreterError(Errored(s"Eval BV residual $e"))
      })
  }

  def evalBool[S, T <: Effects[S]](f: T)(e: Expr): State[S, Boolean] = {
    val ldr = StVarLoader[S, T](f)
    for {
      res <- ir.eval.statePartialEvalExpr[S](ldr)(e)
    } yield (
      e match {
        case l: BoolLit => l == TrueLiteral
        case _  => throw InterpreterError(Errored(s"Eval BV residual $e"))
      })
  }



  /** State modifying helpers, e.g. store
    */

  /* Expand addr for number of values to store */
  def store[S, T <: Effects[S]](f: T)(
      vname: String,
      addr: BasilValue,
      values: List[BasilValue],
      endian: Endian
  ): State[S, Unit] = for {
      mem <- f.loadVar(vname)
      (mapval, keytype, valtype) = mem match {
        case m @ MapValue(_, MapType(kt, vt)) if kt == addr.irType && values.forall(v => v.irType == vt) => (m, kt, vt)
        case v => throw InterpreterError(TypeError(s"Invalid map store operation to $vname : $v"))
      }
      keys = (0 until values.size).map(i => BasilValue.unsafeAdd(addr, i))
      vals = endian match {
        case Endian.LittleEndian => values.reverse
        case Endian.BigEndian    => values
      }
      x <- f.storeMem(vname, keys.zip(vals).toMap)
    } yield (x)


  /** Extract bitvec to bytes and store bytes */
  def storeBV[S, T <: Effects[S]](f: T)(
      vname: String,
      addr: BasilValue,
      value: BitVecLiteral,
      endian: Endian
  ): State[S, Unit] = for {
    mem <- f.loadVar(vname)
    (mapval, vsize) = mem match {
      case m @ MapValue(_, MapType(kt, BitVecType(size))) if kt == addr.irType => (m, size)
      case v =>
        throw InterpreterError(
          TypeError(
            s"Invalid map store operation to $vname : ${v.irType} (expect [${addr.irType}] <- ${value.getType})"
          )
        )
    }
    cells = value.size / vsize
    _ = {
    if (cells < 1) {
      throw InterpreterError(MemoryError("Tried to execute fractional store"))
    }}

    extractVals = (0 until cells).map(i => BitVectorEval.boogie_extract((i + 1) * vsize, i * vsize, value)).toList
    vs = endian match {
      case Endian.LittleEndian => extractVals.map(Scalar(_))
      case Endian.BigEndian    => extractVals.reverse.map(Scalar(_))
    }

    keys = (0 until cells).map(i => BasilValue.unsafeAdd(addr, i))
  } yield (f.storeMem(vname, keys.zip(vs).toMap))

  def storeSingle[S, T <: Effects[S]](f: T)(vname: String, addr: BasilValue, value: BasilValue): State[S, Unit] = {
    f.storeMem(vname, Map((addr -> value)))
  }
}


enum Effect:
  case Call(target: String, begin: ExecutionContinuation, returnTo: ExecutionContinuation)
  case SetNext(c: ExecutionContinuation)
  case Return
  case StoreVar(v: String, s: Scope, value: BasilValue)
  case StoreMem(vname: String, update: Map[BasilValue, BasilValue])


// case class TracingInterpreter(
//     val s: InterpreterState,
//     val trace: List[Effect]
// ) extends Effects[TracingInterpreter] {
// 
//   def evalBV(e: Expr) = Eval.evalBV(this)(e)
//   def evalInt(e: Expr) = Eval.evalInt(this)(e)
//   def evalBool(e: Expr) = Eval.evalBool(this)(e)
// 
//   def loadVar(v: String) = 
//   def loadMem(v: String, addrs: List[BasilValue]) = s.loadMem(v, addrs)
// 
//   /** effects * */
//   def setNext(c: ExecutionContinuation) = {
//     // Logger.debug(s"    eff : DONEXT $c")
//     TracingInterpreter(s.setNext(c), trace)
//   }
// 
//   def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = {
//     //Logger.debug(s"    eff : CALL $target")
//     TracingInterpreter(s.call(target, beginFrom, returnTo), Effect.Call(target, beginFrom, returnTo) :: trace)
//   }
// 
//   def doReturn() = {
//     //Logger.debug(s"    eff : RETURN")
//     TracingInterpreter(s.doReturn(), Effect.Return :: trace)
//   }
// 
//   def storeVar(v: String, c: Scope, value: BasilValue) = {
//     //Logger.debug(s"    eff : SET $v := $value")
//     TracingInterpreter(s.storeVar(v, c, value), Effect.StoreVar(v, c, value) :: trace)
//   }
// 
//   def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = {
//     //Logger.debug(s"    eff : STORE $vname <- $update")
//     TracingInterpreter(s.storeMem(vname, update), Effect.StoreMem(vname, update) :: trace)
//   }
// 
//   def getNext = s.getNext
// 
// }

case class InterpreterState(
    val nextCmd: ExecutionContinuation = Stopped(),
    val callStack: List[ExecutionContinuation] = List.empty,
    val memoryState: MemoryState = MemoryState()
)

case class NormalInterpreter() extends Effects[InterpreterState] {

  /* eval */
  def evalBV(e: Expr) = Eval.evalBV(this)(e)

  def evalInt(e: Expr) = Eval.evalInt(this)(e)

  def evalBool(e: Expr) = Eval.evalBool(this)(e)

  def loadVar(v: String) = get((s: InterpreterState) => s.memoryState.getVar(v))

  def loadMem(v: String, addrs: List[BasilValue]) = get((s: InterpreterState) => s.memoryState.doLoad(v, addrs))

  def getNext = get ((s: InterpreterState) => s.nextCmd)

  /** effects * */
  def setNext(c: ExecutionContinuation) = modify ((s: InterpreterState) => {
    InterpreterState(c, s.callStack, s.memoryState)
  })

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation) = modify ((s:InterpreterState) => {
    Logger.info(s"    eff : CALL $target")
    InterpreterState(
      beginFrom,
      returnTo :: s.callStack,
      s.memoryState.pushStackFrame(target)
    )
  })

  def doReturn() = {
    modify ((s: InterpreterState) => {s.callStack match {
      case Nil => InterpreterState(Stopped(), Nil, s.memoryState)
      case h :: tl => {
        InterpreterState(h, tl, s.memoryState.popStackFrame())
      }
    }
    })
  }

  def storeVar(v: String, scope: Scope, value: BasilValue) = {
    Logger.debug(s"    eff : SET $v := $value")
    modify ((s: InterpreterState) => InterpreterState(s.nextCmd, s.callStack, s.memoryState.defVar(v, scope, value)))
  }

  def storeMem(vname: String, update: Map[BasilValue, BasilValue]) = modify ((s:InterpreterState) => {
    Logger.debug(s"    eff : STORE $vname <- $update")
    InterpreterState(s.nextCmd, s.callStack, s.memoryState.doStore(vname, update))
  })

}

case object InterpFuns {

  def initialState[S, T <: Effects[S]](s: T): State[S, Unit] = {
    val SP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
    val FP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
    val LR: BitVecLiteral = BitVecLiteral(BigInt("FF", 16), 64)

    for {
      l <- s.storeVar("R30", Scope.Global, Scalar(LR))
      h <- s.storeVar("mem", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
      i <- s.storeVar("stack", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
      j <- s.storeVar("R31", Scope.Global, Scalar(SP))
      k <- s.storeVar("R29", Scope.Global, Scalar(FP))
    } yield (l)
    // s.storeVar("mem", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
  }

  def initialiseProgram[S, T <: Effects[S]](f:T)(p: Program): State[S, Unit] = {
    for {
      d <- initialState(f)
      mem <- sequence(p.initialMemory.map(memory => 
          Eval.store(f)(
          "mem",
          Scalar(BitVecLiteral(memory.address, 64)),
          memory.bytes.toList.map(Scalar(_)),
          Endian.LittleEndian)))
      mem <- sequence(p.initialMemory.map(memory => 
          Eval.store(f)(
          "stack",
          Scalar(BitVecLiteral(memory.address, 64)),
          memory.bytes.toList.map(Scalar(_)),
          Endian.LittleEndian)))
      r <- f.call(p.mainProcedure.name, Run(IRWalk.firstInBlock(p.mainProcedure.entryBlock.get)), Stopped())
    } yield (r)
  }

  def interpretJump[S, T <: Effects[S]](f: T)(j: Jump): State[S, Unit] = {
    j match {
      case gt: GoTo if gt.targets.size == 1 => {
        f.setNext(Run(IRWalk.firstInBlock(gt.targets.head)))
      }
      case gt: GoTo =>
        val assumes = gt.targets.flatMap(_.statements.headOption).collect {
            case a: Assume => a
          }
        if (assumes.size != gt.targets.size) {
          throw InterpreterError(Errored(s"Some goto target missing guard $gt"))
        }
        for {
          chosen : List[Assume] <- filterM((a:Assume) => f.evalBool(a.body), assumes)

          res <- chosen match {
            case Nil      => f.setNext(Errored(s"No jump target satisfied $gt"))
            case h :: Nil => f.setNext(Run(h))
            case h :: tl  => f.setNext(Errored(s"More than one jump guard satisfied $gt"))
          }
        } yield (res)
      case r: Return      => f.doReturn()
      case h: Unreachable => f.setNext(EscapedControlFlow(h))
    }
  }

  def interpretStatement[S, T <: Effects[S]](f: T)(s: Statement): State[S, Unit] = {
    s match {
      case assign: Assign => {
        for {
          rhs <- f.evalBV(assign.rhs)
          st <- f.storeVar(assign.lhs.name, assign.lhs.toBoogie.scope, Scalar(rhs))
          n <- f.setNext(Run(s.successor))
        } yield (st)
      }
      case assign: MemoryAssign => for {
        index : BitVecLiteral <- f.evalBV(assign.index)
        value : BitVecLiteral <- f.evalBV(assign.value)
        // st.storeMem(assign.mem.name, index, value, assign.endian, assign.size).setNext(Run(s.successor))
        _ <- Eval.storeBV(f)(assign.mem.name, Scalar(index), value, assign.endian)
        n <- f.setNext(Run(s.successor))
      } yield (n)
      case assert: Assert => for {
        b <- f.evalBool(assert.body) 
        n <- (if (!b) then {
          f.setNext(FailedAssertion(assert))
        } else {
          f.setNext(Run(s.successor))
        }) 
      } yield (n)
      case assume: Assume => for {
        b <- f.evalBool(assume.body) 
        n <- (if (!b) {
          f.setNext(Errored(s"Assumption not satisfied: $assume"))
        } else {
          f.setNext(Run(s.successor))
        })
      } yield (n)
      case dc: DirectCall => for {
        n <- if (dc.target.entryBlock.isDefined) {
          val block = dc.target.entryBlock.get
          f.call(dc.target.name, Run(block.statements.headOption.getOrElse(block.jump)), Run(dc.successor))
        } else {
          f.setNext(Run(dc.successor))
        }
      } yield (n)
      case ic: IndirectCall => for {
        n <- (if (ic.target == Register("R30", 64)) {
          f.doReturn()
        } else {
          f.setNext(EscapedControlFlow(ic))
        })
      } yield (n)
      case _: NOP => f.setNext(Run(s.successor))
    }
  }

  def protect[T](x: () => T, fnly: PartialFunction[Exception, T]): T = {
    try {
      x()
    } catch {
      case e: Exception if fnly.isDefinedAt(e) => fnly(e)
    }
  }

  def interpret[S, T <: Effects[S]](f: T, m: State[S, Unit]): State[S, Unit] = {
    for {
      n <- f.getNext 
      _ = {
        Logger.debug(s"interpret ${n}")
      }
      c <- n match {
        case Run(c: Statement) => interpret(f, interpretStatement(f)(c))
        case Run(c: Jump) => interpret(f, interpretJump(f)(c))
        case Stopped() => State((s:S) => (s,()))
        case errorstop => State((s:S) => (s,()))
      }
    } yield(c)
  }

  def interpretProg[S, T <: Effects[S]](f: T)(p: Program, is: S): S = {
    val (fs: S,_) = execute[S, Unit](is, interpret(f, initialiseProgram(f)(p)))
    fs
  }

}

def interpret(IRProgram: Program): InterpreterState = {
  InterpFuns.interpretProg(NormalInterpreter())(IRProgram, InterpreterState())
}

// def interpretTrace(IRProgram: Program): TracingInterpreter = {
//   val s: TracingInterpreter = InterpFuns.interpretProg(IRProgram, TracingInterpreter(InterpreterState(), List()))
//   s
// }
