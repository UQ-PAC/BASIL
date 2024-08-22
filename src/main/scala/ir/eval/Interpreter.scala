package ir.eval
import ir.eval.BitVectorEval.*
import ir._
import util.Logger
import boogie.Scope

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.util.control.Breaks.{break, breakable}

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

enum BasilValue(val irType: IRType):
  case Scalar(val value: Literal) extends BasilValue(value.getType)
  // Erase the type of basil values and enforce the invariant that
  // \exists i . \forall v \in value.keys , v.irType = i  and
  // \exists j . \forall v \in value.values, v.irType = j
  case MapValue(val value: Map[BasilValue, BasilValue], override val irType: MapType) extends BasilValue(irType)

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
  def defVar(name: String, s: Scope , value: BasilValue): MemoryState = {
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

  /* canonical load operation */
  def load(vname: String, addr: Scalar, endian: Endian, count: Int): List[BasilValue] = {
    val (frame, mem) = findVar(vname)

    val mapv: MapValue = mem match {
      case m @ MapValue(innerMap, ty) => m
      case _                          => throw InterpreterError(Errored("Load from nonmap"))
    }

    if (count == 0) {
      throw InterpreterError(Errored(s"Attempted fractional load"))
    }

    val keys = (0 until count).map(i => BasilValue.unsafeAdd(addr, i))

    val values = keys.map(k =>
      mapv.value.get(k).getOrElse(throw InterpreterError(MemoryError(s"Read from uninitialised $vname[$k]")))
    )

    val vals = endian match {
      case Endian.LittleEndian => values.reverse
      case Endian.BigEndian    => values
    }

    vals.toList
  }

  /** Load and concat bitvectors */
  def loadBV(vname: String, addr: Scalar, endian: Endian, size: Int): BitVecLiteral = {
    val (frame, mem) = findVar(vname)

    val (valsize, mapv) = mem match {
      case mapv @ MapValue(_, MapType(_, BitVecType(sz))) => (sz, mapv)
      case _ => throw InterpreterError(Errored("Trued to load-concat non bv"))
    }

    val cells = size / BasilValue.size(mapv.irType.result)

    val bvs: List[BitVecLiteral] = {
      val res = load(vname, addr, endian, cells)
      val rr = res.map {
        case Scalar(bv @ BitVecLiteral(v, sz)) if sz == valsize => bv
        case _ => throw InterpreterError(Errored(s"Loaded value that did not match expected type bv$valsize"))
      }
      rr
    }

    val bvres = bvs.foldLeft(BitVecLiteral(0, 0))((acc, r) => eval.evalBVBinExpr(BVCONCAT, acc, r))
    assert(bvres.size == size)
    bvres
  }

  def loadSingle(vname: String, addr: Scalar): BasilValue = {
    load(vname, addr, Endian.LittleEndian, 1).head
  }

  /** Canonical store operation */
  def store(vname: String, addr: BasilValue, values: List[BasilValue], endian: Endian) = {
    val (frame, mem) = findVar(vname)

    val (mapval, keytype, valtype) = mem match {
      case m @ MapValue(_, MapType(kt, vt)) if kt == addr.irType && values.forall(v => v.irType == vt) => (m, kt, vt)
      case v => throw InterpreterError(TypeError(s"Invalid map store operation to $vname : ${v.irType}"))
    }
    val keys = (0 until values.size).map(i => BasilValue.unsafeAdd(addr, i))
    val vals = endian match {
      case Endian.LittleEndian => values.reverse
      case Endian.BigEndian    => values
    }

    val nmap = MapValue(mapval.value ++ keys.zip(vals), mapval.irType)
    setVar(frame, vname, nmap)
  }

  /** Extract bitvec to bytes and store bytes */
  def storeBV(vname: String, addr: BasilValue, value: BitVecLiteral, endian: Endian) = {
    val (frame, mem) = findVar(vname)
    val (mapval, vsize) = mem match {
      case m @ MapValue(_, MapType(kt, BitVecType(size))) if kt == addr.irType => (m, size)
      case v =>
        throw InterpreterError(
          TypeError(
            s"Invalid map store operation to $vname : ${v.irType} (expect [${addr.irType}] <- ${value.getType})"
          )
        )
    }
    val cells = value.size / vsize
    if (cells < 1) {
      throw InterpreterError(MemoryError("Tried to execute fractional store"))
    }

    val extractVals = (0 until cells).map(i => BitVectorEval.boogie_extract((i + 1) * vsize, i * vsize, value)).toList

    val vs = endian match {
      case Endian.LittleEndian => extractVals.reverse.map(Scalar(_))
      case Endian.BigEndian    => extractVals.map(Scalar(_))
    }

    store(vname, addr, vs, endian)
  }

  def storeSingle(vname: String, addr: BasilValue, value: BasilValue) = {
    store(vname, addr, List(value), Endian.LittleEndian)
  }

}

case object Eval {

  def getVar(s: MemoryState)(v: Variable) = s.getVarLiteralOpt(v)

  def doLoad(s: MemoryState)(m: Memory, addr: Expr, endian: Endian, sz: Int): Option[Literal] = {
    addr match {
      case l: Literal if sz == 1 => (
        s.loadSingle(m.name, Scalar(l)) match {
          case Scalar(v) => Some(v)
          case _         => None
        }
      )
      case l: Literal => Some(s.loadBV(m.name, Scalar(l), endian, sz))
      case _          => None
    }
  }

  def evalBV(s: MemoryState, e: Expr): BitVecLiteral = {
    ir.eval.evalBVExpr(e, Eval.getVar(s), Eval.doLoad(s)) match {
      case Right(e) => e
      case Left(e)  => throw InterpreterError(Errored(s"Eval BV residual $e"))
    }
  }

  def evalInt(s: MemoryState, e: Expr): BigInt = {
    ir.eval.evalIntExpr(e, Eval.getVar(s), Eval.doLoad(s)) match {
      case Right(e) => e
      case Left(e)  => throw InterpreterError(Errored(s"Eval int residual $e"))
    }
  }

  def evalBool(s: MemoryState, e: Expr): Boolean = {
    ir.eval.evalLogExpr(e, Eval.getVar(s), Eval.doLoad(s)) match {
      case Right(e) => e
      case Left(e)  => throw InterpreterError(Errored(s"Eval bool residual $e"))
    }
  }

}


sealed trait Effects[T <: Effects[T]] {
  /* evaluation (may side-effect via InterpreterException on evaluation failure) */
  def evalBV(e: Expr): BitVecLiteral

  def evalInt(e: Expr): BigInt

  def evalBool(e: Expr): Boolean

  /** effects * */
  def setNext(c: ExecutionContinuation): T

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation): T

  def doReturn(): T

  def storeVar(v: String, scope: Scope, value: BasilValue): T

  def storeMem(vname: String, addr: BitVecLiteral, value: BitVecLiteral, endian: Endian, size: Int): T

  def storeMultiMem(vname: String, addr: Literal, value: List[Literal], endian: Endian): T
}



// enum Effect:
//   case Call(c: DirectCall)
//   case SetNext(c: ExecutionContinuation)
//   case Return
//   case StoreVar(v: Variable, value: Literal)
//   case StoreMem(vname: String, addr: BitVecLiteral, value: BitVecLiteral, endian: Endian, size: Int)
//   case StoreMultiMem(vname: String, addr: Literal, value: List[Literal], endian: Endian)
// 
// case class TracingInterpreter(
//     val s: InterpreterState,
//     val trace: List[Effect]
// ) extends Effects[TracingInterpreter] {
// 
// 
//   def evalBV(e: Expr): BitVecLiteral = s.evalBV(e)
//   def evalInt(e: Expr): BigInt = s.evalInt(e)
//   def evalBool(e: Expr): Boolean = s.evalBool(e)
// 
//   /** effects * */
//   def setNext(c: ExecutionContinuation) = {
//     TracingInterpreter(s.setNext(c), Effect.SetNext(c)::trace)
//   }
// 
//   def call(c: DirectCall) = {
//     TracingInterpreter(s.call(c), Effect.Call(c)::trace)
//   }
// 
//   def doReturn() = {
//     TracingInterpreter(s.doReturn(), Effect.Return::trace)
//   }
// 
//   def storeVar(v: Variable, value: Literal) = {
//     TracingInterpreter(s.storeVar(v, value), Effect.StoreVar(v,value)::trace)
//   }
// 
//   def storeMem(vname: String, addr: BitVecLiteral, value: BitVecLiteral, endian: Endian, size: Int) = {
//     TracingInterpreter(s.storeMem(vname, addr, value, endian, size), Effect.StoreMem(vname,addr,value,endian,size)::trace)
//   }
// 
//   def storeMultiMem(vname: String, addr: Literal, value: List[Literal], endian: Endian) = {
//     TracingInterpreter(s.storeMultiMem(vname, addr, value, endian), Effect.StoreMultiMem(vname,addr,value,endian)::trace)
//   }
// 
// }

case class InterpreterState(
    val nextCmd: ExecutionContinuation = Stopped(),
    val callStack: List[ExecutionContinuation] = List.empty,
    val memoryState: MemoryState = MemoryState()
) extends Effects[InterpreterState] {

  /** eval * */
  def evalBV(e: Expr): BitVecLiteral = Eval.evalBV(memoryState, e)

  def evalInt(e: Expr): BigInt = Eval.evalInt(memoryState, e)

  def evalBool(e: Expr): Boolean = Eval.evalBool(memoryState, e)

  /** effects * */
  def setNext(c: ExecutionContinuation): InterpreterState = {
    InterpreterState(c, callStack, memoryState)
  }

  def call(target: String, beginFrom: ExecutionContinuation, returnTo: ExecutionContinuation): InterpreterState = {
    Logger.debug(s"    eff : CALL $target")
    InterpreterState(
      beginFrom,
      returnTo :: callStack,
      memoryState.pushStackFrame(target)
    )
  }

  def doReturn(): InterpreterState = {
    callStack match {
      case Nil => InterpreterState(Stopped(), Nil, memoryState)
      case h :: tl => {
        Logger.debug(s"    eff : RETURN $h")
        InterpreterState(h, tl, memoryState.popStackFrame())
      }
    }
  }

  def storeVar(v: String, scope: Scope, value: BasilValue)  = {
    Logger.debug(s"    eff : SET $v := $value")
    InterpreterState(nextCmd, callStack, memoryState.defVar(v, scope, value))
  }

  def storeMem(
      vname: String,
      addr: BitVecLiteral,
      value: BitVecLiteral,
      endian: Endian,
      size: Int
  ): InterpreterState = {
    Logger.debug(s"    eff : STORE $vname[$addr..$addr + $size] := $value ($endian)")
    InterpreterState(nextCmd, callStack, memoryState.storeBV(vname, Scalar(addr), value, endian))
  }

  def storeMultiMem(
      vname: String,
      addr: Literal,
      value: List[Literal],
      endian: Endian
  ): InterpreterState = {
    Logger.debug(s"    eff : STOREMULTI $vname[$addr] := $value ($endian)")
    InterpreterState(nextCmd, callStack, memoryState.store(vname, Scalar(addr), value.map(Scalar(_)), endian))
  }

}

case object InterpFuns {

  def initialState[T <: Effects[T]](s: T): T = {
    val SP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
    val FP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
    val LR: BitVecLiteral = BitVecLiteral(BigInt("FF", 16), 64)

    s.storeVar("mem",   Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
     .storeVar("stack", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
     .storeVar("R31",   Scope.Global, Scalar(SP))
     .storeVar("R29",   Scope.Global, Scalar(FP))
     .storeVar("R30",   Scope.Global, Scalar(LR))
  }

  def initialiseProgram[T <: Effects[T]](p: Program, s: T): T = {
    val mem = p.initialMemory.foldLeft(initialState(s))((s, memory) => {
      s.storeMultiMem("mem", BitVecLiteral(memory.address, 64), memory.bytes.toList, Endian.LittleEndian)
      s.storeMultiMem(
        "stack",
        BitVecLiteral(memory.address, 64),
        memory.bytes.toList,
        Endian.LittleEndian
      )
    })

    mem.call(p.mainProcedure.name, Run(IRWalk.firstInBlock(p.mainProcedure.entryBlock.get)), Stopped())
  }

  def interpretJump[T <: Effects[T]](s: T, j: Jump): T = {
    j match {
      case gt: GoTo if gt.targets.size == 1 => {
        s.setNext(Run(IRWalk.firstInBlock(gt.targets.head)))
      }
      case gt: GoTo =>
        val condition = gt.targets.flatMap(_.statements.headOption).collect { case a: Assume =>
          (a, s.evalBool(a.body))
        }

        if (condition.size != gt.targets.size) {
          throw InterpreterError(Errored(s"Some goto target missing guard $gt"))
        }

        val chosen = condition.filter(_._2).toList match {
          case Nil      => throw InterpreterError(Errored(s"No jump target satisfied $gt"))
          case h :: Nil => h
          case h :: tl  => throw InterpreterError(Errored(s"More than one jump guard satisfied $gt"))
        }
        Logger.debug(s"Goto ${chosen._1.parent.label}")

        s.setNext(Run(chosen._1.successor))
      case r: Return      => s.doReturn()
      case h: Unreachable => s.setNext(EscapedControlFlow(h))
    }
  }

  def interpretStatement[T <: Effects[T]](st: T, s: Statement): T = {
    s match {
      case assign: Assign => {
        val rhs = st.evalBV(assign.rhs)
        st.storeVar(assign.lhs.name, assign.lhs.toBoogie.scope, Scalar(rhs)).setNext(Run(s.successor))
      }
      case assign: MemoryAssign => {
        val index: BitVecLiteral = st.evalBV(assign.index)
        val value: BitVecLiteral = st.evalBV(assign.value)
        st.storeMem(assign.mem.name, index, value, assign.endian, assign.size).setNext(Run(s.successor))

      }
      case assert: Assert => {
        if (!st.evalBool(assert.body)) then {
          st.setNext(FailedAssertion(assert))
        } else {
          st.setNext(Run(s.successor))

        }
      }
      case assume: Assume =>
        if (!st.evalBool(assume.body)) {
          st.setNext(Errored(s"Assumption not satisfied: $assume"))
        } else {
          st.setNext(Run(s.successor))

        }
      case dc: DirectCall =>
        if (dc.target.entryBlock.isDefined) {
          val block = dc.target.entryBlock.get
          st.call(dc.target.name, Run(block.statements.headOption.getOrElse(block.jump)), Run(dc.successor))
        } else {
          st.setNext(Run(dc.successor))
        }
      case ic: IndirectCall =>
        if (ic.target == Register("R30", 64)) {
          st.doReturn()
        } else {
          st.setNext(EscapedControlFlow(ic))
        }
      case _: NOP => st.setNext(Run(s.successor))
    }
  }

  def protect[T](x: () => T, fnly: PartialFunction[Exception, T]): T = {
    try {
      x()
    } catch {
      case e: Exception if fnly.isDefinedAt(e) => fnly(e)
    }
  }

  @tailrec
  def interpret(s: InterpreterState): InterpreterState = {
    Logger.debug(s"interpret ${s.nextCmd}")
    s.nextCmd match {
      case Run(c: Statement) =>
        interpret(
          protect[InterpreterState](
            () => interpretStatement(s, c),
            {
              case InterpreterError(e)         => s.setNext(e)
              case e: IllegalArgumentException => s.setNext(Errored(s"Evaluation error $e"))
            }
          )
        )
      case Run(c: Jump) =>
        interpret(
          protect[InterpreterState](
            () => interpretJump(s, c),
            {
              case InterpreterError(e)         => s.setNext(e)
              case e: IllegalArgumentException => s.setNext(Errored(s"Evaluation error $e"))
            }
          )
        )
      case Stopped() => s
      case errorstop => s
    }
  }

  def interpretProg(p: Program): InterpreterState = {
    var s = initialiseProgram(p, InterpreterState())
    interpret(s)
  }

}

def interpret(IRProgram: Program) = {
  InterpFuns.interpretProg(IRProgram)
}
