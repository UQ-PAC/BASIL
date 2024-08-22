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
case class Stopped() extends ExecutionContinuation

/** Normal stop * */
case class Run(val next: Command) extends ExecutionContinuation
case class EscapedControlFlow(val call: Jump | Call) extends ExecutionContinuation

/** controlflow has reached somewhere eunrecoverable */
case class Errored(val message: String = "") extends ExecutionContinuation
case class TypeError(val message: String = "") extends ExecutionContinuation /* type mismatch appeared */
case class EvalError(val message: String = "")
    extends ExecutionContinuation /* failed to evaluate an expression to a concrete value */
case class MemoryError(val message: String = "")
    extends ExecutionContinuation /* failed to evaluate an expression to a concrete value */

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

given Conversion[BitVecLiteral, Scalar] with
  def apply(b: BitVecLiteral): Scalar = new Scalar(b)

given Conversion[IntLiteral, Scalar] with
  def apply(b: IntLiteral): Scalar = new Scalar(b)

given Conversion[BoolLit, Scalar] with
  def apply(b: BoolLit): Scalar = new Scalar(b)

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

  def setVar(frame: StackFrameID, varname: String, value: BasilValue): MemoryState = {
    val nv = stackFrames + (frame -> (stackFrames(frame) + (varname -> value)))
    MemoryState(nv, activations, activationCount)
  }

  def setVar(v: String, value: BasilValue): MemoryState = {
    val frame = findVarOpt(v).map(_._1).getOrElse(activations.head)
    setVar(frame, v, value)
  }

  def setVar(v: String, value: Literal): MemoryState = {
    setVar(v, Scalar(value))
  }

  def defVar(v: Variable, value: Literal): MemoryState = {
    val frame = v.toBoogie.scope match {
      case Scope.Global => globalFrame
      case _            => activations.head
    }
    setVar(frame, v.name, Scalar(value))
  }

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
      case _ => throw InterpreterError(Errored("Invalid map store operation."))
    }
    val keys = (0 until values.size).map(i => BasilValue.unsafeAdd(addr, i))
    val vals = endian match {
      case Endian.LittleEndian => values.reverse
      case Endian.BigEndian    => values
    }

    val nmap = MapValue(mapval.value ++ keys.zip(vals), mapval.irType)
    setVar(frame, vname, nmap)
  }

  /** Store extract bitvec to bytes and store bytes */
  def storeBV(vname: String, addr: BasilValue, value: BitVecLiteral, endian: Endian) = {
    val (frame, mem) = findVar(vname)
    val (mapval, vsize) = mem match {
      case m @ MapValue(_, MapType(kt, BitVecType(size))) if kt == addr.irType => (m, size)
      case _ => throw InterpreterError(Errored("Tried to extract-store non to bv map"))
    }
    val cells = value.size / vsize
    if (cells < 1) {
      throw InterpreterError(Errored("Tried to execute fractional store"))
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

def initialState(): MemoryState = {
  val SP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
  val FP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
  val LR: BitVecLiteral = BitVecLiteral(BigInt("FF", 16), 64)

  MemoryState()
    .setVar(globalFrame, "mem", MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
    .setVar(globalFrame, "stack", MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
    .setVar(globalFrame, "R31", Scalar(SP))
    .setVar(globalFrame, "R29", Scalar(FP))
    .setVar(globalFrame, "R30", Scalar(LR))
}

sealed trait Effects[T <: Effects[T]] {
  /* evaluation (may side-effect via InterpreterException on evaluation failure) */
  def evalBV(e: Expr): BitVecLiteral

  def evalInt(e: Expr): BigInt

  def evalBool(e: Expr): Boolean

  /** effects * */
  def setNext(c: ExecutionContinuation): T

  def call(c: DirectCall): T

  def doReturn(): T

  def storeVar(v: Variable, value: Literal): T

  def storeMem(vname: String, addr: BitVecLiteral, value: BitVecLiteral, endian: Endian, size: Int): T

  def initialiseProgram(p: Program): T
}

case class InterpreterCFState(
    val nextCmd: ExecutionContinuation = Stopped(),
    val callStack: List[DirectCall] = List.empty,
    val memoryState: MemoryState = MemoryState()
) extends Effects[InterpreterCFState] {

  /** eval * */
  def evalBV(e: Expr): BitVecLiteral = Eval.evalBV(memoryState, e)

  def evalInt(e: Expr): BigInt = Eval.evalInt(memoryState, e)

  def evalBool(e: Expr): Boolean = Eval.evalBool(memoryState, e)

  /** effects * */
  def setNext(c: ExecutionContinuation): InterpreterCFState = {
    InterpreterCFState(c, callStack, memoryState)
  }

  def call(c: DirectCall): InterpreterCFState = {
    Logger.debug(s"    eff : CALL $c")
    c.target.entryBlock match {
      case Some(block) =>
        InterpreterCFState(
          Run(block.statements.headOption.getOrElse(block.jump)),
          c :: callStack,
          memoryState.pushStackFrame(c.target.name)
        )
      case None => setNext(Run(c.successor))
    }
  }

  def doReturn(): InterpreterCFState = {
    callStack match {
      case Nil => InterpreterCFState(Stopped(), Nil, memoryState)
      case h :: tl => {
        Logger.debug(s"    eff : RETURN $h")
        InterpreterCFState(Run(h.successor), tl, memoryState.popStackFrame())
      }
    }
  }

  def storeVar(v: Variable, value: Literal) = {
    Logger.debug(s"    eff : SET $v := $value")
    InterpreterCFState(nextCmd, callStack, memoryState.defVar(v, value))
  }

  def storeMem(
      vname: String,
      addr: BitVecLiteral,
      value: BitVecLiteral,
      endian: Endian,
      size: Int
  ): InterpreterCFState = {
    Logger.debug(s"    eff : STORE $vname[$addr..$addr + $size] := $value ($endian)")
    InterpreterCFState(nextCmd, callStack, memoryState.storeBV(vname, Scalar(addr), value, endian))
  }

  def initialiseProgram(p: Program): InterpreterCFState = {
    val mem = p.initialMemory.foldLeft(initialState())((s, memory) => {
      s.store("mem", Scalar(BitVecLiteral(memory.address, 64)), memory.bytes.map(Scalar(_)).toList, Endian.LittleEndian)
      s.store(
        "stack",
        Scalar(BitVecLiteral(memory.address, 64)),
        memory.bytes.map(Scalar(_)).toList,
        Endian.LittleEndian
      )
    })

    InterpreterCFState(
      Run(IRWalk.firstInBlock(p.mainProcedure.entryBlock.get)),
      callStack,
      mem.pushStackFrame(p.mainProcedure.name)
    )
  }
}

case object InterpFuns {

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
        st.storeVar(assign.lhs, rhs).setNext(Run(s.successor))

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
        st.call(dc)
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
  def interpret(s: InterpreterCFState): InterpreterCFState = {
    Logger.debug(s"interpret ${s.nextCmd}")
    s.nextCmd match {
      case Run(c: Statement) =>
        interpret(
          protect[InterpreterCFState](
            () => interpretStatement(s, c),
            {
              case InterpreterError(e)         => s.setNext(e)
              case e: IllegalArgumentException => s.setNext(Errored(s"Evaluation error $e"))
            }
          )
        )
      case Run(c: Jump) =>
        interpret(
          protect[InterpreterCFState](
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

  def interpretProg(p: Program): InterpreterCFState = {
    var s = InterpreterCFState().initialiseProgram(p)
    interpret(s)
  }

}

def interpret(IRProgram: Program) = {
  InterpFuns.interpretProg(IRProgram)
}
