package ir.dsl
import ir.*
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.*

// TODO: naming?
type NonControlFlowStatement =
  LocalAssign | MemoryStore | MemoryLoad | NOP | Assert | Assume

type ControlFlowStatement = DirectCall | IndirectCall

val R0: Register = Register("R0", 64)
val R1: Register = Register("R1", 64)
val R2: Register = Register("R2", 64)
val R3: Register = Register("R3", 64)
val R4: Register = Register("R4", 64)
val R5: Register = Register("R5", 64)
val R6: Register = Register("R6", 64)
val R7: Register = Register("R7", 64)
val R8: Register = Register("R8", 64)
val R29: Register = Register("R29", 64)
val R30: Register = Register("R30", 64)
val R31: Register = Register("R31", 64)

def exprEq(l: Expr, r: Expr): Expr = (l, r) match {
  case (l, r) if l.getType != r.getType => FalseLiteral
  case (l, r) if l.getType == BoolType => BinaryExpr(BoolEQ, l, r)
  case (l, r) if l.getType.isInstanceOf[BitVecType] => BinaryExpr(BVEQ, l, r)
  case (l, r) if l.getType == IntType => BinaryExpr(IntEQ, l, r)
  case _ => FalseLiteral
}

def bv32(i: Int): BitVecLiteral = BitVecLiteral(i, 32)

def bv64(i: Int): BitVecLiteral = BitVecLiteral(i, 64)

def bv8(i: Int): BitVecLiteral = BitVecLiteral(i, 8)

def bv16(i: Int): BitVecLiteral = BitVecLiteral(i, 16)

def R(i: Int): Register = Register(s"R$i", 64)

case class DelayNameResolve(ident: String) {
  def resolveProc(prog: Program): Option[Procedure] = prog.collectFirst {
    case b: Procedure if b.name == ident => b
  }

  def resolveBlock(prog: Program): Option[Block] = prog.collectFirst {
    case b: Block if b.label == ident => b
  }
}

trait EventuallyStatement {
  def resolve(p: Program): Statement
}

case class ResolvableStatement(s: NonControlFlowStatement) extends EventuallyStatement {
  override def resolve(p: Program): Statement = s
}

trait EventuallyJump {
  def resolve(p: Program, proc: Procedure): Jump
}

case class EventuallyIndirectCall(target: Variable) extends EventuallyStatement {
  override def resolve(p: Program): Statement = {
    IndirectCall(target)
  }
}

case class EventuallyCall(
  target: DelayNameResolve,
  lhs: Iterable[(String, Variable)],
  actualParams: Iterable[(String, Expr)]
) extends EventuallyStatement {
  override def resolve(p: Program): Statement = {
    val t = target.resolveProc(p) match {
      case Some(x) => x
      case None => throw Exception("can't resolve proc " + p)
    }
    val actual = SortedMap.from(actualParams.map((name, value) => t.formalInParam.find(_.name == name).get -> value))
    val callLhs = SortedMap.from(lhs.map((name, value) => t.formalOutParam.find(_.name == name).get -> value))
    DirectCall(t, None, callLhs, actual)
  }
}

case class EventuallyGoto(targets: List[DelayNameResolve]) extends EventuallyJump {
  override def resolve(p: Program, proc: Procedure): GoTo = {
    val tgs = targets.flatMap(tn => tn.resolveBlock(p))
    GoTo(tgs)
  }
}
case class EventuallyReturn(params: Iterable[(String, Expr)]) extends EventuallyJump {
  override def resolve(p: Program, proc: Procedure) = {
    val r = SortedMap.from(params.map((n, v) => proc.formalOutParam.find(_.name == n).get -> v))
    Return(None, r)
  }
}
case class EventuallyUnreachable() extends EventuallyJump {
  override def resolve(p: Program, proc: Procedure) = Unreachable()
}


def goto(targets: List[String]): EventuallyGoto = {
  EventuallyGoto(targets.map(p => DelayNameResolve(p)))
}

def goto(): EventuallyGoto = goto(Nil)
def goto(targets: String*): EventuallyGoto = goto(targets.toList)

def ret: EventuallyReturn = ret()
def ret(params: (String, Expr)*): EventuallyReturn = EventuallyReturn(params)

def unreachable: EventuallyUnreachable = EventuallyUnreachable()


def directCall(lhs: Iterable[(String, Variable)], tgt: String, actualParams: (String, Expr)*): EventuallyCall =
  EventuallyCall(DelayNameResolve(tgt), lhs.toArray, actualParams)

def directCall(tgt: String): EventuallyCall = directCall(Nil, tgt)

def indirectCall(tgt: Variable): EventuallyIndirectCall = EventuallyIndirectCall(tgt)


case class EventuallyBlock(label: String, sl: Seq[EventuallyStatement], j: EventuallyJump) {
  val tempBlock: Block = Block(label, None, List(), GoTo(List.empty))

  def resolve(prog: Program, proc: Procedure): Block = {
    val resolved = sl.map(_.resolve(prog))
    tempBlock.statements.addAll(resolved)
    tempBlock.replaceJump(j.resolve(prog, proc))
    tempBlock
  }
}

def block(label: String, sl: (NonControlFlowStatement | EventuallyStatement | EventuallyJump)*): EventuallyBlock = {
  val statements: Seq[EventuallyStatement] = sl.flatMap {
    case s: NonControlFlowStatement => Some(ResolvableStatement(s))
    case o: EventuallyStatement => Some(o)
    case g: EventuallyJump => None
  }
  val jump = sl.collect { case j: EventuallyJump => j }
  require(jump.length == 1, s"DSL block '$label' must contain exactly one jump statement")
  EventuallyBlock(label, statements, jump.head)
}

case class EventuallyProcedure(
  label: String,
  in: Map[String, IRType] = Map(),
  out: Map[String, IRType] = Map(),
  blocks: Seq[EventuallyBlock]
) {
  val _blocks: Seq[Block] = blocks.map(_.tempBlock)
  val tempProc: Procedure = Procedure(
    label,
    None,
    _blocks.headOption,
    None,
    _blocks,
    in.map((n, t) => LocalVar(n, t)),
    out.map((n, t) => LocalVar(n, t))
  )
  val jumps: Map[Block, EventuallyJump] = blocks.map(b => b.tempBlock -> b.j).toMap

  def resolve(prog: Program): Procedure = {
    val resolvedBlocks = blocks.map(b => b.resolve(prog, tempProc))
    jumps.map((b, j) => b.replaceJump(j.resolve(prog, tempProc)))
    resolvedBlocks.headOption.foreach(b => tempProc.entryBlock = b)
    tempProc
  }

  def toProg() = {
    val p = prog(this)
    this.resolve(p)
    p
  }

  def addToProg(p: Program) = {
    p.addProcedure(tempProc)
    val proc = resolve(p)
    proc
  }

}

def proc(label: String, blocks: EventuallyBlock*): EventuallyProcedure = {
  EventuallyProcedure(label, Map(), Map(), blocks)
}

def proc(
  label: String,
  in: Iterable[(String, IRType)],
  out: Iterable[(String, IRType)],
  blocks: EventuallyBlock*
): EventuallyProcedure = {
  EventuallyProcedure(label, in.toMap, out.toMap, blocks)
}

def mem: SharedMemory = SharedMemory("mem", 64, 8)

def stack: SharedMemory = SharedMemory("stack", 64, 8)

case class EventuallyProgram(mainProcedure: EventuallyProcedure, otherProcedures: EventuallyProcedure*) {
  val allProcedures = mainProcedure +: otherProcedures

  def resolve: Program = {
    val initialMemory = mutable.TreeMap[BigInt, MemorySection]()

    val tempProcs = allProcedures.map(_.tempProc)
    val procs = ArrayBuffer.from(tempProcs)

    val p = Program(procs, procs.head, initialMemory)

    allProcedures.foreach(_.resolve(p))
    assert(ir.invariant.correctCalls(p))
    assert(ir.invariant.cfgCorrect(p))
    p
  }
}

def prog(mainProc: EventuallyProcedure, procedures: EventuallyProcedure*) =
  EventuallyProgram(mainProc, procedures: _*).resolve
