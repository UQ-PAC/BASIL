package ir.dsl
import ir.*
import scala.collection.mutable
import scala.collection.immutable.*

val R0: Register = Register("R0", BitVecType(64))
val R1: Register = Register("R1", BitVecType(64))
val R2: Register = Register("R2", BitVecType(64))
val R3: Register = Register("R3", BitVecType(64))
val R4: Register = Register("R4", BitVecType(64))
val R5: Register = Register("R5", BitVecType(64))
val R6: Register = Register("R6", BitVecType(64))
val R7: Register = Register("R7", BitVecType(64))
val R29: Register = Register("R29", BitVecType(64))
val R30: Register = Register("R30", BitVecType(64))
val R31: Register = Register("R31", BitVecType(64))
val ret: EventuallyIndirectCall = EventuallyIndirectCall(Register("R30", BitVecType(64)), None)


def bv32(i: Int): BitVecLiteral = BitVecLiteral(i, 32)

def bv64(i: Int): BitVecLiteral = BitVecLiteral(i, 64)

def bv8(i: Int): BitVecLiteral = BitVecLiteral(i, 8)

def bv16(i: Int): BitVecLiteral = BitVecLiteral(i, 16)

case class DelayNameResolve(ident: String) {
  def resolveProc(prog: Program): Option[Procedure] = prog.collectFirst {
    case b: Procedure if b.name == ident => b
  }

  def resolveBlock(prog: Program): Option[Block] = prog.collectFirst {
    case b: Block if b.label == ident => b
  }
}

trait EventuallyJump {
  def resolve(p: Program): Jump
}

case class EventuallyIndirectCall(target: Variable, fallthrough: Option[DelayNameResolve]) extends EventuallyJump {
  override def resolve(p: Program): IndirectCall = {
    IndirectCall(target, fallthrough.flatMap(_.resolveBlock(p)))
  }
}

case class EventuallyCall(target: DelayNameResolve, fallthrough: Option[DelayNameResolve]) extends EventuallyJump {
  override def resolve(p: Program): DirectCall = {
    val t = target.resolveProc(p).get
    val ft = fallthrough.flatMap(_.resolveBlock(p))
    DirectCall(t, ft)
  }
}

case class EventuallyGoto(targets: List[DelayNameResolve]) extends EventuallyJump {
  override def resolve(p: Program): GoTo = {
    val tgs = targets.flatMap(tn => tn.resolveBlock(p))
    GoTo(tgs)
  }
}

def goto(): EventuallyGoto = EventuallyGoto(List.empty)

def goto(targets: String*): EventuallyGoto = {
  EventuallyGoto(targets.map(p => DelayNameResolve(p)).toList)
}

def goto(targets: List[String]): EventuallyGoto = {
  EventuallyGoto(targets.map(p => DelayNameResolve(p)))
}

def indirectCall(tgt: String, fallthrough: Option[String]): EventuallyCall = EventuallyCall(DelayNameResolve(tgt), fallthrough.map(x => DelayNameResolve(x)))

def call(tgt: String, fallthrough: Option[String]): EventuallyCall = EventuallyCall(DelayNameResolve(tgt), fallthrough.map(x => DelayNameResolve(x)))

def call(tgt: Variable, fallthrough: Option[String]): EventuallyIndirectCall = EventuallyIndirectCall(tgt, fallthrough.map(x => DelayNameResolve(x)))
// def directcall(tgt: String) = EventuallyCall(DelayNameResolve(tgt), None)


case class EventuallyBlock(label: String, sl: Seq[Statement], j: EventuallyJump) {
  val tempBlock: Block = Block(label, None, sl, GoTo(List.empty))

  def resolve(prog: Program): Block = {
    tempBlock.replaceJump(j.resolve(prog))
    tempBlock
  }
}

def block(label: String, sl: (Statement | EventuallyJump)*): EventuallyBlock = {
  val statements = sl.collect {
    case s: Statement => s
  }
  val jump = sl.collectFirst {
    case j: EventuallyJump => j
  }
  EventuallyBlock(label, statements, jump.get)
}

case class EventuallyProcedure(label: String, blocks: Seq[EventuallyBlock]) {
  val _blocks: Seq[Block] = blocks.map(_.tempBlock)
  val tempProc: Procedure = Procedure(label, None, _blocks.headOption, _blocks)
  val jumps: Map[Block, EventuallyJump] = blocks.map(b => b.tempBlock -> b.j).toMap

  def resolve(prog: Program): Procedure = {
    jumps.map((b, j) => b.replaceJump(j.resolve(prog)))
    tempProc
  }


}

def proc(label: String, blocks: EventuallyBlock*): EventuallyProcedure = {
  EventuallyProcedure(label, blocks)
}


def mem: Memory = Memory("mem", 64, 8)

def stack: Memory = Memory("stack", 64, 8)


def prog(procedures: EventuallyProcedure*): Program = {
  require(procedures.nonEmpty)

  val initialMemory = mutable.ArrayBuffer.empty[MemorySection]
  val readOnlyMemory = mutable.ArrayBuffer.empty[MemorySection]
  val p = Program(mutable.ArrayBuffer.from(procedures.map(_.tempProc)), procedures.map(_.tempProc).head, initialMemory, readOnlyMemory)

  procedures.foreach(_.resolve(p))
  p
}


