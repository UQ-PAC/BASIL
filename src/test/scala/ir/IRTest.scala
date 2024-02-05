package ir

import scala.collection.mutable
import scala.collection.immutable._
import org.scalatest.funsuite.AnyFunSuite
import intrusivelist.{IntrusiveList, IntrusiveListElement}

object Counter {
  private var n: Int = 0;

  def get(): Int = {
    n = n + 1
    n
  }

  def getBlockId(): String = s"block_${get()}"
}

package object IRDSL {

  val R0 = Register("R0", BitVecType(64))
  val R1 = Register("R1", BitVecType(64))
  val R2 = Register("R2", BitVecType(64))
  val R3 = Register("R3", BitVecType(64))
  val R4 = Register("R4", BitVecType(64))
  val R5 = Register("R5", BitVecType(64))
  val R6 = Register("R6", BitVecType(64))
  val R7 = Register("R7", BitVecType(64))
  val ret = EventuallyIndirectCall(Register("R30", BitVecType(64)), None)


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

  def goto() = EventuallyGoto(List.empty)

  def goto(targets: String*) = {
    EventuallyGoto(targets.map(p => DelayNameResolve(p)).toList)
  }

  def goto(targets: List[String]) = {
    EventuallyGoto(targets.map(p => DelayNameResolve(p)))
  }

  def indirectCall(tgt: String, fallthrough: Option[String]) = EventuallyCall(DelayNameResolve(tgt), fallthrough.map(x => DelayNameResolve(x)))

  def call(tgt: String, fallthrough: Option[String]) = EventuallyCall(DelayNameResolve(tgt), fallthrough.map(x => DelayNameResolve(x)))

  def call(tgt: Variable, fallthrough: Option[String]) = EventuallyIndirectCall(tgt, fallthrough.map(x => DelayNameResolve(x)))
  // def directcall(tgt: String) = EventuallyCall(DelayNameResolve(tgt), None)


  case class EventuallyBlock(label: String, sl: Seq[Statement], j: EventuallyJump) {
    val tempBlock: Block = Block(Regular(), label, None, sl, GoTo(List.empty))

    def resolve(prog: Program) = {
      tempBlock.replaceJump(j.resolve(prog))
      tempBlock
    }
  }

  def block(label: String, sl: (Statement | EventuallyJump)*) = {
    val statements = sl.collect {
      case s: Statement => s
    }
    val jump = sl.collectFirst {
      case j: EventuallyJump => j
    }
    EventuallyBlock(label, statements, jump.get)
  }

  case class EventuallyProcedure(label: String, blocks: Seq[EventuallyBlock]) {
    val _blocks = blocks.map(_.tempBlock)
    val tempProc = Procedure(label, None, _blocks.headOption, None, _blocks)
    val jumps: Map[Block, EventuallyJump] = blocks.map(b => b.tempBlock -> b.j).toMap

    def resolve(prog: Program): Procedure = {
      jumps.map((b, j) => b.replaceJump(j.resolve(prog)))
      tempProc
    }


  }

  def proc(label: String, blocks: EventuallyBlock*): EventuallyProcedure = {
    EventuallyProcedure(label, blocks)
  }


  def mem = Memory("mem", 64, 8)

  def stack = Memory("stack", 64, 8)


  def prog(procedures: EventuallyProcedure*): Program = {
    require(procedures.nonEmpty)

    val initialMemory = mutable.ArrayBuffer.empty[MemorySection]
    val readOnlyMemory = mutable.ArrayBuffer.empty[MemorySection]
    val p = Program(mutable.ArrayBuffer.from(procedures.map(_.tempProc)), procedures.map(_.tempProc).head, initialMemory, readOnlyMemory)

    procedures.foreach(_.resolve(p))
    p
  }

}


class IRTest extends AnyFunSuite {


  import IRDSL._

  def getProg(): Program = {
    prog(
      proc("main",
        block("l_main",
          LocalAssign(R0, bv64(10)),
          LocalAssign(R1, bv64(10)),
          goto("newblock")
        ),
        block("l_main_1",
          LocalAssign(R0, bv64(22)),
          call("p2", Some("returntarget"))
        ),
        block("returntarget",
          ret
        )
      ),
      proc("p2",
        block("l_p2", LocalAssign(R0, bv64(10)), goto("l_p2_1")),
        block("l_p2_1", ret)
      )
    )
  }

  test("blockintralinks") {

    val p = prog(
      proc("main",
        block("lmain",
          goto("lmain1")
        ),
        block("lmain1",
          goto("lmain2")),
        block("lmain2",
          ret)
      )
    )

    val blocks = p.collect {
      case b: Block => b.label -> b
    }.toMap

    assert(blocks("lmain").nextBlocks.toSet == Set(blocks("lmain1")))
    assert(blocks("lmain1").nextBlocks.toSet == Set(blocks("lmain2")))
    assert(blocks("lmain2").nextBlocks.toSet == Set.empty)

    assert(blocks("lmain2").prevBlocks.toSet == Set(blocks("lmain1")))
    assert(blocks("lmain1").prevBlocks.toSet == Set(blocks("lmain")))

    assert(blocks("lmain1").singleSuccessor.contains(blocks("lmain2")))
    assert(blocks("lmain1").singlePredecessor.contains(blocks("lmain")))

    blocks("lmain").replaceJump(GoTo(Set.empty))
    val b = p.procedures.head.removeBlocks(blocks("lmain1"))
    assert(!b.hasParent)

    assert(blocks("lmain").singlePredecessor.isEmpty)
    assert(blocks("lmain").singleSuccessor.isEmpty)
    assert(blocks("lmain2").singlePredecessor.isEmpty)
    assert(blocks("lmain2").singleSuccessor.isEmpty)

  }

  test("removeblockinline") {

    val p = prog(
      proc("main",
        block("lmain",
          goto("lmain1")
        ),
        block("lmain1",
          goto("lmain2")),
        block("lmain2",
          ret)
      )
    )

    val blocks = p.collect {
      case b: Block => b.label -> b
    }.toMap

    val b = p.procedures.head.removeBlocksInline(blocks("lmain1"))

    blocks("lmain").singleSuccessor.contains(blocks("lmain2"))
    blocks("lmain2").singlePredecessor.contains(blocks("lmain"))

  }


  test("simple replace jump") {

    val p = prog(
      proc("main",
        block("lmain",
          goto("lmain1")
        ),
        block("lmain1",
          goto("lmain2")),
        block("lmain2",
          ret)
      )
    )

    val blocks = p.collect {
      case b: Block => b.label -> b
    }.toMap

    assert(IntraProcIRCursor.succ(blocks("lmain").jump) == Set(blocks("lmain1")))
    assert(IntraProcIRCursor.succ(blocks("lmain1").jump) == Set(blocks("lmain2")))

    assert(IntraProcIRCursor.pred(blocks("lmain1")) == Set(blocks("lmain").jump))
    assert(IntraProcIRCursor.pred(blocks("lmain2")) == Set(blocks("lmain1").jump))

    blocks("lmain").replaceJump(goto("lmain2").resolve(p))

    assert(IntraProcIRCursor.succ(blocks("lmain").jump) == Set(blocks("lmain2")))
    // lmain1 is unreachable but still jumps to lmain2
    assert(IntraProcIRCursor.pred(blocks("lmain2")) == Set(blocks("lmain").jump, blocks("lmain1").jump))

    // disconnected but still targeted
    assert(IntraProcIRCursor.succ(blocks("lmain1").jump) == Set(blocks("lmain2")))
    assert(IntraProcIRCursor.pred(blocks("lmain1")) == Set())

    blocks("lmain1").parent.removeBlocksDisconnect(blocks("lmain1"))

    // lmain1 is disconnected from the il, no parent or predecessor
    assert(!blocks("lmain1").hasParent)
    assert(blocks("lmain1").prevBlocks.isEmpty)

    // leave lmain1's jump
    assert(blocks("lmain1").nextBlocks.toSet == Set(blocks("lmain2")))

    // lmain2 now only has lmain as predecessor
    assert(IntraProcIRCursor.pred(blocks("lmain2")) == Set(blocks("lmain").jump))
    assert(blocks("lmain2").prevBlocks.toSet == Set(blocks("lmain")))
    assert(blocks("lmain2").singlePredecessor.contains(blocks("lmain")))

  }

  test("aftercalls") {
    val p = getProg()
    val directcalls = p.collect {
      case c: DirectCall => c
    }
    assert(directcalls.forall(_.afterCall.isDefined))

    assert(directcalls.forall(c => IntraProcIRCursor.succ(c).contains(c.afterCall.get)))
    assert(directcalls.forall(c => IntraProcBlockIRCursor.succ(c).contains(c.afterCall.get)))

    val afterCalls = p.collect {
      case b: Block if b.kind.isInstanceOf[AfterCall] => (b, b.kind.asInstanceOf[AfterCall])
    }

    assert(afterCalls.forall((b, ac) => IntraProcIRCursor.pred(b).contains(ac.from)))
    assert(afterCalls.forall((b, ac) => IntraProcBlockIRCursor.pred(b).contains(ac.from.parent)))

  }


}

