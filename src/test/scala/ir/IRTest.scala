package ir

import scala.collection.mutable
import scala.collection.immutable.*
import org.scalatest.funsuite.AnyFunSuite
import util.intrusive_list.*
import translating.serialiseIL
import ir.dsl.*
import ir._

class IRTest extends AnyFunSuite {

  extension (p: Program) {
    def procs: Map[String, Procedure] = p.collect {
      case b: Procedure => b.name -> b
    }.toMap

    def blocks: Map[String, Block] = p.collect {
      case b: Block => b.label -> b
    }.toMap
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
    val p = prog(
      proc("main",
        block("l_main",
          Assign(R0, bv64(10)),
          Assign(R1, bv64(10)),
          goto("newblock")
        ),
        block("l_main_1",
          Assign(R0, bv64(22)),
          directCall("p2"), 
          goto("returntarget")
        ),
        block("returntarget",
          ret
        )
      ),
      proc("p2",
        block("l_p2", Assign(R0, bv64(10)), goto("l_p2_1")),
        block("l_p2_1", ret)
      )
    )


    val blocks = p.collect {
      case b: Block => b.label -> b
    }.toMap

    val directcalls = p.collect {
      case c: DirectCall => c
    }

    assert(p.toSet.contains(blocks("l_main_1").jump))
    assert(directcalls.forall(c => IntraProcIRCursor.succ(c).count(c => isAfterCall(c.asInstanceOf[Command])) == 1))

    val afterCalls = p.collect {
      case b: Command if isAfterCall(b) => b
    }.toSet

    assert(afterCalls.toSet == Set(blocks("l_main_1").jump))
    val aftercallGotos = p.collect {
      case c: Command if isAfterCall(c) => c
    }.toSet
    // assert(aftercallGotos == Set(blocks("l_main_1").fallthrough.get))

    assert(1 == aftercallGotos.count(b => IntraProcIRCursor.pred(b).contains(blocks("l_main_1").jump)))
    assert(1 == aftercallGotos.count(b => IntraProcIRCursor.succ(b).contains(blocks("l_main_1").jump match {
      case GoTo(targets, _) => targets.head
    })))

  }

  test("addblocks") {

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

    val b2 = block("newblock2",
      Assign(R0, bv64(22)),
      Assign(R0, bv64(22)),
      Assign(R0, bv64(22)),
      goto("lmain2")
    ).resolve(p)
    val b1 = block("newblock1",
      Assign(R0, bv64(22)),
      Assign(R0, bv64(22)),
      Assign(R0, bv64(22)),
      goto("lmain2")
    ).resolve(p)

    p.procedures.head.addBlocks(Seq(b1, b2))

    val blocks = p.collect {
      case b: Block => b.label -> b
    }.toMap

    assert(p.toSet.contains(b1))
    assert(p.toSet.contains(b2))
    assert(blocks("lmain2").incomingJumps.contains(b1.jump.asInstanceOf[GoTo]))
    assert(blocks("lmain2").incomingJumps.contains(b2.jump.asInstanceOf[GoTo]))


  }

  test("addblocks empty proc") {

    val p = prog(
      proc("main",
        block("lmain",
          goto("lmain1")
        ),
        block("lmain1",
          goto("lmain2")),
        block("lmain2",
          ret)
      ),
      proc("called")
    )


    val b1= block("newblock2",
      Assign(R0, bv64(22)),
      Assign(R0, bv64(22)),
      Assign(R0, bv64(22)),
      directCall("main"),
      halt
    ).resolve(p)
    val b2 = block("newblock1",
      Assign(R0, bv64(22)),
      Assign(R0, bv64(22)),
      Assign(R0, bv64(22)),
      ret
    ).resolve(p)

    assert(p.mainProcedure eq p.procedures.find(_.name == "main").get)
    val called = p.procedures.find(_.name == "called").get
    called.addBlocks(b1)
    called.addBlocks(b2)

    assert(called.blocks.size == 2)
    assert(called.entryBlock.contains(b1))
    assert(called.returnBlock.isEmpty)

    def blocks = p.collect {
      case b: Block => b.label -> b
    }.toMap

    assert(called.incomingCalls().isEmpty)
    val b3 = block("newblock3",
      Assign(R0, bv64(22)),
      directCall("called"),
      halt
    ).resolve(p)

    assert(b3.calls.toSet == Set(p.procs("called")))
    val oldb = blocks("lmain2")
    p.mainProcedure.replaceBlock(blocks("lmain2"), b3)

    assert(p.mainProcedure.calls.toSet == Set(p.procs("called")))
    assert(p.mainProcedure.calls.forall(_.callers().exists(_ == p.mainProcedure)))
    assert(!oldb.hasParent)
    assert(oldb.incomingJumps.isEmpty)
    assert(!blocks("lmain").jump.asInstanceOf[GoTo].targets.contains(oldb))
    assert(called.incomingCalls().toSet == Set(b3.statements.last))
    assert(called.incomingCalls().map(_.parent.parent).toSet == called.callers().toSet)
    val olds = blocks.size
    p.mainProcedure.replaceBlock(b3, b3)
    assert(called.incomingCalls().toSet == Set(b3.statements.last))
    assert(olds == blocks.size)
    p.mainProcedure.addBlocks(block("test", ret).resolve(p))
    assert(olds != blocks.size)

    p.mainProcedure.replaceBlocks(Set(block("test", ret).resolve(p)))
    assert(blocks.count(_._2.parent.name == "main") == 1)

  }

  test("clearblocks") {
    val p = prog(
        proc("main",
          block("l_main",
            Assign(R0, bv64(10)),
            Assign(R1, bv64(10)),
            goto("returntarget")
          ),
          block("returntarget",
            ret
          )
        ),
      )

    assert(p.blocks.size > 1)
    assert(p.procs("main").entryBlock.isDefined)
    p.procs("main").returnBlock = block("retb", ret).resolve(p)
    assert(p.procs("main").returnBlock.isDefined)
    p.procs("main").clearBlocks()
    assert(p.blocks.isEmpty)
    assert(p.procs("main").entryBlock.isEmpty)
    assert(p.procs("main").returnBlock.isEmpty)
  }

  test("interproc aftercall") {

    val p = prog(
      proc("p1",
        block("b1",
          Assign(R0, bv64(10)),
          ret
        )
      ),
      proc("main",
        block("l_main",
          Assign(R0, bv64(10)),
          directCall("p1"), goto("returntarget")
        ),
        block("returntarget",
          ret
        )
      ),
    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), p)
    transforms.addReturnBlocks(p)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), p)

    val next = InterProcIRCursor.succ(p.blocks("l_main").jump)
    val prev = InterProcIRCursor.pred(p.blocks("returntarget"))

    assert(prev.size == 1 && prev.collect {
      case c : GoTo => (c.parent == p.blocks("l_main"))
    }.contains(true))

    // assert(next == Set(p.procs("p1"), p.blocks("l_main").fallthrough.get))

    val prevB: Command = (p.blocks("l_main").statements.lastOption match
      case Some(c: IndirectCall) => c.returnTarget
      case Some(c: DirectCall) => c.returnTarget
      case o => None
    ).get

    assert(isAfterCall(prevB))
    assert(InterProcIRCursor.pred(prevB).size == 1)

  }

  test("replace jump") {
    val p = prog(
      proc("p1",
        block("b1",
          ret
        )
      ),
      proc("main",
        block("l_main",
          indirectCall(R1), goto("returntarget")
        ),
        block("block2",
          directCall("p1"), goto("returntarget")
        ),
        block("returntarget",
          ret
        )
      ),
    )

    val main = p.blocks("l_main")
    val p1 = p.procs("p1")
    val block2 = p.blocks("block2")

    val oldJump = main.jump
    val newJump = block2.jump

    main.replaceJump(newJump)

    assert(newJump.parent == main)
    assert(block2.jump.isInstanceOf[GoTo])
    assert(block2.jump.asInstanceOf[GoTo].targets.isEmpty)
  }

}

