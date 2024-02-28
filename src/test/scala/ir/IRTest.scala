package ir

import scala.collection.mutable
import scala.collection.immutable.*
import org.scalatest.funsuite.AnyFunSuite
import util.intrusive_list.*
import ir.dsl.*

class IRTest extends AnyFunSuite {

  extension (p: Program) {
    def procs: Map[String, Procedure] = p.collect {
      case b: Procedure => b.name -> b
    }.toMap

    def blocks: Map[String, Block] = p.collect {
      case b: Block => b.label -> b
    }.toMap
  }

  test("remove block with return ") {

    var p = prog(
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

    val bl = p.blocks("lmain2")

    assert(bl.jump.asInstanceOf[GoTo].targets.contains(p.procs("main").returnBlock))

    assert(p.procs("main").returnBlock.incomingJumps.nonEmpty)

    p.procs("main").removeBlocksDisconnect(bl)

    assert(bl.incomingJumps.isEmpty)
    assert(bl.nextBlocks.isEmpty)

    assert(p.procs("main").returnBlock.incomingJumps.isEmpty)

    //bl.replaceJump(Return(), false)
    assert(bl.jump.isInstanceOf[Return])
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
    assert(blocks("lmain2").nextBlocks.toSet == Set(p.procs("main").returnBlock))

    assert(blocks("lmain2").prevBlocks.toSet == Set(blocks("lmain1")))
    assert(blocks("lmain1").prevBlocks.toSet == Set(blocks("lmain")))

    assert(blocks("lmain1").singleSuccessor.contains(blocks("lmain2")))
    assert(blocks("lmain1").singlePredecessor.contains(blocks("lmain")))

    blocks("lmain").replaceJump(GoTo(Set.empty))
    val b = p.procedures.head.removeBlocks(blocks("lmain1"))
    assert(!b.hasParent)

    assert(blocks("lmain").singleSuccessor.isEmpty)
    assert(blocks("lmain2").singlePredecessor.isEmpty)
    assert(blocks("lmain2").singleSuccessor.contains(p.procs("main").returnBlock))

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

    p.procedures.head.removeBlocksInline(blocks("lmain1"))

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
    val p = prog(
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

    val blocks = p.collect {
      case b: Block => b.label -> b
    }.toMap


    val directcalls = p.collect {
      case c: DirectCall => c
    }

    assert(blocks("l_main_1").fallthrough.nonEmpty)
    assert(p.toSet.contains(blocks("l_main_1").fallthrough.get))
    assert(directcalls.forall(c => IntraProcIRCursor.succ(c).count(_.asInstanceOf[GoTo].isAfterCall) == 1))
    assert(directcalls.forall(c => IntraProcBlockIRCursor.succ(c).count(_.isAfterCall) == 1))

    val afterCalls = p.collect {
      case b: Block if b.isAfterCall => b
    }.toSet

    assert(afterCalls.toSet == Set(blocks("returntarget")))
    val aftercallGotos = p.collect {
      case c: Jump if c.isAfterCall => c
    }.toSet
    assert(aftercallGotos == Set(blocks("l_main_1").fallthrough.get))

    assert(1 == aftercallGotos.count(b => IntraProcIRCursor.pred(b).contains(blocks("l_main_1").jump)))
    assert(1 == aftercallGotos.count(b => IntraProcIRCursor.succ(b).contains(blocks("l_main_1").fallthrough.map(_.targets.head).head)))

    assert(afterCalls.forall(b => IntraProcBlockIRCursor.pred(b).contains(blocks("l_main_1"))))

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
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      goto("lmain2")
    ).resolve(p)
    val b1 = block("newblock1",
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
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
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      call("main", None)
    ).resolve(p)
    val b2 = block("newblock1",
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      ret
    ).resolve(p)

    assert(p.mainProcedure eq p.procedures.find(_.name == "main").get)
    val called = p.procedures.find(_.name == "called").get

    assert(called.blocks.size == 2)
    assert(called.blocks.contains(called.returnBlock))
    assert(called.blocks.contains(called.entryBlock))
    assert(called.innerBlocks.isEmpty)

    called.addBlock(b1)
    called.addBlock(b2)

    assert(called.blocks.size == 4)
    assert(called.entryBlock.jump.asInstanceOf[GoTo].targets.contains(called.returnBlock))
    assert(b1.incomingJumps.toSet == Set.empty)
    assert(b2.incomingJumps.isEmpty)

    def blocks = p.collect {
      case b: Block => b.label -> b
    }.toMap

    assert(called.incomingCalls().isEmpty)
    val b3 = block("newblock3",
      LocalAssign(R0, bv64(22)),
      call("called", None)
    ).resolve(p)

    assert(b3.calls.toSet == Set(p.procs("called")))
    val oldb = blocks("lmain2")
    p.mainProcedure.replaceBlock(blocks("lmain2"), b3)

    assert(p.mainProcedure.calls.toSet == Set(p.procs("called")))
    assert(p.mainProcedure.calls.forall(_.callers().exists(_ == p.mainProcedure)))
    assert(!oldb.hasParent)
    assert(oldb.incomingJumps.isEmpty)
    assert(!blocks("lmain").jump.asInstanceOf[GoTo].targets.contains(oldb))
    assert(called.incomingCalls().toSet == Set(b3.jump))
    assert(called.incomingCalls().map(_.parent.parent).toSet == called.callers().toSet)
    val olds = blocks.size
    p.mainProcedure.replaceBlock(b3, b3)
    assert(called.incomingCalls().toSet == Set(b3.jump))
    assert(olds == blocks.size)
    p.mainProcedure.addBlock(block("test", ret).resolve(p))
    assert(olds != blocks.size)

    p.mainProcedure.replaceBlocks(Set(block("test", ret).resolve(p)))
    // val mainblocks = blocks.filter(_._2.parent.name == "main")
    assert(blocks.count(_._2.parent.name == "main") == 3) // test entry and return_block

  }

  test("clearblocks") {
    val p = prog(
        proc("main",
          block("l_main",
            LocalAssign(R0, bv64(10)),
            LocalAssign(R1, bv64(10)),
            goto("returntarget")
          ),
          block("returntarget",
            ret
          )
        ),
      )

    assert(p.procs("main").innerBlocks.size > 1)
    //p.procs("main").returnBlock = block("retb", ret).resolve(p)
    p.procs("main").clearBlocks()
    assert(p.procs("main").innerBlocks.isEmpty)

    assert(!p.procs("main").hasImplementation)
    assert(p.procs("main").isStub)
  }

  test("interproc aftercall") {

    val p = prog(
      proc("p1",
        block("b1",
          LocalAssign(R0, bv64(10)),
          ret
        )
      ),
      proc("main",
        block("l_main",
          LocalAssign(R0, bv64(10)),
          call("p1", Some("returntarget"))
        ),
        block("returntarget",
          ret
        )
      ),
    )
    val returnUnifier = ConvertToSingleProcedureReturn()
    returnUnifier.visitProgram(p)

    val next = InterProcIRCursor.succ(p.blocks("l_main").jump)
    val prev = InterProcIRCursor.pred(p.blocks("returntarget"))

    assert(prev.size == 1 && prev.collect {
      case c : GoTo => (c.parent == p.blocks("l_main")) && c.isAfterCall
    }.contains(true))

    assert(next == Set(p.procs("p1"), p.blocks("l_main").fallthrough.get))

    val prevB: Block = (p.blocks("l_main").jump match
      case c: IndirectCall => c.returnTarget
      case c: DirectCall => c.returnTarget
      case _ => None
    ).get

    assert(prevB.isAfterCall)
    assert(InterProcIRCursor.pred(prevB).size == 1)
    assert(InterProcIRCursor.pred(prevB).head == p.blocks("l_main").fallthrough.get)
    assert(InterProcBlockIRCursor.pred(prevB).head == p.blocks("l_main"), p.procs("p1").returnBlock)

  }

}

