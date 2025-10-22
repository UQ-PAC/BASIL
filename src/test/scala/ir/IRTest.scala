package ir

import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

import scala.collection.immutable.*

@test_util.tags.UnitTest
class IRTest extends AnyFunSuite with CaptureOutput {

  test("blockintralinks") {
    val p = prog(proc("main", block("lmain", goto("lmain1")), block("lmain1", goto("lmain2")), block("lmain2", ret)))

    val blocks = p.labelToBlock

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
    val p = prog(proc("main", block("lmain", goto("lmain1")), block("lmain1", goto("lmain2")), block("lmain2", ret)))

    val blocks = p.labelToBlock

    assert(IntraProcIRCursor.succ(blocks("lmain").jump) == Set(blocks("lmain1")))
    assert(IntraProcIRCursor.succ(blocks("lmain1").jump) == Set(blocks("lmain2")))

    assert(IntraProcIRCursor.pred(blocks("lmain1")) == Set(blocks("lmain").jump))
    assert(IntraProcIRCursor.pred(blocks("lmain2")) == Set(blocks("lmain1").jump))

    blocks("lmain").replaceJump(goto("lmain2").resolve(p, "main"))

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
      proc(
        "main",
        block("l_main", LocalAssign(R0, bv64(10)), LocalAssign(R1, bv64(10)), goto("l_main_1")),
        block("l_main_1", LocalAssign(R0, bv64(22)), directCall("p2"), goto("returntarget")),
        block("returntarget", ret)
      ),
      proc("p2", block("l_p2", LocalAssign(R0, bv64(10)), goto("l_p2_1")), block("l_p2_1", ret))
    )

    val blocks = p.labelToBlock

    val directcalls = p.collect { case c: DirectCall =>
      c
    }

    assert(p.toSet.contains(blocks("l_main_1").jump))
    assert(directcalls.forall(c => IntraProcIRCursor.succ(c).count(c => isAfterCall(c.asInstanceOf[Command])) == 1))

    val afterCalls = p.collect {
      case b: Command if isAfterCall(b) => b
    }.toSet

    assert(afterCalls == Set(blocks("l_main_1").jump))
    val aftercallGotos = p.collect {
      case c: Command if isAfterCall(c) => c
    }.toSet

    assert(1 == aftercallGotos.count(b => IntraProcIRCursor.pred(b).contains(blocks("l_main_1").statements.last)))
    assert(
      1 == aftercallGotos.count(b =>
        IntraProcIRCursor
          .succ(b)
          .contains(blocks("l_main_1").jump match {
            case GoTo(targets, _) => targets.head
            case _ => throw Exception("unreachable")
          })
      )
    )
  }

  test("addblocks") {
    val p = prog(proc("main", block("lmain", goto("lmain1")), block("lmain1", goto("lmain2")), block("lmain2", ret)))

    val pp = p.procedures.head

    val b2 = block(
      "newblock2",
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      goto("lmain2")
    ).resolve(p, pp.name)
    val b1 = block(
      "newblock1",
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      goto("lmain2")
    ).resolve(p, pp.name)

    p.procedures.head.addBlocks(Seq(b1, b2))

    val blocks = p.labelToBlock

    assert(p.toSet.contains(b1))
    assert(p.toSet.contains(b2))
    assert(blocks("lmain2").incomingJumps.contains(b1.jump.asInstanceOf[GoTo]))
    assert(blocks("lmain2").incomingJumps.contains(b2.jump.asInstanceOf[GoTo]))
  }

  test("addblocks empty proc") {
    val p = prog(
      proc("main", block("lmain", goto("lmain1")), block("lmain1", goto("lmain2")), block("lmain2", ret)),
      proc("called")
    )
    val called = p.procedures.find(_.name == "called").get

    val b1 = block(
      "newblock2",
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      LocalAssign(R0, bv64(22)),
      directCall("main"),
      unreachable
    ).resolve(p, "called")
    val b2 = block("newblock1", LocalAssign(R0, bv64(22)), LocalAssign(R0, bv64(22)), LocalAssign(R0, bv64(22)), ret)
      .resolve(p, "called")

    assert(p.mainProcedure eq p.procedures.find(_.name == "main").get)

    called.addBlock(b1)
    called.addBlock(b2)
    // no longer implicitly set entryblock to the first block
    called.entryBlock = b1

    assert(called.blocks.size == 2)
    assert(called.entryBlock.contains(b1))
    assert(called.returnBlock.isEmpty)

    var blocks = p.labelToBlock
    val procs = p.nameToProcedure

    assert(called.incomingCalls().isEmpty)
    val b3 = block("newblock3", LocalAssign(R0, bv64(22)), directCall("called"), unreachable).resolve(p, "called")

    blocks = p.labelToBlock

    assert(b3.calls == Set(procs("called")))
    val oldb = blocks("lmain2")
    p.mainProcedure.replaceBlock(blocks("lmain2"), b3)

    assert(p.mainProcedure.calls == Set(procs("called")))
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
    p.mainProcedure.addBlock(block("test", ret).resolve(p, p.mainProcedure.name))
    blocks = p.labelToBlock
    assert(olds != blocks.size)

    p.mainProcedure.replaceBlocks(Set(block("test", ret).resolve(p, p.mainProcedure.name)))
    blocks = p.labelToBlock
    assert(blocks.count(_(1).parent.name == "main") == 1)

  }

  test("clearblocks") {
    val p = prog(
      proc(
        "main",
        block("l_main", LocalAssign(R0, bv64(10)), LocalAssign(R1, bv64(10)), goto("returntarget")),
        block("returntarget", ret)
      )
    )

    var blocks = p.labelToBlock
    var procs = p.nameToProcedure

    assert(blocks.size > 1)
    assert(procs("main").entryBlock.isDefined)
    procs("main").returnBlock = block("retb", ret).resolve(p, "main")
    assert(procs("main").returnBlock.isDefined)
    procs("main").clearBlocks()

    blocks = p.labelToBlock
    procs = p.nameToProcedure
    assert(blocks.isEmpty)
    assert(procs("main").entryBlock.isEmpty)
    assert(procs("main").returnBlock.isEmpty)
  }

  test("interproc aftercall") {

    val p = prog(
      proc("p1", block("b1", LocalAssign(R0, bv64(10)), ret)),
      proc(
        "main",
        block("l_main", LocalAssign(R0, bv64(10)), directCall("p1"), goto("returntarget")),
        block("returntarget", ret)
      )
    )

    val rr1 = transforms.ReplaceReturns()
    cilvisitor.visit_prog(rr1, p)
    rr1.addR30Begins()
    transforms.addReturnBlocks(p)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), p)

    val rr2 = transforms.ReplaceReturns()
    cilvisitor.visit_prog(rr2, p)
    rr2.addR30Begins()
    transforms.addReturnBlocks(p)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), p)

    val blocks = p.labelToBlock
    val procs = p.nameToProcedure

    val next = InterProcIRCursor.succ(blocks("l_main").jump)
    val prev = InterProcIRCursor.pred(blocks("returntarget"))

    assert(
      prev.size == 1 && prev
        .collect { case c: GoTo =>
          c.parent == p.labelToBlock("l_main")
        }
        .contains(true)
    )

    // assert(next == Set(p.procs("p1"), p.labelToBlock("l_main").fallthrough.get))

    val prevB: Command = (blocks("l_main").statements.lastOption match
      case Some(c: IndirectCall) => c.returnTarget
      case Some(c: DirectCall) => c.returnTarget
      case o => None
    ).get

    assert(isAfterCall(prevB))
    assert(InterProcIRCursor.pred(prevB).size == 1)
  }

  test("replace jump") {
    val p = prog(
      proc("p1", block("b1", ret)),
      proc(
        "main",
        block("l_main", indirectCall(R1), goto("returntarget")),
        block("block2", directCall("p1"), goto("returntarget")),
        block("returntarget", ret)
      )
    )

    val blocks = p.labelToBlock
    val procs = p.nameToProcedure

    val main = blocks("l_main")
    val block2 = blocks("block2")

    val newJump = block2.jump

    main.replaceJump(newJump)

    assert(newJump.parent == main)
    assert(block2.jump.isInstanceOf[Unreachable])
  }

  test("proc iterator") {

    val p = prog(
      proc(
        "main",
        block("lmain", goto("lmain1")),
        block("lmain1", goto("lmain", "lmainret", "lmain3")),
        block("lmain3", goto("lmainret")),
        block("lmainret", ret)
      )
    )

    val blockOrder = p.mainProcedure.preOrderIterator.collect { case b: Block =>
      b.label
    }.toList

    // assert(blockOrder == List("lmain", "lmain1", "lmainret", "lmain3"))

    assert(blockOrder.head == "lmain")
    assert(blockOrder.tail.head == "lmain1")
    assert(blockOrder.tail.tail.take(2).toSet == Set("lmain3", "lmainret"))

  }

  test("dsl params") {

    val p = prog(
      proc("p1", Seq(("R0_in" -> BitVecType(64))), Seq(("R0_out", BitVecType(64))))(
        block("b1", ret("R0_out" -> LocalVar("R0_in", BitVecType(64))))
      ),
      proc("main", Seq(), Seq(("R0_out") -> BitVecType(64)))(
        block("l_main", indirectCall(R1), goto("returntarget")),
        block(
          "block2",
          directCall(Seq(("R0_out" -> R0)), "p1", Set("R0_in" -> BitVecLiteral(150, 64))),
          goto("returntarget")
        ),
        block("returntarget", ret("R0_out" -> BitVecLiteral(1, 64)))
      )
    )

    val p1 = p.procedures.find(_.procName == "p1").get
    val main = p.procedures.find(_.procName == "main").get

    assert(p1.formalInParam == SortedSet(LocalVar("R0_in", BitVecType(64))))
    assert(p1.formalOutParam == SortedSet(LocalVar("R0_out", BitVecType(64))))
    assert(main.formalInParam.isEmpty)
    assert(main.formalOutParam == SortedSet(LocalVar("R0_out", BitVecType(64))))

  }

  test("initial memory") {

    val program = prog(
      Seq(
        MemorySection(
          ".interp",
          BigInt("4194872"),
          110,
          Seq(
            0x2f, 0x6e, 0x69, 0x78, 0x2f, 0x73, 0x74, 0x6f, 0x72, 0x65, 0x2f, 0x61, 0x31, 0x33, 0x61, 0x31, 0x71, 0x69,
            0x32, 0x6b, 0x6e, 0x71, 0x61, 0x30, 0x73, 0x64, 0x31, 0x67, 0x76, 0x6d, 0x35, 0x78, 0x33, 0x34, 0x70, 0x72,
            0x35, 0x33, 0x77, 0x67, 0x30, 0x69, 0x6c, 0x2d, 0x67, 0x6c, 0x69, 0x62, 0x63, 0x2d, 0x61, 0x61, 0x72, 0x63,
            0x68, 0x36, 0x34, 0x2d, 0x75, 0x6e, 0x6b, 0x6e, 0x6f, 0x77, 0x6e, 0x2d, 0x6c, 0x69, 0x6e, 0x75, 0x78, 0x2d,
            0x67, 0x6e, 0x75, 0x2d, 0x32, 0x2e, 0x33, 0x38, 0x2d, 0x34, 0x34, 0x2f, 0x6c, 0x69, 0x62, 0x2f, 0x6c, 0x64,
            0x2d, 0x6c, 0x69, 0x6e, 0x75, 0x78, 0x2d, 0x61, 0x61, 0x72, 0x63, 0x68, 0x36, 0x34, 0x2e, 0x73, 0x6f, 0x2e,
            0x31, 0x0
          ).map(BitVecLiteral(_, 8)).toSeq,
          false,
          None
        )
      ),
      proc(
        "knownBitsExample_4196164",
        Seq("R0_in" -> BitVecType(64), "R1_in" -> BitVecType(64)),
        Seq("R0_out" -> BitVecType(64), "R2_out" -> BitVecType(64), "R3_out" -> BitVecType(64))
      )(
        block(
          "lknownBitsExample",
          LocalAssign(
            LocalVar("R2", BitVecType(64), 2),
            BinaryExpr(
              BVOR,
              BinaryExpr(
                BVAND,
                LocalVar("R0_in", BitVecType(64), 0),
                BitVecLiteral(BigInt("18374966859414961920"), 64)
              ),
              BitVecLiteral(BigInt("18446744069414584320"), 64)
            ),
            Some("%0000023e")
          ),
          LocalAssign(
            LocalVar("R0", BitVecType(64), 3),
            BinaryExpr(
              BVOR,
              BinaryExpr(
                BVAND,
                LocalVar("R0_in", BitVecType(64), 0),
                BitVecLiteral(BigInt("18374966859414961920"), 64)
              ),
              BitVecLiteral(BigInt("71777218305454335"), 64)
            ),
            Some("%00000257")
          ),
          ret(
            "R0_out" -> LocalVar("R0", BitVecType(64), 3),
            "R2_out" -> LocalVar("R2", BitVecType(64), 2),
            "R3_out" -> BitVecLiteral(BigInt("71777218305454335"), 64)
          )
        )
      )
    )

    assert(program.initialMemory.nonEmpty)
  }

  test("dsl procedure with self-recursive call") {
    // this should be correctly resolved
    val emptyprog = prog(proc("main"))
    val recursiveproc = proc("p2", block("b1", LocalAssign(R0, bv64(10)), directCall("p2"), goto("b1"))).cloneable

    // these calls should not throw
    assert(prog(recursiveproc) != null)
    assert(recursiveproc.addToProg(emptyprog) != null)
  }

  test("LambdaTypes") {
    val x = LocalVar("x", BitVecType(64))
    val y = LocalVar("y", BitVecType(8))
    val l = LambdaExpr(List(x, y), BinaryExpr(BVCONCAT, x, y))

    assert(l.getType == MapType(x.getType, MapType(y.getType, BitVecType(64 + 8))))

    assert(l.returnType == BitVecType(64 + 8))
    val (pt, rt) = curryFunctionType(l.getType)
    assert(rt == l.returnType)
    assert(pt == List(x.getType, y.getType))
  }

  test("AssocExpr") {
    val x = AssocExpr(BoolAND, List(LocalVar("x", BoolType), LocalVar("y", BoolType), LocalVar("y", BoolType)))
    assert(x.variables.map(_.name).toSet == Set("x", "y"))

    val s = Assert(x)

    val n = prog(proc("x", block("xin", s, ret)))

    val v = translating.FindVars()
    cilvisitor.visit_prog(v, n)
    assert(v.vars.map(_.name) == Set("x", "y"))
    assert(v.locals.map(_.name) == Set("x", "y"))
    assert(v.globals == Set())

  }

  test("dsl return block inference") {
    var p = prog(proc("hi")(block("entry", goto("retblock")), block("retblock", ret)))
    assertResult(Some("retblock"), "normal infer case")(p.procedures.head.returnBlock.map(_.label))

    p = prog(proc("hi")(block("entry", goto("retblock")), block("retblock", ret), block("retblock2", ret)))
    assertResult(None, "should fail because multiple returns")(p.procedures.head.returnBlock.map(_.label))

    p = prog(proc("hi")(block("entry", goto("entry"))))
    assertResult(None, "should fail because no returns")(p.procedures.head.returnBlock.map(_.label))

    p = prog(
      proc("hi", returnBlockLabel = Some("retblock"))(
        block("entry", goto("retblock")),
        block("retblock", ret),
        block("retblock2", ret)
      )
    )
    assertResult(Some("retblock"), "no infer because Some")(p.procedures.head.returnBlock.map(_.label))

    p = prog(proc("hi", returnBlockLabel = None)(block("entry", goto("retblock")), block("retblock", ret)))
    assertResult(None, "no infer because None")(p.procedures.head.returnBlock.map(_.label))

    assertThrows[Exception] {
      // throws because return block not found
      p =
        prog(proc("hi", returnBlockLabel = Some("DISAFJAD"))(block("entry", goto("retblock")), block("retblock", ret)))
    }
  }

}
