package ir

import scala.collection.mutable
import scala.collection.immutable._
import org.scalatest.funsuite.AnyFunSuite
import intrusivelist.{IntrusiveList, IntrusiveListElement}


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

