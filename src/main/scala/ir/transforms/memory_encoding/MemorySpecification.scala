package ir.transforms.memoryEncoding

import util.MemoryEncodingRepresentation
import boogie.*
import ir.*
import ir.cilvisitor.*

case class MemoryEncodingTransform(val me: MemoryEncoding, val spec: MemorySpecification) extends CILVisitor {
  val helpers = MemorySpecificationHelpers(me)

  override def vproc(p: Procedure) = {
    spec.specifyProcedure(p, helpers)
    DoChildren()
  }

  override def vstmt(s: Statement) = {
    s match {
      case s: MemoryStore => {
        val size = BitVecLiteral(s.size / 8, 64);
        ChangeTo(
          List(
            Assert(me.validAccessCall(s.index, size), comment = Some("Requires Valid Memory")),
            MemoryStore(helpers.mem, s.index, s.value, s.endian, s.size, s.label)
          )
        )
      }
      case s: MemoryLoad => {
        val size = BitVecLiteral(s.size / 8, 64);
        ChangeTo(
          List(
            Assert(me.validAccessCall(s.index, size), comment = Some("Requires Valid Memory")),
            MemoryLoad(s.lhs, helpers.mem, s.index, s.endian, s.size, s.label)
          )
        )
      }
      case _ => {
        DoChildren()
      }
    }
  }
}

class MemorySpecification() {
  def specifyProcedure(p: Procedure, h: MemorySpecificationHelpers) = {
    p.procName match {
      case "main" => specifyMain(p, h)
      case "malloc" => specifyMalloc(p, h)
      case "free" => specifyFree(p, h)
      case _ => {}
    }
  }

  def specifyMain(p: Procedure, h: MemorySpecificationHelpers) = {
    p.requiresExpr ++= List(
      BinaryExpr(
        EQ,
        BinaryExpr(BVAND, BitVecLiteral(0xff00, 64), BitVecLiteral(0xff00, 64)),
        BitVecLiteral(0xff00, 64)
      ),
      BinaryExpr(
        EQ,
        BinaryExpr(BVSHL, BitVecLiteral(0xffffffffL, 64), BitVecLiteral(32, 64)),
        BinaryExpr(BVSHL, BitVecLiteral(0xffffffffL, 64), BitVecLiteral(32, 64))
      ),
      BinaryExpr(BVULE, BitVecLiteral(0, 64), BitVecLiteral(1, 64)),
      BinaryExpr(BVUGT, BitVecLiteral(1, 64), BitVecLiteral(0, 64)),
      h.me.initHeapCall()
    )
  }

  def specifyMalloc(p: Procedure, h: MemorySpecificationHelpers) = {
    p.modifies ++= Set(h.me.gMemEncoding)
    if h.me.simplify then p.modifies ++= Set(Register("R0", h.me.addressSize))

    p.requires ++= List(BinaryBExpr(EQ, h.preR(0).toGamma, TrueBLiteral))

    p.requiresExpr ++= List(
      // QuantifierExpr(
      //   QuantifierSort.exists,
      //   LambdaExpr(List(h.i), h.me.canAllocateCall(h.i, h.preR(0), me = h.me.gMemEncoding))
      // )
    )

    p.ensures ++= List(BinaryBExpr(EQ, h.postR(0).toGamma, TrueBLiteral))

    p.ensuresExpr ++= List(
      // We are allowed to allocate r0 with size old(r0)
      h.me.canAllocateCall(h.postR(0), h.oldR(0), me = OldExpr(h.me.gMemEncoding)),
      // Ensure offset of return address r(0) is 0
      BinaryExpr(EQ, h.me.addrOffsetCall(h.postR(0)), h.me.addressLiteral(0)),
      // Ensure base of return allocation is r(0)
      BinaryExpr(EQ, h.me.allocBaseCall(h.me.addrAllocCall(h.postR(0))), h.postR(0)),
      // Update the memory encoding:
      BinaryExpr(
        EQ,
        h.me.gMemEncoding,
        // To allocate old(r0) space at r(0) address
        h.me.allocateCall(h.postR(0), h.oldR(0), OldExpr(h.me.gMemEncoding))
      )
    )
  }

  def specifyFree(p: Procedure, h: MemorySpecificationHelpers) = {
    p.modifies ++= Set(h.me.gMemEncoding)

    p.requiresExpr ++= List(
      // Only free a heap value
      h.me.addrIsHeapCall(h.preR(0)),
      // Free only at base of region:
      BinaryExpr(EQ, h.me.addrOffsetCall(h.preR(0)), BitVecLiteral(0, 64)),
      // Object must be alive:
      BinaryExpr(EQ, h.me.allocLiveCall(h.me.addrAllocCall(h.preR(0))), h.me.liveLiteral)
    )

    val inBounds = BinaryBExpr(
      BoolAND,
      BinaryBExpr(BVUGE, h.i.toBoogie, h.me.allocBaseBCall(h.me.addrAllocBCall(h.preR(0).toBoogie))),
      BinaryBExpr(
        BVULT,
        h.i.toBoogie,
        BinaryBExpr(
          BVADD,
          h.me.allocBaseBCall(h.me.addrAllocBCall(h.preR(0).toBoogie)),
          h.me.allocSizeBCall(h.me.addrAllocBCall(h.preR(0).toBoogie))
        )
      )
    )
    p.requires ++= List(
      ForAll(
        List(h.i.toBoogie),
        BinaryBExpr(
          BoolIMPLIES,
          inBounds,
          BinaryBExpr(EQ, MapAccess(h.mem.toGamma, h.i.toBoogie), TrueBLiteral)

          // in_bounds(addrBase(pre_r(0)), BinaryExpr(BVADD, addrBase(pre_r(0)), allocSize(pre_r(0))), i).toBoogie,
          // (if mer.splitMem then {
          //    BinaryBExpr(
          //      EQ,
          //      ExprMapAccess(MapAccess(split_mem.toGamma, bAddrBase(i.toBoogie)), bAddrOffset(i.toBoogie), BoolBType),
          //      TrueBLiteral
          //    )
          //  } else {
          //    BinaryBExpr(EQ, MapAccess(mem.toGamma, i.toBoogie), TrueBLiteral)
          //  })
        )
      )
    )
  }
}

// trait MemorySpecificationCore extends MemorySpecification {
//   override def specifyProcedure(p: Procedure, h: MemorySpecificationHelpers) = {
//     p.procName match {
//       case "main" => specifyMain(p, h)
//       case "malloc" => specifyMalloc(p, h)
//       case "free" => specifyFree(p, h)
//       case _ => {}
//     }
//   }

//   def specifyMain(p: Procedure, h: MemorySpecificationHelpers) = {}

//   def specifyMalloc(p: Procedure, h: MemorySpecificationHelpers) = {
//     p.modifies ++= Set(h.me.gMemEncoding)
//     if h.me.simplify then p.modifies ++= Set(Register("R0", h.me.addressSize))

//     p.requires ++= List(BinaryBExpr(EQ, h.preR(0).toGamma, TrueBLiteral))

//     p.requiresExpr ++= List(
//       QuantifierExpr(QuantifierSort.exists, LambdaExpr(List(h.i), h.me.canAllocateCall(h.i, h.preR(0), me=h.me.gMemEncoding)))
//     )

//     p.ensures ++= List(BinaryBExpr(EQ, h.postR(0).toGamma, TrueBLiteral))

//     p.ensuresExpr ++= List(
//       // BinaryExpr(BVUGT, BinaryExpr(BVADD, r(0), old_r(0)), r(0)),
//       // // Size mapping for returned pointer matches input size:
//       // sizeUpdate(r(0), old_r(0)),
//       // // Returned address is on formerly fresh space:
//       // BinaryExpr(EQ, allocLive(r(0), map = OldExpr(mem_encoding)), fresh_bv),
//       // // Returned address is on now live space:
//       // liveUpdate(r(0), live_bv),
//       // // All other lives are unchanged:
//       // // Address is on heap:
//       // addrIsHeap(r(0), map = OldExpr(mem_encoding)),

//       // We are allowed to allocate r0 with size old(r0)
//       h.me.canAllocateCall(h.postR(0), h.oldR(0), me=OldExpr(h.me.gMemEncoding)),
//       // Ensure offset of return address r(0) is 0
//       BinaryExpr(EQ, h.me.addrOffsetCall(h.postR(0)), h.me.addressLiteral(0)),
//       // Ensure base of return allocation is r(0)
//       BinaryExpr(EQ, h.me.allocBaseCall(h.me.addrAllocCall(h.postR(0))), h.postR(0)),
//       // Update the memory encoding:
//       BinaryExpr(EQ, h.me.gMemEncoding,
//         // To allocate old(r0) space at r(0) address
//         h.me.allocateCall(h.postR(0), h.oldR(0), OldExpr(h.me.gMemEncoding))
//       ),
//     )
//   }

//   def specifyFree(p: Procedure, h: MemorySpecificationHelpers) = {}
// }

// // trait MemorySpecificationStrings extends MemorySpecification {
// //   override def specifyProcedure(p: Procedure, repr: MemoryEncodingRepresentation, simplify: Boolean) = {
// //     p.procName match {
// //       case "strlen" => specifyStrlen(p)
// //       case "memcpy" => specifyMemcpy(p)
// //       case "memset" => specifyMemset(p)
// //       case _ => {}
// //     }
// //   }
// //   def specifyStrlen(p: Procedure) = {}
// //   def specifyMemcpy(p: Procedure) = {}
// //   def specifyMemset(p: Proceudre) = {}
// // }

private case class MemorySpecificationHelpers(val me: MemoryEncoding) {
  // For use in postconditions. rn, or rn_out if simplify
  def postR(n: Int) =
    if me.simplify then LocalVar(s"R${n}_out", BitVecType(me.addressSize)) else Register(s"R$n", me.addressSize)

  // For use in postconditions (obviously). old(rn), or rn_in if simplify
  def oldR(n: Int) =
    if me.simplify then LocalVar(s"R${n}_in", BitVecType(me.addressSize)) else OldExpr(Register(s"R$n", me.addressSize))
  def oldGammaR(n: Int) =
    if me.simplify then LocalVar(s"R${n}_in", BitVecType(me.addressSize)).toGamma
    else Old(Register(s"R$n", me.addressSize).toGamma)

  // For use in preconditions. rn, rn_in if simplify
  def preR(n: Int) =
    if me.simplify then LocalVar(s"R${n}_in", BitVecType(me.addressSize)) else Register(s"R$n", me.addressSize)

  // (always try to use mem, unsplit/split should rarely be necessary)
  val unsplitMem = SharedMemory("mem", 64, 8)
  val splitMem = SharedMemorySplit("split_mem", 64, 8)
  val mem = (if me.splitMem then splitMem else unsplitMem)

  val i = LocalVar("i", BitVecType(64))
}
