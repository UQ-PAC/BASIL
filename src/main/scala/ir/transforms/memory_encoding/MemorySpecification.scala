package ir.transforms.memoryEncoding

import util.MemoryEncodingRepresentation
import boogie.*
import ir.*

case class MemoryEncodingTransform(
  val specs: List[MemorySpecification],
  val me: MemoryEncoding
) extends CILVisitor {
  private val helpers = Helpers(me)
  override def vproc(p: Procedure) = {
    for (s <- specs) {
      s.specifyProcedure(p, helpers)
    }
    DoChildren()
  }
}

trait MemorySpecification {
  def specifyProcedure(p: Procedure, h: Helpers) = {}
}

trait MemorySpecificationCore extends MemorySpecification {
  override def specifyProcedure(p: Procedure, h: Helpers) = {
    p.procName match {
      case "main" => specifyMain(p, h)
      case "malloc" => specifyMalloc(p, h)
      case "free" => specifyFree(p, h)
      case _ => {}
    }
  }

  def specifyMain(p: Procedure, h: Helpers) = {}

  def specifyMalloc(p: Procedure, h: Helpers) = {
    p.modifies ++= Set(h.me.memEncoding)
    if h.simplify then p.modifies ++= Set(h.preR(0))

    p.requires ++= List(BinaryBExpr(EQ, h.preR(0).toGamma, TrueBLiteral))

    p.requiresExpr ++= List(
      QuantifierExpr(QuantifierSort.exists, LambdaExpr(List(h.i), h.canAllocate(h.i, h.preR(0), map=h.me.memEncoding)))
    )

    p.ensures ++= List(BinaryBExpr(EQ, h.r(0).toGamma, TrueBLiteral))

    p.ensuresExpr ++= List(
      // BinaryExpr(BVUGT, BinaryExpr(BVADD, r(0), old_r(0)), r(0)),
      // // Size mapping for returned pointer matches input size:
      // sizeUpdate(r(0), old_r(0)),
      // // Returned address is on formerly fresh space:
      // BinaryExpr(EQ, allocLive(r(0), map = OldExpr(mem_encoding)), fresh_bv),
      // // Returned address is on now live space:
      // liveUpdate(r(0), live_bv),
      // // All other lives are unchanged:
      // // Address is on heap:
      // addrIsHeap(r(0), map = OldExpr(mem_encoding)),
      
      // We are allowed to allocate r0 with size old(r0)
      h.me.canAllocateCall(h.r(0), h.oldR(0), map=OldExpr(h.me.memEncoding)),
      // Ensure offset of return address r(0) is 0
      BinaryExpr(EQ, h.me.addrOffsetCall(h.r(0)), h.me.addrLiteral(0)),
      // Ensure base of return allocation is r(0)
      BinaryExpr(EQ, h.me.allocBaseCall(h.me.addrAllocCall(h.r(0))), h.r(0)),
      // Update the memory encoding:
      BinaryExpr(EQ, h.me.memEncoding,
        // To allocate old(r0) space at r(0) address
        h.allocate(h.r(0), h.oldR(0), OldExpr(h.me.memEncoding))
      ),
    )
  }

  def specifyFree(p: Procedure, h: Helpers) = {}
}

// trait MemorySpecificationStrings extends MemorySpecification {
//   override def specifyProcedure(p: Procedure, repr: MemoryEncodingRepresentation, simplify: Boolean) = {
//     p.procName match {
//       case "strlen" => specifyStrlen(p)
//       case "memcpy" => specifyMemcpy(p)
//       case "memset" => specifyMemset(p)
//       case _ => {}
//     }
//   }
//   def specifyStrlen(p: Procedure) = {}
//   def specifyMemcpy(p: Procedure) = {}
//   def specifyMemset(p: Proceudre) = {}
// }

private case class Helpers(val me: MemoryEncoding) {
  def me: MemoryEncoding = me

  // For use in postconditions. rn, or rn_out if simplify
  def postR(n: Int) = if me.simplify then LocalVar(s"R${n}_out", BitVecType(64)) else Register(s"R$n", 64)

  // For use in postconditions (obviously). old(rn), or rn_in if simplify
  def oldR(n: Int) = if me.simplify then LocalVar(s"R${n}_in", BitVecType(64)) else OldExpr(Register(s"R$n", 64))
  def oldGammaR(n: Int) =
    if me.simplify then LocalVar(s"R${n}_in", BitVecType(64)).toGamma else Old(Register(s"R$n", 64).toGamma)

  // For use in preconditions. rn, rn_in if simplify
  def preR(n: Int) = if me.simplify then LocalVar(s"R${n}_in", BitVecType(64)) else Register(s"R$n", 64)

  // (always try to use mem, unsplit/split should rarely be necessary)
  val unsplitMem = SharedMemory("mem", 64, 8)
  val splitMem = SharedMemorySplit("split_mem", 64, 8)
  val mem = (if me.repr == MemoryEncodingRepresentation.Split(splitMem = true) then unsplit_mem else split_mem)

  val i = LocalVar("i", BitVecType(64))
}

