package ir.transforms

import ir.*
import ir.cilvisitor.*
import util.assertion.*

import scala.collection.mutable

class ReplaceReturns(insertR30InvariantAssertion: Procedure => Boolean = _ => true, doSimplify: Boolean = false)
    extends CILVisitor {

  private def R30BeginName(p: Procedure): String = s"R30_begin_${p.name}"

  private val R30procedures: mutable.Set[Procedure] = mutable.Set()

  /** Assumes IR with 1 call per block which appears as the last statement.
    */
  override def vstmt(j: Statement): VisitAction[List[Statement]] = {
    val procedure = j.parent.parent
    val assertR30Addr = insertR30InvariantAssertion(procedure)
    val R30Begin = LocalVar(R30BeginName(procedure), BitVecType(64))

    j match {
      case IndirectCall(r30 @ Register("R30", rt), _) =>
        debugAssert(j.parent.statements.lastOption.contains(j))
        j.parent.jump match {
          case _: Unreachable | _: Return =>
            j.parent.replaceJump(Return())

            if (assertR30Addr && doSimplify) {
              R30procedures.add(procedure)
              ChangeTo(List(Assert(BinaryExpr(EQ, r30, R30Begin), Some("is returning to caller-set R30"))))
            } else {
              ChangeTo(List())
            }
          case _ => SkipChildren()
        }
      case i: IndirectCall =>
        i.parent.jump match {
          case _: Unreachable =>
            i.parent.replaceJump(Return())
            if (assertR30Addr) {
              R30procedures.add(procedure)
              ChangeTo(List(Assert(BinaryExpr(EQ, Register("R30", 64), R30Begin), Some("R30 = R30_in")), i))
            } else {
              SkipChildren()
            }
          case _ => SkipChildren()
        }
      case d: DirectCall =>
        d.parent.jump match {
          // d.parent.jump == d.successor,  from singleprocend invariant
          // case (Some(l: LocalAssign), _) if l.lhs.name == "R30" && l.rhs.isInstanceOf[BitVecLiteral] => SkipChildren()
          // ^ we can resolve the exact return target if we are assigning a constant
          // If we can't find one
          case _: Unreachable =>
            if (d.target == procedure) {
              // recursive tail call
              d.parent.replaceJump(GoTo(procedure.entryBlock.get))
            } else {
              // non-recursive tail call
              d.parent.replaceJump(Return())
            }
            if (assertR30Addr) {
              R30procedures.add(procedure)
              ChangeTo(List(Assert(BinaryExpr(EQ, Register("R30", 64), R30Begin), Some("R30 = R30_in")), d))
            } else {
              SkipChildren()
            }
          case _ => SkipChildren()
        }
      case _ => SkipChildren()
    }
  }

  override def vjump(j: Jump): SkipChildren[Jump] = SkipChildren()

  def addR30Begins(): Unit = {
    for {
      p <- R30procedures
      b <- p.entryBlock
    } {
      val R30Begin = LocalVar(R30BeginName(p), BitVecType(64))
      b.statements.prepend(LocalAssign(R30Begin, Register("R30", 64)))
    }
  }

}

def addReturnBlocks(p: Program, toAll: Boolean = false): Unit = {
  p.procedures.foreach { p =>
    val returningBlocks = p.blocks.filter(_.jump.isInstanceOf[Return]).toList
    val containsReturn = returningBlocks.nonEmpty

    if (
      returningBlocks.size == 1 && !p.entryBlock.contains(returningBlocks.head)
      && returningBlocks.forall(_.statements.isEmpty)
    ) {
      p.returnBlock = returningBlocks.head
    } else {
      val returnBlockID = p.freshBlockId(p.procName + "_basil_return")
      val entryBlockID = p.freshBlockId(p.procName + "_basil_entry")

      if (toAll && p.blocks.isEmpty && p.entryBlock.isEmpty && p.returnBlock.isEmpty) {
        p.returnBlock = Block(label = returnBlockID, jump = Return())
        p.entryBlock = Block(label = entryBlockID, jump = GoTo(p.returnBlock.get))
      } else if (p.returnBlock.isEmpty && (toAll || containsReturn)) {
        p.returnBlock = p.addBlock(Block(label = returnBlockID, jump = Return()))
      }
    }
  }
}

class ConvertSingleReturn extends CILVisitor {

  /** Assumes procedures have defined return blocks if they contain a return statement.
    */
  override def vjump(j: Jump): VisitAction[Jump] = j match {
    case r: Return if !j.parent.parent.returnBlock.contains(j.parent) =>
      ChangeTo(GoTo(Seq(j.parent.parent.returnBlock.get)))
    case _ => SkipChildren()
  }

  override def vstmt(s: Statement): SkipChildren[List[Statement]] = SkipChildren()
}

/**
 * Establish procedure diamond structure with designated entry and return block which contain no statements.
 *
 * This is supposed to be idempotent, if it fails please report as a bug.
 */
def establishProcedureDiamondForm(program: Program, doSimplify: Boolean = false): Unit = {
  // FIXME: Main will often maintain the stack by loading R30 from the caller's stack frame
  //        before returning, which makes the R30 assertin faile. Hence we currently skip this
  //        assertion for main, instead we should precondition the stack layout before main
  //        but the interaction between spec and memory regions is nontrivial currently
  val replaceReturns = ReplaceReturns(proc => program.mainProcedure != proc, doSimplify)
  cilvisitor.visit_prog(replaceReturns, program)
  replaceReturns.addR30Begins()

  addReturnBlocks(program)
  cilvisitor.visit_prog(ConvertSingleReturn(), program)
  debugAssert(ir.invariant.programDiamondForm(program))
}
