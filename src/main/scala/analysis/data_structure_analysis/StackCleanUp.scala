package analysis.data_structure_analysis

import ir.eval.BitVectorEval.{bv2SignedInt, isNegative}
import ir.{
  BVADD,
  BVAND,
  BVASHR,
  BVBinOp,
  BVCOMP,
  BVCONCAT,
  BVEQ,
  BVLSHR,
  BVMUL,
  BVNAND,
  BVNEQ,
  BVNOR,
  BVOR,
  BVSDIV,
  BVSGE,
  BVSGT,
  BVSHL,
  BVSLE,
  BVSLT,
  BVSMOD,
  BVSREM,
  BVSUB,
  BVUDIV,
  BVUGE,
  BVUGT,
  BVULE,
  BVULT,
  BVUREM,
  BVXNOR,
  BVXOR,
  BinaryExpr,
  BitVecLiteral,
  BitVecType,
  Block,
  BoolBinOp,
  Command,
  DirectCall,
  Expr,
  Extract,
  IntBinOp,
  Jump,
  Literal,
  LocalAssign,
  LocalVar,
  MemoryLoad,
  Procedure,
  Program,
  Register,
  Repeat,
  SignExtend,
  Statement,
  UnaryExpr,
  UninterpretedFunction,
  Variable,
  ZeroExtend
}
import ir.transforms.{AbstractDomain, PowerSetDomain, ProcedureSummaryGenerator}
import util.StackLogger as Logger

import scala.annotation.tailrec
import scala.collection.mutable

enum StackMaintained {
  case unsure
  case maintained
  case clobbered
}

enum StackStatus(maintained: Boolean = false, size: Option[Int] = None) {
  case Unmaintained extends StackStatus
  case MaintainedNoSize extends StackStatus(true)
  case MaintainedWithSize(size: Int) extends StackStatus(true, Some(size))
  case Bot extends StackStatus(true, Some(0))

  def join(other: StackStatus): StackStatus = {
    (this, other) match
      case (Unmaintained, _) => Unmaintained
      case (_, Unmaintained) => Unmaintained
      case (MaintainedNoSize, _) => MaintainedNoSize
      case (_, MaintainedNoSize) => MaintainedNoSize
      case (MaintainedWithSize(size1), MaintainedWithSize(size2)) =>
        MaintainedWithSize(math.max(size1, size2))
  }

  def move(f: Int => Int): StackStatus = {
    this match
      case StackStatus.MaintainedWithSize(size) => MaintainedWithSize(f(size))
      case ss => ss
  }
}

object StackStatus {
  def join(a: StackStatus, b: StackStatus): StackStatus = a.join(b)
}

def getStackSize(proc: Procedure, sva: SymbolicValues): Unit = {
  val stackPointers = sva.getSorted("R31")
  if stackPointers.nonEmpty then
    val init = stackPointers(stackPointers.firstKey)
    val end = stackPointers(stackPointers.lastKey)
    Logger.debug(s"found init $init and final $end")
  else Logger.debug(s"found R31 unchanged")
}

class StackStatusDomain(val isNotMaintained: String => Boolean, val proc: Procedure, val symVals: SymbolicValues)
    extends AbstractDomain[StackStatus] {
  val replacements: mutable.Map[LocalVar, LocalVar] = mutable.Map.empty
  def replace(in: LocalVar): LocalVar = {
    var cur = in
    while replacements.contains(cur) do cur = replacements(cur)
    cur
  }
  override def init(b: Block): StackStatus = StackStatus.Bot
  override def join(a: StackStatus, b: StackStatus, pos: Block): StackStatus = a.join(b)

  override def transfer(a: StackStatus, b: Command): StackStatus = {
    b match
      case load @ MemoryLoad(_, _, index, _, _, _) => ???
//        symVals.exprToSymValSet(index, identity, this.replace).getStack()
      case _ => ???
  }

  override def top: StackStatus = ???

  override def bot: StackStatus = ???
}

def estimateStackSize(proc: Procedure): Int = {
  val size = proc.collectFirst {
    case LocalAssign(_, BinaryExpr(BVADD, Register("R31", 64), size: BitVecLiteral), _) if isNegative(size) =>
      -bv2SignedInt(size).toInt
  }

  if size.isEmpty then 0 else size.get
}

def estimateStackSizes(program: Program): Map[Procedure, Int] = {

  program.procedures.foldLeft(Map[Procedure, Int]()) { case (m, proc) =>
    m + (proc -> estimateStackSize(proc))
  }
}
