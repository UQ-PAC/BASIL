package translating.offlineLifter

import util.Logger
import ir.*
import ir.eval.BitVectorEval.*
import translating.TempIf
import collection.mutable.ArrayBuffer
import collection.mutable
import lifter.*

trait Builder[L] {

  def push_stmt(s: Statement): Unit

  def gen_branch(arg0: Expr): L
  def true_branch(arg0: L): L
  def false_branch(arg0: L): L
  def merge_branch(arg0: L): L

  def switch_ctx(arg0: L): Unit
  def defaultLabel: L
}

class NopBuilder extends Builder[String] {

  def gen_branch(arg0: Expr): String = ???
  def push_stmt(s: Statement): Unit = ???
  def true_branch(arg0: String): String = ???
  def false_branch(arg0: String): String = ???
  def merge_branch(arg0: String): String = ???
  def switch_ctx(arg0: String): Unit = ???
  def defaultLabel: String = "null"

}

class StmtListBuilder extends Builder[Int] {

  private var writing: Int = 0
  private val blocks = mutable.ArrayBuffer[mutable.ArrayBuffer[Statement]](mutable.ArrayBuffer.empty)
  private val branches = mutable.ArrayBuffer[Stmt](Block(0))

  sealed trait Stmt
  case class Block(statements: Int) extends Stmt
  case class Branch(cond: Expr, trueB: Int, falseB: Int, joinB: Int) extends Stmt

  def defaultLabel = -1

  def push_stmt(s: Statement) = {
    blocks(writing).append(s)
  }

  private def push_block() = {
    val s = blocks.size
    blocks.append(mutable.ArrayBuffer.empty)
    s
  }

  def gen_branch(cond: Expr) = {
    val branchID = branches.size
    val trueB = push_block()
    val falseB = push_block()
    val joinB = push_block()
    branches.append(Branch(cond, trueB, falseB, joinB))
    branchID
  }

  def false_branch(id: Int) = {
    branches(id) match {
      case b: Branch => b.falseB
      case _ => ???
    }
  }

  def true_branch(id: Int) = {
    branches(id) match {
      case b: Branch => b.trueB
      case _ => ???
    }
  }

  def merge_branch(id: Int) = {
    branches(id) match {
      case b: Branch => b.joinB
      case _ => ???
    }
  }

  def switch_ctx(id: Int) = {
    writing = id
  }

  def extract: Seq[Statement] = {
    branches.toSeq.flatMap {
      case Branch(cond, trueB, falseB, joinB) => Seq(TempIf(cond, blocks(trueB).toSeq, blocks(falseB).toSeq))
      case Block(id) => blocks(id).toSeq
    }
  }

}

object Lifter {

  def liftBlockBytes(ops: Iterable[Int], initialSp: BigInt): Seq[Seq[Statement]] = {
    var sp = initialSp
    ops.map { op =>
      val ins = if (op == 0xd503201f.toInt) {
        Seq()
      } else if (op == 0xd4207d00.toInt) {
        Seq(Assert(FalseLiteral, Some(s"aarch64_system_exceptions_debug_breakpoint (0x$op)")))
      } else {
        try {
          val lift = StmtListLifter()
          f_A64_decoder[Expr, Int, BitVecLiteral](lift, BitVecLiteral(BigInt(op), 32), BitVecLiteral(sp, 64))
          lift.extract.toSeq
        } catch {
          case e => {
            val o = "%x".format(op)
            val msg = (s"Lift failure $o : $e")
            Logger.error(msg /* + "\n" + e.getStackTrace.mkString("\n  ") */ )
            Seq(Assert(FalseLiteral, Some(msg)))
          }
        }
      }
      sp = sp + 32
      ins
    }.toSeq
  }

  def liftOpcode(op: BigInt, sp: BigInt): Seq[Statement] = {
    val lift = StmtListLifter()
    val dec = f_A64_decoder[Expr, Int, BitVecLiteral](lift, BitVecLiteral(op, 32), BitVecLiteral(sp, 64))
    println(dec)
    lift.extract
  }

}
