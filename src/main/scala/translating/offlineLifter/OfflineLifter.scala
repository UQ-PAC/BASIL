package translating.offlineLifter

import ir.*
import ir.eval.BitVectorEval.*
import lifter.*
import translating.TempIf
import util.Logger

import collection.mutable.ArrayBuffer
import collection.mutable

trait Builder[L] {

  def fresh_local: String

  def push_stmt(s: Statement): Unit

  def gen_branch(arg0: Expr): L
  def true_branch(arg0: L): L
  def false_branch(arg0: L): L
  def merge_branch(arg0: L): L

  def switch_ctx(arg0: L): Unit
  def defaultLabel: L
}

class StmtListBuilder extends Builder[Int] {

  var pcValue = BigInt(0)

  private val localCounter = util.Counter()
  def fresh_local = {
    s"var${localCounter.next()}_${pcValue}"
  }

  sealed trait Stmt:
    var next: Option[Int] = None

  case class Block(statements: mutable.ArrayBuffer[Statement]) extends Stmt
  case class Branch(cond: Expr, trueB: Int, falseB: Int, joinB: Int) extends Stmt

  private var writing: Int = 0
  private val branches = mutable.ArrayBuffer[Stmt](Block(mutable.ArrayBuffer.empty))

  def defaultLabel = -1

  def push_stmt(s: Statement) = {
    branches(writing) match {
      case Block(x) => x.append(s)
      case _ => ???
    }
  }

  private def push_block() = {
    val s = branches.size
    branches.append(Block(mutable.ArrayBuffer.empty))
    s
  }

  def gen_branch(cond: Expr) = {
    val trueB = push_block()
    val falseB = push_block()
    val joinB = push_block()
    val branchID = branches.size
    branches.append(Branch(cond, trueB, falseB, joinB))
    branches(writing).next = Some(branchID)
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

  def extract(i: Int): Seq[Statement] = {
    branches(i) match {
      case bl @ Block(b) => b.toSeq ++ bl.next.toSeq.flatMap(extract)
      case bl @ Branch(cond, trueB, falseB, joinB) =>
        Seq(TempIf(cond, extract(trueB), extract(falseB))) ++ extract(joinB) ++ bl.next.toSeq.flatMap(extract)
    }
  }

  def extract: Seq[Statement] = {
    extract(0)
  }

}

class StmtListLifter extends LifterIFace[Int] {
  val builder = StmtListBuilder()
  def b: Builder[Int] = builder
  def extract: Seq[Statement] = builder.extract

}

object Lifter {

  def liftBlockBytes(ops: Iterable[Int], initialSp: BigInt): Seq[Seq[Statement]] = {

    var sp = initialSp
    ops.toSeq.map { op =>
      val ins = if (op == 0xd503201f.toInt) {
        Seq()
      } else if (op == 0xd4207d00.toInt) {
        Seq(Assert(FalseLiteral, Some(s"aarch64_system_exceptions_debug_breakpoint (0x$op)")))
      } else {
        try {
          val checker = ir.invariant.ReadUninitialised()
          val lift = StmtListLifter()
          lift.builder.pcValue = sp
          f_A64_decoder[Expr, Int, BitVecLiteral](lift, BitVecLiteral(BigInt(op), 32), BitVecLiteral(sp, 64))
          val stmts = lift.extract.toSeq
          checker.readUninitialised(stmts)
          val r = checker.getResult()
          if (r.isDefined) throw Exception(r.get + "\n" + stmts.mkString("\n"))
          stmts
        } catch {
          case e => {
            val o = "%x".format(op)
            val msg = (s"Lift failure $o : $e")
            Logger.error(msg + "\n" + e.getStackTrace.mkString("\n  "))
            Seq(Assert(FalseLiteral, Some(msg)))
          }
        }
      }
      sp = sp + 32
      ins
    }
  }

  def liftOpcode(op: BigInt, sp: BigInt): Seq[Statement] = {
    val lift = StmtListLifter()
    val dec = f_A64_decoder[Expr, Int, BitVecLiteral](lift, BitVecLiteral(op, 32), BitVecLiteral(sp, 64))
    println(dec)
    lift.extract
  }

}
