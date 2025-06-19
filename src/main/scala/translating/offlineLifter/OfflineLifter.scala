 package translating.offlineLifter

import util.Logger
import ir.*
import ir.eval.BitVectorEval.*
import collection.mutable.ArrayBuffer
import collection.mutable
import lifter.*
import ir.dsl.*
import lifter.*
import translating.TempIf

trait Builder[L] {

  def push_stmt(s: Statement) : Unit

  def gen_branch(arg0: Expr) : L
  def true_branch(arg0: L): L
  def false_branch(arg0: L): L
  def merge_branch(arg0: L): L

  def switch_ctx(arg0: L): Unit
  def defaultLabel : L
}


class NopBuilder extends Builder[String] {

  def gen_branch(arg0: Expr) : String = ???
  def push_stmt(s: Statement) : Unit = ???
  def true_branch(arg0: String): String = ???
  def false_branch(arg0: String): String = ???
  def merge_branch(arg0: String): String = ???
  def switch_ctx(arg0: String): Unit = ???
  def defaultLabel : String = "null"

}

object LoadExpr {

  def apply(addr: Expr, size: Int) = {
    UninterpretedFunction("load", Seq(addr, IntLiteral(size)), BitVecType(size))
  }

  def unapply(e: Expr) = e match {
    case UninterpretedFunction("load", Seq(addr, IntLiteral(size)), BitVecType(ts)) =>
      Some(addr, size)
    case _ => None
  }
}

class StmtListBuilder extends Builder[Int] {

  private var writing : Int = 0
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
      case b : Branch => b.falseB
      case _ => ???
    }
  }

  def true_branch(id: Int) = {
    branches(id) match {
      case b : Branch => b.trueB
      case _ => ???
    }
  }

  def merge_branch(id: Int) = {
    branches(id) match {
      case b : Branch => b.joinB
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


trait LifterIFace[L] extends LiftState[Expr, L, BitVecLiteral] {

  def b: Builder[L]

  def rTExprDefault: ir.Expr = null
  def rTLabelDefault: L = b.defaultLabel
  def rTSymDefault: ir.Expr = null

  val endian = Endian.LittleEndian
  val memory = SharedMemory("mem", 64, 8)

  /* Lift-time semantics */
  def mkBits(n: BigInt, y: BigInt): BitVecLiteral = {
    require(n >= 0)
    BitVecLiteral(extract(y, n), n.toInt)
  }
  def bvextract(e: BitVecLiteral, lo: BigInt, width: BigInt): BitVecLiteral = {
    smt_extract((lo + width - 1).toInt, lo.toInt, e)
  }


  def f_eq_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean = (smt_bveq(x, y))
  def f_ne_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean = (!smt_bveq(x, y))
  def f_add_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvadd(x, y))
  def f_sub_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvsub(x, y))
  def f_mul_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvmul(x, y))
  def f_and_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvand(x, y))
  def f_or_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvor(x, y))
  def f_eor_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): BitVecLiteral = (smt_bvxor(x, y))
  def f_not_bits(t: BigInt, x: BitVecLiteral): BitVecLiteral = (smt_bvnot(x))
  def f_slt_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean = smt_bvslt(x, y)
  def f_sle_bits(t: BigInt, x: BitVecLiteral, y: BitVecLiteral): Boolean = smt_bvsle(x, y)
  def f_zeros_bits(w: BigInt): BitVecLiteral = BitVecLiteral(0, w.toInt)
  def f_ones_bits(w: BigInt): BitVecLiteral = BitVecLiteral(BigInt(2).pow(w.toInt) - 1, w.toInt)
  def f_ZeroExtend(t0: BigInt, t1: BigInt, n: BitVecLiteral, x: BigInt): BitVecLiteral = smt_zero_extend(x.toInt - n.size, n)
  def f_SignExtend(t0: BigInt, t1: BigInt, n: BitVecLiteral, x: BigInt): BitVecLiteral = smt_sign_extend(x.toInt - n.size, n)
  def f_asr_bits(targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BitVecLiteral): BitVecLiteral = smt_bvashr(arg0, arg1)
  def f_lsl_bits(targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BitVecLiteral): BitVecLiteral =
    smt_bvshl(arg0, BitVecLiteral(arg1.value, arg0.size))
  def f_lsr_bits(targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BitVecLiteral): BitVecLiteral =
    smt_bvlshr(arg0, BitVecLiteral(arg1.value , arg0.size))

  def f_decl_bool(arg0: String): Expr = LocalVar(arg0, BoolType)
  def f_decl_bv(arg0: String, arg1: BigInt): Expr = LocalVar(arg0, BitVecType(arg1.toInt))
  def f_AtomicEnd(): Expr = LocalVar("ATOMICEND", BoolType)
  def f_AtomicStart(): Expr = LocalVar("ATOMICSTART", BoolType)

  def f_replicate_bits(targ0: BigInt, targ1: BigInt, arg0: BitVecLiteral, arg1: BigInt): BitVecLiteral = {
    def bv_replicate(value: BitVecLiteral, times: Int): BitVecLiteral = {
      var walk = BitVecLiteral(0, 0)
      for (i <- 1 to times) {
        walk = smt_concat(value, walk)
      }
      walk
    }

    bv_replicate(arg0, arg1.toInt)
  }
  def f_append_bits(targ0: BigInt, targ1: BigInt, a: BitVecLiteral, b: BitVecLiteral): BitVecLiteral =
    BitVecLiteral((a.value << b.size) + b.value, (a.size + b.size))

  /** Run-time IR program generation */

  def f_gen_BFAdd(arg0: Expr, arg1: Expr): Expr = throw NotImplementedError()
  def f_gen_BFMul(arg0: Expr, arg1: Expr): Expr = throw NotImplementedError()
  def f_gen_FPAdd(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPCompare(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Expr =
    throw NotImplementedError()
  def f_gen_FPCompareEQ(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPCompareGE(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPCompareGT(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPConvert(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr =
    throw NotImplementedError()
  def f_gen_FPConvertBF(arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPDiv(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPMax(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPMaxNum(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPMin(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPMinNum(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPMul(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPMulAdd(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Expr =
    throw NotImplementedError()
  def f_gen_FPMulAddH(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Expr =
    throw NotImplementedError()
  def f_gen_FPMulX(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPRSqrtStepFused(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = throw NotImplementedError()
  def f_gen_FPRecipEstimate(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = throw NotImplementedError()
  def f_gen_UnsignedRSqrtEstimate(targ0: BigInt, arg0: Expr): Expr = throw NotImplementedError()
  def f_gen_FPRSqrtEstimate(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = throw NotImplementedError()
  def f_gen_FPRecipStepFused(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = throw NotImplementedError()
  def f_gen_FPRecpX(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = throw NotImplementedError()
  def f_gen_FPRoundInt(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Expr =
    throw NotImplementedError()
  def f_gen_FPRoundIntN(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr, arg3: Expr): Expr =
    throw NotImplementedError()
  def f_gen_FPSqrt(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = throw NotImplementedError()
  def f_gen_FPSub(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_FPToFixed(
    targ0: BigInt,
    targ1: BigInt,
    arg0: Expr,
    arg1: Expr,
    arg2: Expr,
    arg3: Expr,
    arg4: Expr
  ): Expr = throw NotImplementedError()
  def f_gen_FPToFixedJS_impl(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr =
    throw NotImplementedError()
  def f_gen_FixedToFP(
    targ0: BigInt,
    targ1: BigInt,
    arg0: Expr,
    arg1: Expr,
    arg2: Expr,
    arg3: Expr,
    arg4: Expr
  ): Expr = throw NotImplementedError()
  def f_gen_bit_lit(targ0: BigInt, arg0: BitVecLiteral): Expr = BitVecLiteral(arg0.value, targ0.toInt)
  def f_gen_bool_lit(arg0: Boolean): Expr = if arg0 then BitVecLiteral(1, 1) else BitVecLiteral(0, 1)
  def f_gen_branch(arg0: Expr): L = b.gen_branch(arg0)
  def f_cvt_bits_uint(targ0: BigInt, arg0: BitVecLiteral): BigInt = arg0.value
  def f_gen_cvt_bits_uint(targ0: BigInt, arg0: Expr): Expr = arg0
  def f_gen_cvt_bool_bv(arg0: Expr): Expr = arg0
  def f_gen_eor_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(EQ, arg0, arg1)
  def f_gen_eq_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(EQ, arg0, arg1)
  def f_gen_eq_enum(arg0: Expr, arg1: Expr): Expr = BinaryExpr(EQ, arg0, arg1)
  def f_gen_int_lit(arg0: BigInt): BitVecLiteral = BitVecLiteral(arg0, 1123)

  def f_gen_store(lval: Expr, e: Expr): Unit = lval match
    case v: Variable => {
      val stmt = e match {
        case LoadExpr(addr, size) => {
          MemoryLoad(v, memory, addr, endian, size.toInt)
        } case _ => {
          LocalAssign(v, e)
        }
      }
      b.push_stmt(stmt)
    }
    case m => throw NotImplementedError(s"fail assign $m")
  def f_gen_load(e: Expr): Expr = e

  def f_gen_SignExtend(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: BitVecLiteral): Expr = {
    val oldSize = (targ0)
    val newSize = (targ1)
    if (arg1.value != newSize) {
      throw Exception()
    }
    SignExtend((newSize - oldSize).toInt, arg0)
  }
  def f_gen_ZeroExtend(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: BitVecLiteral): Expr = {
    val oldSize = (targ0)
    val newSize = (targ1)
    if (arg1.value != newSize) {
      throw Exception()
    }
    ZeroExtend((newSize - oldSize).toInt, arg0)
  }
  def f_gen_add_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(BVADD, arg0, arg1)
  def f_gen_and_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(BVAND, arg0, arg1)

  def f_gen_and_bool(arg0: Expr, arg1: Expr): Expr = BinaryExpr(BoolAND, arg0, arg1)
  def f_gen_asr_bits(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: Expr): Expr =
    BinaryExpr(BVASHR, arg0, gen_zero_extend_to(targ0, arg1))
  def f_gen_lsl_bits(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: Expr): Expr =
    BinaryExpr(BVSHL, arg0, gen_zero_extend_to(targ0, arg1))
  def f_gen_lsr_bits(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: Expr): Expr =
    BinaryExpr(BVLSHR, arg0, gen_zero_extend_to(targ0, arg1))
  def f_gen_mul_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(BVMUL, arg0, arg1)
  def f_gen_ne_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = UnaryExpr(BoolNOT, BinaryExpr(EQ, arg0, arg1))
  def f_gen_not_bits(targ0: BigInt, arg0: Expr): Expr = arg0.getType match {
    case BoolType => UnaryExpr(BoolNOT, arg0)
    case BitVecType(_) => UnaryExpr(BVNOT, arg0)
    case _: MapType => throw IllegalArgumentException()
    case IntType => throw IllegalArgumentException()
  }

  def f_gen_not_bool(arg0: Expr): Expr = arg0.getType match {
    case BoolType => UnaryExpr(BoolNOT, arg0)
    case BitVecType(sz) => BinaryExpr(NEQ, BitVecLiteral(0, sz), arg0)
    case _: MapType => throw IllegalArgumentException()
    case IntType => throw IllegalArgumentException()
  }

  def f_gen_or_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(BVOR, arg0, arg1)
  def f_gen_or_bool(arg0: Expr, arg1: Expr): Expr = BinaryExpr(BVOR, arg0, arg1)
  def f_gen_sdiv_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(BoolOR, arg0, arg1)
  def f_gen_sle_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(BVSLE, arg0, arg1)
  def f_gen_slt_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(BVSLT, arg0, arg1)
  def f_gen_sub_bits(targ0: BigInt, arg0: Expr, arg1: Expr): Expr = BinaryExpr(BVADD, arg0, UnaryExpr(BVNOT, arg1))
  def f_gen_AArch64_MemTag_set(arg0: Expr, arg1: Expr, arg2: Expr): Expr = throw NotImplementedError()
  def f_gen_AArch64_MemTag_read(arg0: Expr, arg1: Expr): Expr = throw NotImplementedError()
  def f_gen_Mem_read(targ0: BigInt, arg0: Expr, arg1: Expr, arg2: Expr): Expr = {
    val size: Int = arg1 match
      case BitVecLiteral(v, s) => v.toInt * 8
      case IntLiteral(v) => v.toInt * 8
      case _ => throw NotImplementedError(s"Cannot convert $arg2 to int")
    LoadExpr(arg1, size)
  }
  def f_gen_slice(e: Expr, lo: BigInt, wd: BigInt): Expr = Extract((wd + lo).toInt, lo.toInt, e)
  def f_gen_replicate_bits(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: BitVecLiteral): Expr =
    Range.Exclusive(1, arg1.value.toInt, 1).map(v => arg0).foldLeft(arg0)((a, b) => (BinaryExpr(BVCONCAT, a, b)))
  def f_gen_append_bits(targ0: BigInt, targ1: BigInt, arg0: Expr, arg1: Expr): Expr =
    BinaryExpr(BVCONCAT, arg0, arg1)

  def f_gen_array_load(arg0: Expr, arg1: BigInt): Expr = arg0 match
    case Register("_R", t) => Register("R" + arg1, 64)
    case _ => {
      Logger.warn(s"Unknown array load $arg0")
      arg0
    }
  def f_gen_array_store(arg0: Expr, arg1: BigInt, arg2: Expr): Unit = arg0 match
    case Register(n, t) if n.contains("R") => b.push_stmt(LocalAssign(Register("R" + arg1, 64), arg2))
    case _ => Logger.warn(s"Unknown array store $arg0")

  def f_gen_Mem_set(sz: BigInt, ptr: Expr, width: BitVecLiteral, acctype: Expr, value: Expr): Unit =
    assert(width.value == sz)
    val stmt = MemoryStore(memory, ptr, value, endian, sz.toInt * 8)
    b.push_stmt(stmt)

  def f_gen_assert(arg0: Expr): Unit = {
    b.push_stmt(Assert(arg0))
  }

  def f_switch_context(arg0: L): Unit = b.switch_ctx(arg0)

  def f_true_branch(arg0: L): L = b.true_branch(arg0)
  def f_false_branch(arg0: L): L = b.false_branch(arg0)
  def f_merge_branch(arg0: L): L = b.merge_branch(arg0)

  /** Global variable definitions * */


  def v_PSTATE_UAO: Mutable[Expr] = throw NotImplementedError()
  def v_PSTATE_PAN: Mutable[Expr] = throw NotImplementedError()
  def v_PSTATE_DIT: Mutable[Expr] = throw NotImplementedError()
  def v_PSTATE_SSBS: Mutable[Expr] = throw NotImplementedError()
  def v_PSTATE_G: Mutable[Expr] = throw NotImplementedError()
  def v_PSTATE_A: Mutable[Expr] = throw NotImplementedError()
  def v_PSTATE_I: Mutable[Expr] = throw NotImplementedError()
  def v_PSTATE_F: Mutable[Expr] = throw NotImplementedError()
  def v_PSTATE_D: Mutable[Expr] = throw NotImplementedError()
  def v_PSTATE_C = Mutable(Register("CF", 1)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "C")
  def v_PSTATE_Z = Mutable(Register("ZF", 1)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "Z")
  def v_PSTATE_V = Mutable(Register("VF", 1)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "V")
  def v_PSTATE_N = Mutable(Register("NF", 1)) // Expr_Field(Expr_Var(Ident "PSTATE"), Ident "N")

  def v__PC = Mutable(Register("_PC", 64))
  def v__R = Mutable(Register("_R", 128))
  def v__Z = Mutable(Register("_Z", 1))
  def v_SP_EL0 = Mutable(Register("R31", 64))
  def v_FPSR = Mutable(Register("FPSR", 1))
  def v_FPCR = Mutable(Register("FPCR", (32)))

  def v_PSTATE_BTYPE = Mutable(Register("PSTATE.BTYPE", 1))
  def v_BTypeCompatible = Mutable(Register("BTypeCompatible", 1))
  def v___BranchTaken = Mutable(Register("__BranchTaken", 1))
  def v_BTypeNext = Mutable(Register("BTypeNext", 1))
  def v___ExclusiveLocal = Mutable(Register("__ExclusiveLocal", 1))

}

def extract(x: BigInt, sz: BigInt) = x % (BigInt(2).pow((sz + 1).toInt))


def zero_extend_to(s: BigInt, x: BitVecLiteral) = {
  require(s >= x.size, s"$s $x")
  if (s == x.size) x else BitVecLiteral(x.value, s.toInt)
}

def gen_zero_extend_to(s: BigInt, x: Expr) = {
  x.getType match {
    case BitVecType(sz) => ZeroExtend((s - sz).toInt, x)
    case _ => throw Exception("Type mismatch gen_zero_extend_to")
  }
}

class StmtListLifter extends LifterIFace[Int] {
  val builder = StmtListBuilder()
  def b : Builder[Int] = builder
  def extract : Seq[Statement] = builder.extract
}

object Lifter {

  def liftBlockBytes(ops: Iterable[Int], initialSp: BigInt) : Seq[Statement] = {
    val lift = StmtListLifter()
    var sp = initialSp
    ops.foreach { op =>
      if (op == 0xd503201f.toInt) {
        // nop
      } else {
        try {
          f_A64_decoder[Expr, Int, BitVecLiteral](lift, BitVecLiteral(BigInt(op), 32), BitVecLiteral(sp, 64))
        } catch {
          case e => {
            val o = "%x".format(op)
            println(s"Lift failure $o : $e")
            println(e.getStackTrace.mkString("\n  "))

          }
        }
      }
      sp = sp + 32
    }
    lift.extract
  }

  def liftOpcode(op: BigInt, sp: BigInt) : Seq[Statement] = {
    val lift = StmtListLifter()
    val dec = f_A64_decoder[Expr, Int, BitVecLiteral](lift, BitVecLiteral(op, 32), BitVecLiteral(sp, 64))
    println(dec)
    lift.extract
  }

}
