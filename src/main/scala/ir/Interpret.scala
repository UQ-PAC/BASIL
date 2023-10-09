package ir

import analysis.BitVectorEval.*
import util.Logger

import scala.collection.mutable

class Interpret(IRProgram: Program) {
  val regs: mutable.Map[Variable, BitVecLiteral] = mutable.Map()
  val mems: mutable.Map[Int, BitVecLiteral] = mutable.Map()
  private val SP: BitVecLiteral = BitVecLiteral(BigInt("1000000"), 64)
  private val FP: BitVecLiteral = BitVecLiteral(BigInt("1000000"), 64)
  private val LR: BitVecLiteral = BitVecLiteral(BigInt("1234"), 64)
  private val mainProcedure: Procedure = IRProgram.procedures.find(_.name == "main").get
  private var nextBlock: Option[Block] = None

  def eval(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): Literal = {
    exp match {
      case id: Variable =>
        env.get(id) match {
          case Some(value) =>
            Logger.debug(s"\t${id.name} == $value")
            value
          case _ => throw new Exception(s"$id not found in env")
        }

      case n: Literal =>
        n match {
          case bv: BitVecLiteral =>
            Logger.debug(s"\tBitVecLiteral(${bv.value}, ${bv.size})")
            bv
          case _ => ???
        }

      case ze: ZeroExtend =>
        Logger.debug(s"\t$ze")
        smt_zero_extend(ze.extension, eval(ze.body, env))

      case se: SignExtend =>
        Logger.debug(s"\t$se")
        smt_sign_extend(se.extension, eval(se.body, env))

      case e: Extract =>
        Logger.debug(s"\tExtract($e, ${e.start}, ${e.end})")
        boogie_extract(e.end, e.start, eval(e.body, env))

      case r: Repeat =>
        Logger.debug(s"\t$r")
        val arg = eval(r.body, env)
        var result = arg
        for (_ <- 1 to r.repeats) {
          result = smt_concat(result, arg)
        }
        result

      case bin: BinaryExpr =>
        val left: Literal = eval(bin.arg1, env)
        val right: Literal = eval(bin.arg2, env)
        Logger.debug(s"\tBinaryExpr($left ${bin.op} $right)")
        bin.op match {
          case BVAND    => smt_bvand(left, right)
          case BVOR     => smt_bvor(left, right)
          case BVADD    => smt_bvadd(left, right)
          case BVMUL    => smt_bvmul(left, right)
          case BVUDIV   => smt_bvudiv(left, right)
          case BVUREM   => smt_bvurem(left, right)
          case BVSHL    => smt_bvshl(left, right)
          case BVLSHR   => smt_bvlshr(left, right)
          case BVULT    => smt_bvult(left, right)
          case BVNAND   => ???
          case BVNOR    => ???
          case BVXOR    => ???
          case BVXNOR   => ???
          case BVCOMP   => smt_bvcomp(left, right)
          case BVSUB    => smt_bvsub(left, right)
          case BVSDIV   => smt_bvsdiv(left, right)
          case BVSREM   => smt_bvsrem(left, right)
          case BVSMOD   => ???
          case BVASHR   => smt_bvashr(left, right)
          case BVULE    => smt_bvule(left, right)
          case BVUGT    => ???
          case BVUGE    => ???
          case BVSLT    => smt_bvslt(left, right)
          case BVSLE    => smt_bvsle(left, right)
          case BVSGT    => ???
          case BVSGE    => ???
          case BVEQ     => smt_bveq(left, right)
          case BVNEQ    => smt_bvneq(left, right)
          case BVCONCAT => smt_concat(left, right)
        }

      case un: UnaryExpr =>
        val arg = eval(un.arg, env)
        Logger.debug(s"\tUnaryExpr($un)")
        un.op match {
          case BVNEG   => smt_bvneg(arg)
          case BVNOT   => smt_bvnot(arg)
          case IntNEG  => ???
          case BoolNOT => ???
        }

      case m: Memory =>
        Logger.debug(s"\t$m")
        ???

      case ml: MemoryLoad =>
        Logger.debug(s"\t$ml")
        val startIndex: Int = eval(ml.index, env).asInstanceOf[BitVecLiteral].value.toInt
        val endIndex: Int = startIndex + ml.size / 8 - 1

        val selectedMemory = (startIndex to endIndex).map(i => mems.getOrElse(i, BitVecLiteral(0, 8)))

        selectedMemory.reduceLeft { (result: BitVecLiteral, current: BitVecLiteral) =>
          val (newValue, newSize) = ml.endian match {
            case Endian.BigEndian => (result.value << current.size | current.value, result.size + current.size)
            case Endian.LittleEndian => (result.value | (current.value << result.size), result.size + current.size)
          }
          BitVecLiteral(newValue, newSize)
        }
        ???

      case ms: MemoryStore =>
        //TODO MemoryStore return value?
        Logger.debug(s"\tMemoryStore mem:${ms.mem} i:${ms.index} val:${ms.value} size:${ms.size}")
        val index: Int = eval(ms.index, env).asInstanceOf[BitVecLiteral].value.toInt
        val value: BitVecLiteral = eval(ms.value, env).asInstanceOf[BitVecLiteral]

        val binaryString: String = value.value.toString(2).reverse.padTo(ms.size, '0').reverse
        val data: List[BitVecLiteral] = binaryString.grouped(8).toList.map(chunk => BitVecLiteral(BigInt(chunk, 2), 8))
        data.zipWithIndex.foreach { case (bv, i) =>
          mems(index + i) = bv
        }
        value
    }
  }

  def interpretProcedure(p: Procedure): Unit = {
    Logger.debug(s"Procedure(${p.name}, ${p.address})")

    // Procedure.in
    for ((in, index) <- p.in.zipWithIndex) {
      Logger.debug(s"\tin[$index]:${in.name} ${in.size} ${in.value}")
    }

    // Procedure.out
    for ((out, index) <- p.out.zipWithIndex) {
      Logger.debug(s"\tout[$index]:${out.name} ${out.size} ${out.value}")
    }

    // Procedure.Block
    nextBlock = Some(p.blocks.head)
    Logger.debug(s"Block:${nextBlock.get.label} ${nextBlock.get.address}")
  }

  def interpretBlock(b: Block): Unit = {
    // Block.Statement
    for ((statement, index) <- b.statements.zipWithIndex) {
      Logger.debug(s"statement[$index]:")
      interpretStatement(statement)
    }

    // Block.Jump
    // TODO: Check condition
    for ((jump, index) <- b.jumps.zipWithIndex) {
      Logger.debug(s"jump[$index]:")
      jump match {
        case gt: GoTo       => Logger.debug(s"$gt")
        case dc: DirectCall => Logger.debug(s"$dc")
        case ic: IndirectCall =>
          Logger.debug(s"$ic")
          if (ic.target == Register("R30", BitVecType(64))) {
            nextBlock = None
            Logger.debug("EXIT main")
          }
      }
    }
  }

  def interpretStatement(s: Statement): Unit = {
    s match {
      case assign: LocalAssign =>
        Logger.debug(s"LocalAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        Logger.debug(s"LocalAssign ${assign.lhs} -> $evalRight\n")
        regs += (assign.lhs -> evalRight.asInstanceOf[BitVecLiteral])

      case assign: MemoryAssign =>
        Logger.debug(s"MemoryAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        evalRight match {
          // TODO: assign the memory array
          case BitVecLiteral(_, _) => Logger.debug(s"MemoryAssign ${assign.lhs} -> $evalRight\n")
          case _                   => throw new Exception("cannot register non-bitvectors")
        }
    }
  }

  // initialize memory array from IRProgram
  private var currentAddress = 0
  IRProgram.initialMemory
    .sortBy(_.address)
    .foreach { im =>
      if (im.address + im.size > currentAddress) {
        val start = im.address.max(currentAddress)
        val data = if (im.address < currentAddress) im.bytes.slice(currentAddress - im.address, im.size) else im.bytes
        data.zipWithIndex.foreach { case (byte, index) =>
          mems(start + index) = byte.asInstanceOf[BitVecLiteral]
        }
        currentAddress = im.address + im.size
      }
    }

  // Initial SP, FP and LR
  regs += (Register("R31", BitVecType(64)) -> SP)
  regs += (Register("R29", BitVecType(64)) -> FP)
  regs += (Register("R30", BitVecType(64)) -> LR)

  // Program.Procedure
  interpretProcedure(mainProcedure)
  while (nextBlock.isDefined) {
    interpretBlock(nextBlock.get)
  }

  Logger.debug("\nREGS:")
  for ((key, value) <- regs) {
    Logger.debug(s"${key} -> ${value}")
  }

  Logger.debug("Interpret End")
}
