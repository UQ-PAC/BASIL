package ir

import analysis.BitVectorEval
import util.Logger

import scala.collection.mutable

class Interpret(IRProgram: Program) {
  val regs: mutable.Map[Variable, BitVecLiteral] = mutable.Map()
  val mems: Array[BitVecLiteral] =
    Array.fill(IRProgram.initialMemory.map(section => section.address + section.size).max)(BitVecLiteral(0, 8))
  val mainProcedure: Procedure = IRProgram.procedures.find(_.name == "main").get
  val exitVariable: Variable = mainProcedure.out.find(_.name == "LR_out").get.value
  var nextBlock: Block = _

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
        BitVectorEval.smt_zero_extend(ze.extension, eval(ze.body, env))

      case se: SignExtend =>
        Logger.debug(s"\t$se")
        BitVectorEval.smt_sign_extend(se.extension, eval(se.body, env))

      case e: Extract =>
        Logger.debug(s"\tExtract($e, ${e.start}, ${e.end})")
        // TODO: wait for BitVectorEval.smt_extract return correct result
//        BitVectorEval.smt_extract(e.end - 1, e.start + 1, eval(e.body, env))
        // return dummy result
        val body: Literal = eval(e.body, env)
        body match {
          case BitVecLiteral(value, size) => BitVecLiteral(value, e.end - e.start)
          case _                          => throw new Exception("error")
        }

      case r: Repeat =>
        Logger.debug(s"\t$r")
        val arg = eval(r.body, env)
        var result = arg
        for (_ <- 1 to r.repeats) {
          result = BitVectorEval.smt_concat(result, arg)
        }
        result

      case bin: BinaryExpr =>
        val left: Literal = eval(bin.arg1, env)
        val right: Literal = eval(bin.arg2, env)
        Logger.debug(s"\tBinaryExpr($left ${bin.op} $right)")
        bin.op match {
          case BVAND    => BitVectorEval.smt_bvand(left, right)
          case BVOR     => BitVectorEval.smt_bvor(left, right)
          case BVADD    => BitVectorEval.smt_bvadd(left, right)
          case BVMUL    => BitVectorEval.smt_bvmul(left, right)
          case BVUDIV   => BitVectorEval.smt_bvudiv(left, right)
          case BVUREM   => BitVectorEval.smt_bvurem(left, right)
          case BVSHL    => BitVectorEval.smt_bvshl(left, right)
          case BVLSHR   => BitVectorEval.smt_bvlshr(left, right)
          case BVULT    => BitVectorEval.smt_bvult(left, right)
          case BVNAND   => ???
          case BVNOR    => ???
          case BVXOR    => ???
          case BVXNOR   => ???
          case BVCOMP   => BitVectorEval.smt_bvcomp(left, right)
          case BVSUB    => BitVectorEval.smt_bvsub(left, right)
          case BVSDIV   => BitVectorEval.smt_bvsdiv(left, right)
          case BVSREM   => BitVectorEval.smt_bvsrem(left, right)
          case BVSMOD   => ???
          case BVASHR   => BitVectorEval.smt_bvashr(left, right)
          case BVULE    => BitVectorEval.smt_bvule(left, right)
          case BVUGT    => ???
          case BVUGE    => ???
          case BVSLT    => BitVectorEval.smt_bvslt(left, right)
          case BVSLE    => BitVectorEval.smt_bvsle(left, right)
          case BVSGT    => ???
          case BVSGE    => ???
          case BVEQ     => BitVectorEval.smt_bveq(left, right)
          case BVNEQ    => BitVectorEval.smt_bvneq(left, right)
          case BVCONCAT => BitVectorEval.smt_concat(left, right)
        }

      case un: UnaryExpr =>
        val arg = eval(un.arg, env)
        Logger.debug(s"\tUnaryExpr($un)")
        un.op match {
          case BVNEG   => BitVectorEval.smt_bvneg(arg)
          case BVNOT   => BitVectorEval.smt_bvnot(arg)
          case IntNEG  => ???
          case BoolNOT => ???
        }

      case m: Memory =>
        Logger.debug(s"\t$m")
        ???

      case ml: MemoryLoad =>
        Logger.debug(s"\t$ml")
        // TODO: load bv from mems
        BitVecLiteral(0, ml.size)

      case ms: MemoryStore =>
        Logger.debug(s"\t$ms")
        eval(ms.value, env)
    }
  }

  def interpretProcedure(p: Procedure): Unit = {
    Logger.debug(s"Procedure(${p.name}, ${p.address.getOrElse("None")})")

    // Procedure.in
    for ((in, index) <- p.in.zipWithIndex) {
      Logger.debug(s"\tin[$index]:${in.name} ${in.size} ${in.value}")
    }

    // Procedure.out
    for ((out, index) <- p.out.zipWithIndex) {
      Logger.debug(s"\tout[$index]:${out.name} ${out.size} ${out.value}")
    }

    // Procedure.Block
    nextBlock = p.blocks.head
    Logger.debug(s"Block:${nextBlock.label} ${nextBlock.address}")
    interpretBlock(nextBlock)
  }

  def interpretBlock(b: Block): Unit = {
    // Block.Statement
    for ((statement, index) <- b.statements.zipWithIndex) {
      Logger.debug(s"statement[$index]:")
      interpretStmt(statement)
    }

    // Block.Jump
    for ((jump, index) <- b.jumps.zipWithIndex) {
      Logger.debug(s"jump[$index]:")
      jump match {
        case gt: GoTo       => Logger.debug(s"$gt")
        case dc: DirectCall => Logger.debug(s"$dc")
        case ic: IndirectCall =>
          Logger.debug(s"$ic")
          if (ic.target == exitVariable) {
            Logger.debug("EXIT main")
          }
      }
    }
  }

  def interpretStmt(s: Statement): Unit = {
    s match {
      case assign: LocalAssign =>
        Logger.debug(s"LocalAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        Logger.debug(s"LocalAssign ${assign.lhs} -> $evalRight\n")

        evalRight match {
          case BitVecLiteral(_, 64) => regs += (assign.lhs -> evalRight.asInstanceOf[BitVecLiteral])
          case _                    => throw new Exception("cannot register non-bitvectors")
        }

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
  var currentAddr = 0
  IRProgram.initialMemory
    .sortBy(_.address)
    .foreach { im =>
      if (im.address + im.size > currentAddr) {
        val start = im.address.max(currentAddr)
        val data = if (im.address < currentAddr) im.bytes.slice(currentAddr - im.address, im.size) else im.bytes
        data.zipWithIndex.foreach { case (byte, index) =>
          mems(start + index) = byte.asInstanceOf[BitVecLiteral]
        }
        currentAddr = im.address + im.size
      }
    }

  // Program.Procedure
  interpretProcedure(mainProcedure)

  Logger.debug("\nREGS:")
  for (reg <- regs) {
    Logger.debug(s"${reg._1} -> ${reg._2}")
  }

  Logger.debug("Interpret End")
}
