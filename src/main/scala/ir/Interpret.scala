package ir

import analysis.util.*

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
            println(s"\t${id.name} == $value")
            value
          case _ => throw new Exception(s"$id not found in env")
        }

      case n: Literal =>
        n match {
          case bv: BitVecLiteral =>
            println(s"\tBitVecLiteral(${bv.value}, ${bv.size})")
            bv
          case _ => ???
        }

      case ze: ZeroExtend =>
        println(s"\t$ze")
        smt_zero_extend(ze.extension, eval(ze.body, env))

      case se: SignExtend =>
        println(s"\t$se")
        smt_sign_extend(se.extension, eval(se.body, env))

      case e: Extract =>
        println(s"\tExtract($e, ${e.start}, ${e.end})")
        // TODO: wait for smt_extract return correct result
//        smt_extract(e.end - 1, e.start + 1, eval(e.body, env))
        // return dummy result
        val body: Literal = eval(e.body, env)
        body match {
          case BitVecLiteral(value, size) => BitVecLiteral(value, e.end - e.start)
          case _                          => throw new Exception("error")
        }

      case r: Repeat =>
        println(s"\t$r")
        val arg = eval(r.body, env)
        var result = arg
        for (_ <- 1 to r.repeats) {
          result = smt_concat(result, arg)
        }
        result

      case bin: BinaryExpr =>
        val left: Literal = eval(bin.arg1, env)
        val right: Literal = eval(bin.arg2, env)
        println(s"\tBinaryExpr($left ${bin.op} $right)")
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
        println(s"\tUnaryExpr($un)")
        un.op match {
          case BVNEG   => smt_bvneg(arg)
          case BVNOT   => smt_bvnot(arg)
          case IntNEG  => ???
          case BoolNOT => ???
        }

      case m: Memory =>
        println(s"\t$m")
        ???

      case ml: MemoryLoad =>
        println(s"\t$ml")
        // TODO: load bv from mems
        BitVecLiteral(0, ml.size)

      case ms: MemoryStore =>
        println(s"\t$ms")
        eval(ms.value, env)
    }
  }

  def interpretProcedure(p: Procedure): Unit = {
    println(s"Procedure(${p.name}, ${p.address})")

    // Procedure.in
    for ((in, index) <- p.in.zipWithIndex) {
      println(s"\tin[$index]:${in.name} ${in.size} ${in.value}")
    }

    // Procedure.out
    for ((out, index) <- p.out.zipWithIndex) {
      println(s"\tout[$index]:${out.name} ${out.size} ${out.value}")
    }

    // Procedure.Block
    nextBlock = p.blocks.head
    println(s"Block:${nextBlock.label} ${nextBlock.address}")
    interpretBlock(nextBlock)
  }

  def interpretBlock(b: Block): Unit = {
    // Block.Statement
    for ((statement, index) <- b.statements.zipWithIndex) {
      println(s"statement[$index]:")
      interpretStmt(statement)
    }

    // Block.Jump
    for ((jump, index) <- b.jumps.zipWithIndex) {
      println(s"jump[$index]:")
      jump match {
        case gt: GoTo       => println(s"$gt")
        case dc: DirectCall => println(s"$dc")
        case ic: IndirectCall =>
          println(s"$ic")
          if (ic.target == exitVariable) {
            println("EXIT main")
          }
      }
    }
  }

  def interpretStmt(s: Statement): Unit = {
    s match {
      case assign: LocalAssign =>
        println(s"LocalAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        println(s"LocalAssign ${assign.lhs} -> $evalRight\n")

        evalRight match {
          case BitVecLiteral(_, 64) => regs += (assign.lhs -> evalRight.asInstanceOf[BitVecLiteral])
          case _                    => throw new Exception("cannot register non-bitvectors")
        }

      case assign: MemoryAssign =>
        println(s"MemoryAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        evalRight match {
          // TODO: assign the memory array
          case BitVecLiteral(_, _) => println(s"MemoryAssign ${assign.lhs} -> $evalRight\n")
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

  println("\nREGS:")
  for (reg <- regs) {
    println(s"${reg._1} -> ${reg._2}")
  }

  println("Interpret End")
}
