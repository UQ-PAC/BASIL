package ir

import analysis.util._
import scala.collection.mutable

class Interpret(IRProgram: Program) {
  val regs: mutable.Map[Variable, BitVecLiteral] = mutable.Map()
  val mems: mutable.Map[Memory, BitVecLiteral] = mutable.Map()
  var nextBlock: Block = IRProgram.procedures.head.blocks.head

  def eval(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): BitVecLiteral = {
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
        smt_extract(e.end - 1, e.start + 1, eval(e.body, env))

      case r: Repeat =>
        println(s"\t$r")
        val arg = eval(r.body, env)
        var result = arg
        for (_ <- 1 to r.repeats) {
          result = smt_concat(result, arg)
        }
        result

      case bin: BinaryExpr =>
        val left: BitVecLiteral = eval(bin.arg1, env)
        val right: BitVecLiteral = eval(bin.arg2, env)
        println(s"\tBinaryExpr($left ${bin.op} $right)")
        bin.op match {
          case BVAND  => smt_bvand(left, right)
          case BVOR   => smt_bvor(left, right)
          case BVADD  => smt_bvadd(left, right)
          case BVMUL  => smt_bvmul(left, right)
          case BVUDIV => smt_bvudiv(left, right)
          case BVUREM => smt_bvurem(left, right)
          case BVSHL  => smt_bvshl(left, right)
          case BVLSHR => smt_bvlshr(left, right)
          //          case BVULT    => smt_bvult(left, right)
          case BVNAND => ???
          case BVNOR  => ???
          case BVXOR  => ???
          case BVXNOR => ???
          case BVCOMP => smt_bvcomp(left, right)
          case BVSUB  => smt_bvsub(left, right)
          case BVSDIV => smt_bvsdiv(left, right)
          case BVSREM => smt_bvsrem(left, right)
          case BVSMOD => ???
          case BVASHR => smt_bvashr(left, right)
          //          case BVULE    => smt_bvule(left, right)
          case BVUGT => ???
          case BVUGE => ???
          //          case BVSLT    => smt_bvslt(left, right)
          //          case BVSLE    => smt_bvsle(left, right)
          case BVSGT => ???
          case BVSGE => ???
          //          case BVEQ     => smt_bveq(left, right)
          //          case BVNEQ    => smt_bvneq(left, right)
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
        BitVecLiteral(0, 0)

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
        case gt: GoTo         => println(s"$gt")
        case dc: DirectCall   => println(s"$dc")
        case ic: IndirectCall => println(s"$ic")
      }
    }
  }

  def interpretStmt(s: Statement): Unit = {
    s match {
      case assign: LocalAssign =>
        println(s"LocalAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        regs += (assign.lhs -> evalRight)
        println(s"LocalAssign ${assign.lhs} -> ${evalRight}\n")

      case assign: MemoryAssign =>
        println(s"MemoryAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        mems += (assign.lhs -> evalRight)
        println(s"MemoryAssign ${assign.lhs} -> ${evalRight}\n")
    }
  }

  // Program.Procedure
  interpretProcedure(IRProgram.procedures.head)

  println("\nREGS:")
  for (reg <- regs) {
    println(s"${reg._1} -> ${reg._2}")
  }

  println("\nMEMS:")
  for (mem <- mems) {
    println(s"${mem._1} -> ${mem._2}")
  }

  println("Interpret End")
}
