package ir

import analysis.Pointer
import scala.collection.mutable

class Interpret(IRProgram: Program) {
  val regs: mutable.Map[Variable, String] = mutable.Map()

  def eval(exp: Expr, env: mutable.Map[Variable, String]): String = {
    exp match {
      case Memory(name, addressSize, valueSize) => s"Memory($name, $addressSize, $valueSize)"
      case repeat: Repeat                       => s"Repeat($repeat)"
      case extract: Extract                     => s"Extract($extract)"
      case pointer: Pointer                     => s"Pointer($pointer)"
      case expr: UnaryExpr                      => s"UnaryExpr($expr)"
      case expr: BinaryExpr       => s"BinaryExpr(${eval(expr.arg1, env)} ${expr.op} ${eval(expr.arg2, env)})"
      case load: MemoryLoad       => s"MemoryLoad($load)"
      case extend: SignExtend     => s"SignExtend($extend)"
      case extend: ZeroExtend     => s"$extend"
      case store: MemoryStore     => s"$store"
      case Variable(name, irType) => s"Variable($name: $irType)"
      case literal: Literal =>
        literal match {
          case BitVecLiteral(value, size) => s"BitVecLiteral($value, $size)"
          case IntLiteral(value)          => s"IntLiteral($value)"
          case lit: BoolLit =>
            lit match {
              case TrueLiteral  => s"TrueLiteral($TrueLiteral)"
              case FalseLiteral => s"FalseLiteral($FalseLiteral)"
            }
        }
    }
  }

  def interpretProcedure(p: Procedure): Unit = {
    println(s"${p.name} ${p.address}")

    // Procedure.in
    for ((in, index) <- p.in.zipWithIndex) {
      print(s"\tin[$index]:${in.name} ${in.size} ${eval(in.value, regs)}\n")
    }

    // Procedure.out
    for ((out, index) <- p.out.zipWithIndex) {
      print(s"\tout[$index]:${out.name} ${out.size} ${eval(out.value, regs)}\n")
    }

    // Procedure.Block
    for ((block, index) <- p.blocks.zipWithIndex) {
      println(s"block[$index]:${block.label} ${block.address}")
      interpretBlock(block)
    }
  }

  def interpretBlock(b: Block): Unit = {
    // Block.Statement
    for ((statement, index) <- b.statements.zipWithIndex) {
      print(s"statement[$index]:")
      interpretStmt(statement)
    }

    // Block.Jump
    for ((jump, index) <- b.jumps.zipWithIndex) {
      print(s"jump[$index]:")
      jump match {
        case to: GoTo           => println(s"$to")
        case call: DirectCall   => println(s"$call")
        case call: IndirectCall => println(s"$call")
      }
    }
  }

  def interpretStmt(s: Statement): Unit = {
    s match {
      case assign: LocalAssign =>
        regs += (assign.lhs -> eval(assign.rhs, regs))
        print(s"LocalAssign ${eval(assign.lhs, regs)} = ${eval(assign.rhs, regs)}\n")
      case assign: MemoryAssign =>
        print(s"MemoryAssign ${eval(assign.lhs, regs)} = ${eval(assign.rhs, regs)}\n")
    }
  }

  // Program.Procedure
  for ((procedure, index) <- IRProgram.procedures.zipWithIndex) {
    print(s"procedure[$index]:")
    interpretProcedure(procedure)
  }

  println("\nREGS:")
  for (reg <- regs) {
    println(s"${reg._1} -> ${reg._2}")
  }
}
