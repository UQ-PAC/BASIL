package translating

import bap._
import ir._
import specification._
import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

class BAPToIR(var program: BAPProgram) {

  private val nameToProcedure: mutable.Map[String, Procedure] = mutable.Map()
  private val labelToBlock: mutable.Map[String, Block] = mutable.Map()

  def translate: Program = {
    val procedures: ArrayBuffer[Procedure] = ArrayBuffer()
    for (s <- program.subroutines) {
      val blocks: ArrayBuffer[Block] = ArrayBuffer()
      for (b <- s.blocks) {
        val block = Block(b.label, b.address, ArrayBuffer(), ArrayBuffer())
        blocks.append(block)
        labelToBlock.addOne(b.label, block)
      }
      val in: ArrayBuffer[Parameter] = ArrayBuffer()
      for (p <- s.in) {
        in.append(p.toIR)
      }
      val out: ArrayBuffer[Parameter] = ArrayBuffer()
      for (p <- s.out) {
        out.append(p.toIR)
      }
      val procedure = Procedure(s.name, Some(s.address), blocks, in, out)
      procedures.append(procedure)
      nameToProcedure.addOne(s.name, procedure)
    }

    for (s <- program.subroutines) {
      for (b <- s.blocks) {
        val block = labelToBlock(b.label)
        for (st <- b.statements) {
          block.statements.append(translate(st))
        }
        for (j <- b.jumps) {
          block.jumps.append(translate(j))
        }
      }
    }

    val memorySections: ArrayBuffer[MemorySection] = ArrayBuffer()
    for (m <- program.memorySections) {
      val bytes = m.bytes.map(_.toIR)
      memorySections.append(MemorySection(m.name, m.address, m.size, bytes))
    }

    Program(procedures, memorySections)
  }

  private def translate(s: BAPStatement) = {
    s match {
      case b: BAPMemAssign => MemoryAssign(b.lhs.toIR, b.rhs.toIR)
      case b: BAPLocalAssign => LocalAssign(b.lhs.toIR, b.rhs.toIR)
      case _ => throw new Exception("unsupported statement: " + s)
    }
  }

  private def translate(j: BAPJump) = {
    j match {
      case b: BAPDirectCall =>
        DirectCall(nameToProcedure(b.target), coerceToBool(b.condition), b.returnTarget.map {(t: String) => labelToBlock(t)})
      case b: BAPIndirectCall =>
        IndirectCall(b.target.toIR, coerceToBool(b.condition), b.returnTarget.map {(t: String) => labelToBlock(t)})
      case b: BAPGoTo =>
        GoTo(labelToBlock(b.target), coerceToBool(b.condition))
      case _ =>
        throw new Exception("unsupported jump: " + j)
    }
  }

  private def coerceToBool(condition: BAPExpr): Option[Expr] = condition match {
    case l: BAPLiteral if l.value > BigInt(0) =>
      None
    case _ =>
      val c = condition.toIR
      c.getType match {
        case BoolType => Some(c)
        case bv: BitVecType => Some(BinaryExpr(BVNEQ, c, BitVecLiteral(0, bv.size)))
        case _ => ???
      }
  }
}
