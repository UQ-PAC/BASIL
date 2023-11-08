package translating

import bap._
import ir._
import specification._
import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import intrusiveList.IntrusiveList

class BAPToIR(var program: BAPProgram, mainAddress: Int) {

  private val nameToProcedure: mutable.Map[String, Procedure] = mutable.Map()
  private val labelToBlock: mutable.Map[String, Block] = mutable.Map()

  def translate: Program = {
    var mainProcedure: Option[Procedure] = None
    val procedures: ArrayBuffer[Procedure] = ArrayBuffer()
    for (s <- program.subroutines) {
      val blocks: IntrusiveList[Block] = IntrusiveList[Block]()
      val in: ArrayBuffer[Parameter] = ArrayBuffer()
      val out: ArrayBuffer[Parameter] = ArrayBuffer()
      val procedure = Procedure(s.name, Some(s.address), blocks, in, out)

      for (b <- s.blocks) {
        val block = Block(b.label, b.address, IntrusiveList(), ArrayBuffer(), procedure)
        blocks.append(block)
        labelToBlock.addOne(b.label, block)
      }
      for (p <- s.in) {
        in.append(p.toIR)
      }
      for (p <- s.out) {
        out.append(p.toIR)
      }
      if (s.address == mainAddress) {
        mainProcedure = Some(procedure)
      }
      procedures.append(procedure)
      nameToProcedure.addOne(s.name, procedure)
    }

    for (s <- program.subroutines) {
      for (b <- s.blocks) {
        val block = labelToBlock(b.label)
        for (st <- b.statements) {
          block.statements.append(translate(st, block))
        }
        for (j <- b.jumps) {
          block.jumps.append(translate(j, block))
        }
      }
    }

    val memorySections: ArrayBuffer[MemorySection] = ArrayBuffer()
    for (m <- program.memorySections) {
      val bytes = m.bytes.map(_.toIR)
      memorySections.append(MemorySection(m.name, m.address, m.size, bytes))
    }

    Program(procedures, mainProcedure.get, memorySections, ArrayBuffer())
  }

  private def translate(s: BAPStatement, parent: Block) = s match {
    case b: BAPMemAssign   => MemoryAssign(b.lhs.toIR, b.rhs.toIR, parent, Some(b.line))
    case b: BAPLocalAssign => LocalAssign(b.lhs.toIR, b.rhs.toIR, parent, Some(b.line))
  }

  private def translate(j: BAPJump, parent: Block) = j match {
    case b: BAPDirectCall =>
      DirectCall(
        nameToProcedure(b.target),
        b.returnTarget.map(t => labelToBlock(t)),
        parent,
        Some(b.line),
      )
    case b: BAPIndirectCall =>
      IndirectCall(b.target.toIR, parent, b.returnTarget.map(t => labelToBlock(t)), Some(b.line))
    case b: BAPGoTo =>
      GoTo(labelToBlock(b.target), parent, coerceToBool(b.condition), Some(b.line))
  }

  /*
  private def translate(e: BAPExpr) = e match {
    case b: BAPConcat => BinaryExpr(BVCONCAT, left.toIR, right.toIR)
    case b: BAPSignedExtend =>
      if (width > body.size) {
        SignExtend(width - body.size, body.toIR)
      } else {
        BAPExtract(width - 1, 0, body).toIR
      }
    case b: BAPUnsignedExtend =>
      if (width > body.size) {
        ZeroExtend(width - body.size, body.toIR)
      } else {
        BAPExtract(width - 1, 0, body).toIR
      }
    case b: BAPExtract =>
      val bodySize = body.size
      if (size > bodySize) {
        if (low == 0) {
          ZeroExtend(size - bodySize, body.toIR)
        } else {
          Extract(high + 1, low, ZeroExtend(size - bodySize, body.toIR))
        }
      } else {
        Extract(high + 1, low, body.toIR)
      }
    case b: BAPLiteral =>

  }
   */

  private def coerceToBool(condition: BAPExpr): Option[Expr] = condition match {
    case l: BAPLiteral if l.value > BigInt(0) =>
      None
    case _ =>
      val c = condition.toIR
      c.getType match {
        case BoolType       => Some(c)
        case bv: BitVecType => Some(BinaryExpr(BVNEQ, c, BitVecLiteral(0, bv.size)))
        case _              => ???
      }
  }
}
