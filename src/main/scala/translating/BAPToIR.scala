package translating

import bap._
import ir._
import specification._
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.par
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

class BAPToIR(var program: BAPProgram, mainAddress: Int) {

  private val nameToProcedure: mutable.Map[String, Procedure] = mutable.Map()
  private val labelToBlock: mutable.Map[String, Block] = mutable.Map()

  def translate: Program = {
    var mainProcedure: Option[Procedure] = None
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
      val procedure = Procedure(s.name, Some(s.address), blocks, in, out, BAPLoader.isNonReturning(s.name))
      if (s.address == mainAddress) {
        mainProcedure = Some(procedure)
      }
      procedures.append(procedure)
      nameToProcedure.addOne(s.name, procedure)
    }

    for (s <- program.subroutines.par) {

      var isReturning = false
      for (b <- s.blocks) {
        val block = labelToBlock(b.label)
        for (st <- b.statements) {
          block.statements.append(translate(st))
        }
        for (j <- b.jumps) {
          val translated = translate(j)
          if (translated.isInstanceOf[DirectCall] && translated.asInstanceOf[DirectCall].target.nonReturning) {
            translated.asInstanceOf[DirectCall].returnTarget = None
          }
          if (j.isInstanceOf[BAPIndirectCall] && j.asInstanceOf[BAPIndirectCall].target.name == "R30") {
            isReturning = true
          }

          block.jumps.append(translated)

        }
      }
      if (!isReturning) {
        nameToProcedure(s.name).nonReturning = true
      }
    }

    val memorySections: ArrayBuffer[MemorySection] = ArrayBuffer()
    for (m <- program.memorySections) {
      val bytes = m.bytes.map(_.toIR)
      memorySections.append(MemorySection(m.name, m.address, m.size, bytes))
    }

    Program(procedures, memorySections, mainProcedure.get)
  }

  private def translate(s: BAPStatement) = s match {
    case b: BAPMemAssign   => MemoryAssign(b.lhs.toIR, b.rhs.toIR)
    case b: BAPLocalAssign => LocalAssign(b.lhs.toIR, b.rhs.toIR)
    case _                 => throw new Exception("unsupported statement: " + s)
  }


  private def translate(j: BAPJump) = j match {
    case b: BAPDirectCall =>
      DirectCall(
        nameToProcedure(b.target),
        coerceToBool(b.condition),
        if (nameToProcedure(b.target).nonReturning)
          Option.empty
        else
          b.returnTarget.map { (t: String) => labelToBlock(t) }
      )
    case b: BAPIndirectCall =>
      IndirectCall(b.target.toIR, coerceToBool(b.condition), b.returnTarget.map { (t: String) => labelToBlock(t) })
    case b: BAPGoTo =>
      GoTo(labelToBlock(b.target), coerceToBool(b.condition))
    case _ =>
      throw new Exception("unsupported jump: " + j)

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
