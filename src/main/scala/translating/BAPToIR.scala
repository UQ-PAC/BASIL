package translating

import bap.*
import boogie.UnaryBExpr
import ir.{UnaryExpr, *}
import specification.*

import scala.collection.mutable
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
      val dummyJump = GoTo(ArrayBuffer(), None)
      for (b <- s.blocks) {
        val block = Block(b.label, b.address, ArrayBuffer(), dummyJump)
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
      if (s.address == mainAddress) {
        mainProcedure = Some(procedure)
      }
      procedures.append(procedure)
      nameToProcedure.addOne(s.name, procedure)
    }

    for (s <- program.subroutines) {
      val procedure = nameToProcedure(s.name)
      for (b <- s.blocks) {
        val block = labelToBlock(b.label)
        for (st <- b.statements) {
          block.statements.append(translate(st))
        }
        val (jump, newBlocks) = translate(b.jumps, block)
        block.jump = jump
        procedure.blocks.addAll(newBlocks)
      }
    }

    val memorySections: ArrayBuffer[MemorySection] = ArrayBuffer()
    for (m <- program.memorySections) {
      val bytes = m.bytes.map(_.toIR)
      memorySections.append(MemorySection(m.name, m.address, m.size, bytes))
    }

    Program(procedures, mainProcedure.get, memorySections, ArrayBuffer())
  }

  private def translate(s: BAPStatement) = s match {
    case b: BAPMemAssign   => MemoryAssign(b.lhs.toIR, b.rhs.toIR, Some(b.line))
    case b: BAPLocalAssign => LocalAssign(b.lhs.toIR, b.rhs.toIR, Some(b.line))
  }

  private def translate(jumps: List[BAPJump], block: Block): (Jump, ArrayBuffer[Block]) = {
    if (jumps.size > 1) {
      val targets = ArrayBuffer[Block]()
      val conditions = ArrayBuffer[BAPExpr]()
      val line = jumps.head.line
      val newBlocks = ArrayBuffer[Block]()
      for (j <- jumps) {
        j match {
          case b: BAPGoTo =>
            val target = labelToBlock(b.target)
            b.condition match {
              case l: BAPLiteral if l.value > BigInt(0) =>
                if (conditions.isEmpty) {
                  targets.append(target)
                } else {
                  // && together all negated previous conditions
                  val conditionsIR = conditions.map { (c: BAPExpr) => BinaryExpr(BVEQ, c.toIR, BitVecLiteral(0, c.size)) }
                  val condition = conditionsIR.tail.foldLeft(conditionsIR.head)((ands: Expr, next: Expr) => BinaryExpr(BoolAND, next, ands))
                  val newLabel = s"${block.label}_goto_${target.label}"
                  val assume = Assume(condition, checkSecurity = true)
                  val newBlock = Block(newLabel, None, ArrayBuffer(assume), GoTo(ArrayBuffer(target), None))
                  newBlocks.append(newBlock)
                  targets.append(newBlock)
                }
              case _ =>
                if (conditions.isEmpty) {
                  val condition = BinaryExpr(BVNEQ, b.condition.toIR, BitVecLiteral(0, b.condition.size))
                  val newLabel = s"${block.label}_goto_${target.label}"
                  val assume = Assume(condition, checkSecurity = true)
                  val newBlock = Block(newLabel, None, ArrayBuffer(assume), GoTo(ArrayBuffer(target), None))
                  newBlocks.append(newBlock)
                  targets.append(newBlock)
                  conditions.append(b.condition)
                } else {
                  // && together this condition with all negated previous conditions
                  val conditionsIR = conditions.map { (c: BAPExpr) => BinaryExpr(BVEQ, c.toIR, BitVecLiteral(0, c.size)) }
                  val negatedConditions = conditionsIR.tail.foldLeft(conditionsIR.head)((ands: Expr, next: Expr) => BinaryExpr(BoolAND, next, ands))
                  val condition = BinaryExpr(BoolAND, BinaryExpr(BVNEQ, b.condition.toIR, BitVecLiteral(0, b.condition.size)), negatedConditions)
                  val newLabel = s"${block.label}_goto_${target.label}"
                  val assume = Assume(condition, checkSecurity = true)
                  val newBlock = Block(newLabel, None, ArrayBuffer(assume), GoTo(ArrayBuffer(target), None))
                  newBlocks.append(newBlock)
                  targets.append(newBlock)
                  conditions.append(b.condition)
                }
            }
          case _ => throw Exception("translation error, call where not expected: " + jumps.mkString(", "))
        }
      }
      (GoTo(targets, Some(line)), newBlocks)
    } else {
      jumps.head match {
        case b: BAPDirectCall =>
          (DirectCall(
            nameToProcedure(b.target),
            b.returnTarget.map(t => labelToBlock(t)),
            Some(b.line)
          ), ArrayBuffer())
        case b: BAPIndirectCall =>
          (IndirectCall(b.target.toIR, b.returnTarget.map(t => labelToBlock(t)), Some(b.line)), ArrayBuffer())
        case b: BAPGoTo =>
          val target = labelToBlock(b.target)
          coerceToBool(b.condition) match {
            case Some(c) =>
              val newLabel = s"${block.label}_goto_${target.label}"
              val assume = Assume(c, checkSecurity = true)
              val newBlock = Block(newLabel, None, ArrayBuffer(assume), GoTo(ArrayBuffer(target), None))
              (GoTo(ArrayBuffer(newBlock), Some(b.line)), ArrayBuffer(newBlock))
            case None =>
              (GoTo(ArrayBuffer(target), Some(b.line)), ArrayBuffer())
          }
      }
    }
  }

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
