package translating

import bap.*
import boogie.UnaryBExpr
import ir.{UnaryExpr, BinaryExpr, *}
import specification.*

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import util.intrusive_list.IntrusiveList
import util.Logger

class BAPToIR(var program: BAPProgram, mainAddress: Int) {

  private val nameToProcedure: mutable.Map[String, Procedure] = mutable.Map()
  private val labelToBlock: mutable.Map[String, Block] = mutable.Map()

  def translate: Program = {
    var mainProcedure: Option[Procedure] = None
    val procedures: ArrayBuffer[Procedure] = ArrayBuffer()
    for (s <- program.subroutines) {
      val procedure = Procedure(s.name, Some(s.address))
      for (b <- s.blocks) {
        val block = Block(b.label, b.address)
        procedure.addBlock(block)

        if (b.address.isDefined && b.address.isDefined && b.address.get == procedure.address.get) {
          procedure.entryBlock.replaceJump(GoTo(block))
        }
        labelToBlock.addOne(b.label, block)
      }
      for (p <- s.in) {
        procedure.in.append(p.toIR)
      }
      for (p <- s.out) {
        procedure.out.append(p.toIR)
      }
      if (s.address == mainAddress) {
        mainProcedure = Some(procedure)
      }
      procedures.append(procedure)
      nameToProcedure.addOne(s.name, procedure)
    }

    for (s <- program.subroutines) {
      val procedure: Procedure = nameToProcedure(s.name)
      for (b <- s.blocks) {
        val block = labelToBlock(b.label)
        for (st <- b.statements) {
          block.statements.append(translate(st))
        }
        var (jump, newBlocks) = translate(b.jumps, block)
        procedure.addBlocks(newBlocks)
        jump = block.replaceJump(jump)
        assert(jump.hasParent)
      }

      // Set entry block to the block with the same address as the procedure or the first in sequence
      procedure.blocks.find(b => b.address == procedure.address) match 
        case Some(x) => procedure.entryBlock.replaceJump(GoTo(Set(x)))
        case None =>
          if (procedure.hasImplementation) {
            // TODO maybe throw an exception if there is no block with the same address, to be safe?
            Logger.warn("No block with procedure's address found; using the first available block as the entry")
            procedure.innerBlocks.nextOption().foreach(b => procedure.entryBlock.replaceJump(GoTo(Set(b))))
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

  /**
    * Translates a list of jumps from BAP into a single Jump at the IR level by moving any conditions on jumps to
    * Assume statements in new blocks
    * */
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
              // condition is true
              case l: BAPLiteral if l.value > BigInt(0) =>
                // condition is true and no previous conditions means no assume block needed
                if (conditions.isEmpty) {
                  targets.append(target)
                } else {
                  // condition is true and previous conditions existing means this condition
                  // is actually that all previous conditions are false
                  val conditionsIR = conditions.map(c => convertConditionBool(c, true))
                  val condition = conditionsIR.tail.foldLeft(conditionsIR.head)((ands: Expr, next: Expr) => BinaryExpr(BoolAND, next, ands))
                  val newBlock = newBlockCondition(block, target, condition)
                  newBlocks.append(newBlock)
                  targets.append(newBlock)
                }
              // non-true condition
              case _ =>
                val currentCondition = convertConditionBool(b.condition, false)
                val condition = if (conditions.isEmpty) {
                  // if this is the first condition then it is the only relevant part of the condition
                  currentCondition
                } else {
                  // if this is not the first condition, then we need to need to add
                  // that all previous conditions are false
                  val conditionsIR = conditions.map(c => convertConditionBool(c, true))
                  conditionsIR.tail.foldLeft(currentCondition)((ands: Expr, next: Expr) => BinaryExpr(BoolAND, next, ands))
                }
                val newBlock = newBlockCondition(block, target, condition)
                newBlocks.append(newBlock)
                targets.append(newBlock)
                conditions.append(b.condition)
            }
          case _ => throw Exception("translation error, call where not expected: " + jumps.mkString(", "))
        }
      }
      (GoTo(targets, Some(line)), newBlocks)
    } else {
      jumps.head match {
        case b: BAPDirectCall =>
          val call = DirectCall(nameToProcedure(b.target), b.returnTarget.map(t => labelToBlock(t)), Some(b.line))
          (call, ArrayBuffer())
        case b: BAPIndirectCall =>
          val call = IndirectCall(b.target.toIR, b.returnTarget.map(t => labelToBlock(t)), Some(b.line))
          (call, ArrayBuffer())
        case b: BAPGoTo =>
          val target = labelToBlock(b.target)
          b.condition match {
            // condition is true
            case l: BAPLiteral if l.value > BigInt(0) =>
              (GoTo(ArrayBuffer(target), Some(b.line)), ArrayBuffer())
            // non-true condition
            case _ =>
              val condition = convertConditionBool(b.condition, false)
              val newBlock = newBlockCondition(block, target, condition)
              (GoTo(ArrayBuffer(newBlock), Some(b.line)), ArrayBuffer(newBlock))
          }
      }
    }
  }

  /**
    * Converts a BAPExpr condition that returns a bitvector of size 1 to an Expr condition that returns a Boolean
    *
    * If negative is true then the negation of the condition is returned
    *
    * If the BAPExpr uses a comparator that returns a Boolean then no further conversion is performed except negation,
    * if necessary.
    * */
  private def convertConditionBool(expr: BAPExpr, negative: Boolean): Expr = {
    val e = expr.toIR
    e.getType match {
      case BitVecType(s) =>
        if (negative) {
          BinaryExpr(BVEQ, e, BitVecLiteral(0, s))
        } else {
          BinaryExpr(BVNEQ, e, BitVecLiteral(0, s))
        }
      case BoolType =>
        if (negative) {
          UnaryExpr(BoolNOT, e)
        } else {
          e
        }
      case _ => ???
    }
  }

  private def newBlockCondition(block: Block, target: Block, condition: Expr): Block = {
    val newLabel = s"${block.label}_goto_${target.label}"
    val assume = Assume(condition, checkSecurity = true)
    Block(newLabel, None, ArrayBuffer(assume), GoTo(ArrayBuffer(target)))
  }

}
