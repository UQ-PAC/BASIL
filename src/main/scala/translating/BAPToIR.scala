package translating

import bap.*
import boogie.UnaryBExpr
import ir.{UnaryExpr, BinaryExpr, *}
import specification.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.TreeMap
import util.intrusive_list.*

class BAPToIR(var program: BAPProgram, mainAddress: BigInt) {

  private val nameToProcedure: mutable.Map[String, Procedure] = mutable.Map()
  private val labelToBlock: mutable.Map[String, Block] = mutable.Map()

  private var loadCounter: Int = 0

  def translate: Program = {
    var mainProcedure: Option[Procedure] = None
    val procedures: ArrayBuffer[Procedure] = ArrayBuffer()
    for (s <- program.subroutines) {
      val procedure = Procedure(s.name, s.address)
      for (b <- s.blocks) {
        val block = Block(b.label, b.address)
        procedure.addBlocks(block)
        if (b.address.isDefined && b.address.isDefined && b.address.get == procedure.address.get) {
          procedure.entryBlock = block
        }
        labelToBlock.addOne(b.label, block)
      }
      for (p <- s.in) {
        procedure.in.append(translateParameter(p))
      }
      for (p <- s.out) {
        procedure.out.append(translateParameter(p))
      }
      if (s.address.contains(mainAddress)) {
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
          val statements = translateStatement(st)
          for (s <- statements) {
            block.statements.append(s)
          }
        }
        val (call, jump, newBlocks) = translate(b.jumps, block)
        procedure.addBlocks(newBlocks)
        call.foreach(c => block.statements.append(c))
        block.replaceJump(jump)
        assert(jump.hasParent)
      }

      // Set entry block to the block with the same address as the procedure or the first in sequence
      procedure.blocks.find(b => b.address == procedure.address).foreach(procedure.entryBlock = _)
      if procedure.entryBlock.isEmpty then procedure.blocks.nextOption().foreach(procedure.entryBlock = _)
      // TODO maybe throw an exception if there is no block with the same address, to be safe?

    }

    val memorySections: mutable.TreeMap[BigInt, MemorySection] = mutable.TreeMap()
    for (m <- program.memorySections) {
      val bytes = if (m.name == ".bss" && m.bytes.isEmpty) {
        for (_ <- 0 until m.size) yield BitVecLiteral(0, 8)
      } else {
        m.bytes.map(translateLiteral)
      }
      val readOnly = m.name == ".rodata" || m.name == ".got" // crude heuristic
      memorySections.addOne(m.address, MemorySection(m.name, m.address, m.size, bytes, readOnly, None))
    }

    Program(procedures, mainProcedure.get, memorySections)
  }

  private def translateStatement(s: BAPStatement): Seq[Statement] = s match {
    case b: BAPMemAssign =>
      val mem = translateMemory(b.lhs)
      if (mem != translateMemory(b.rhs.memory)) {
        throw Exception(s"$b has conflicting lhs ${b.lhs} and rhs ${b.rhs.memory}")
      }
      Seq(MemoryStore(mem, translateExprOnly(b.rhs.index), translateExprOnly(b.rhs.value), b.rhs.endian, b.rhs.size, Some(b.line)))
    case b: BAPLocalAssign =>
      val lhs = translateVar(b.lhs)
      val (rhs, load) = translateExpr(b.rhs)
      if (load.isDefined) {
        val loadWithLabel = MemoryLoad(load.get.lhs, load.get.mem, load.get.index, load.get.endian, load.get.size, Some(b.line + "$0"))
        val assign = LocalAssign(lhs, rhs, Some(b.line + "$1"))
        Seq(loadWithLabel, assign)
      } else {
        val assign = LocalAssign(lhs, rhs, Some(b.line))
        Seq(assign)
      }
  }


  private def translateExpr(e: BAPExpr): (Expr, Option[MemoryLoad]) = 
    try {
      _translateExpr(e)
    } catch {
      case exp: Exception => {
        Logger.error(s"Error translating $e:\n\t" + e)
        throw exp
      }
  }

  private def _translateExpr(e: BAPExpr): (Expr, Option[MemoryLoad]) = e match {
    case b @ BAPConcat(left, right) =>
      val (arg0, load0) = translateExpr(left)
      val (arg1, load1) = translateExpr(right)
      (load0, load1) match {
        case (Some(load), None) => (BinaryExpr(BVCONCAT, arg0, arg1), Some(load))
        case (None, Some(load)) => (BinaryExpr(BVCONCAT, arg0, arg1), Some(load))
        case (None, None) => (BinaryExpr(BVCONCAT, arg0, arg1), None)
        case (Some(_), Some(_)) => throw Exception(s"$b contains multiple loads")
      }
    case BAPSignedExtend(width, body) =>
      if (width > body.size) {
        val (irBody, load) = translateExpr(body)
        val se = SignExtend(width - body.size, irBody)
        (se, load)
      } else {
        translateExpr(BAPExtract(width - 1, 0, body))
      }
    case BAPUnsignedExtend(width, body) =>
      if (width > body.size) {
        val (irBody, load) = translateExpr(body)
        val ze = ZeroExtend(width - body.size, irBody)
        (ze, load)
      } else {
        translateExpr(BAPExtract(width - 1, 0, body))
      }
    case b @ BAPExtract(high, low, body) =>
      val bodySize = body.size
      val (irBody, load) = translateExpr(body)
      val extract = if (b.size > bodySize) {
        if (low == 0) {
          ZeroExtend(b.size - bodySize, irBody)
        } else {
          Extract(high + 1, low, ZeroExtend(b.size - bodySize, irBody))
        }
      } else {
        Extract(high + 1, low, irBody)
      }
      (extract, load)
    case literal: BAPLiteral => (translateLiteral(literal), None)
    case BAPUnOp(operator, exp) => operator match {
      case NOT => (UnaryExpr(BVNOT, translateExprOnly(exp)), None)
      case NEG => (UnaryExpr(BVNEG, translateExprOnly(exp)), None)
    }
    case BAPBinOp(operator, lhs, rhs) => 
      val (arg0, l1) = translateExpr(lhs)
      val (arg1, l2) = translateExpr(rhs)
      assert(!(l1.isDefined && l2.isDefined), "Don't expect two loads in an expression")
      val load : Option[MemoryLoad] = l1.orElse(l2)

      operator match {
      case PLUS => (BinaryExpr(BVADD, arg0, arg1), load)
      case MINUS => (BinaryExpr(BVSUB, arg0, arg1), load)
      case TIMES => (BinaryExpr(BVMUL, arg0, arg1), load)
      case DIVIDE => (BinaryExpr(BVUDIV, arg0, arg1), load)
      case SDIVIDE => (BinaryExpr(BVSDIV, arg0, arg1), load)
      // counterintuitive but correct according to BAP source
      case MOD => (BinaryExpr(BVSREM, arg0, arg1), load)
      // counterintuitive but correct according to BAP source
      case SMOD => (BinaryExpr(BVUREM, arg0, arg1), load)
      case LSHIFT => // BAP says caring about this case is necessary?
        if (lhs.size == rhs.size) {
          (BinaryExpr(BVSHL, arg0, arg1), load)
        } else {
          (BinaryExpr(BVSHL, arg0, ZeroExtend(lhs.size - rhs.size, arg1)), load)
        }
      case RSHIFT =>
        if (lhs.size == rhs.size) {
          (BinaryExpr(BVLSHR, arg0, arg1), load)
        } else {
          (BinaryExpr(BVLSHR, arg0, ZeroExtend(lhs.size - rhs.size, arg1)), load)
        }
      case ARSHIFT =>
        if (lhs.size == rhs.size) {
          (BinaryExpr(BVASHR, arg0, arg1), load)
        } else {
          (BinaryExpr(BVASHR, arg0, ZeroExtend(lhs.size - rhs.size, arg1)), load)
        }
      case AND => (BinaryExpr(BVAND, arg0, arg1), load)
      case OR => (BinaryExpr(BVOR, arg0, arg1), load)
      case XOR => (BinaryExpr(BVXOR, arg0, arg1), load)
      case EQ => (BinaryExpr(BVCOMP, arg0, arg1), load)
      case NEQ => (UnaryExpr(BVNOT, BinaryExpr(BVCOMP, arg0, arg1)), load)
      case LT => (BinaryExpr(BVULT, arg0, arg1), load)
      case LE => (BinaryExpr(BVULE, arg0, arg1), load)
      case SLT => (BinaryExpr(BVSLT, arg0, arg1), load)
      case SLE => (BinaryExpr(BVSLE, arg0, arg1), load)
    }
    case b: BAPVar => (translateVar(b), None)
    case BAPMemAccess(memory, index, endian, size) =>
      val temp = LocalVar("$load$" + loadCounter, BitVecType(size))
      loadCounter += 1
      val load = MemoryLoad(temp, translateMemory(memory), translateExprOnly(index), endian, size, None)
      (temp, Some(load))
  }

  private def translateExprOnly(e: BAPExpr) = {
    val (expr, load) = translateExpr(e)
    if (load.isDefined) {
      throw Exception(s"unexpected load in $e")
    }
    expr
  }

  private def translateVar(variable: BAPVar): Variable = variable match {
    case BAPRegister(name, size) => Register(name, size)
    case BAPLocalVar(name, size) => LocalVar(name, BitVecType(size))
  }

  private def translateMemory(memory: BAPMemory): Memory = {
    SharedMemory(memory.name, memory.addressSize, memory.valueSize)
  }

  private def translateParameter(parameter: BAPParameter): Parameter = {
    val register = translateExprOnly(parameter.value)
    register match {
      case r: Register => Parameter(parameter.name, parameter.size, r)
      case _ => throw Exception(s"subroutine parameter $this refers to non-register variable ${parameter.value}")
    }
  }

  private def translateLiteral(literal: BAPLiteral) = {
    BitVecLiteral(literal.value, literal.size)
  }


  /**
    * Translates a list of jumps from BAP into a single Jump at the IR level by moving any conditions on jumps to
    * Assume statements in new blocks
    * */
  private def translate(jumps: List[BAPJump], block: Block): (Option[Call], Jump, ArrayBuffer[Block]) = {
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
                  val conditionsIR = conditions.map(c => {
                    val (e, l) = convertConditionBool(c, true)
                    (e, l.toList)
                  })
                  val (condition,loads) = conditionsIR.tail.foldLeft(conditionsIR.head) {
                    (ands, next) => (BinaryExpr(BoolAND, next(0), ands(0)), next(1) ++ ands(1))
                  }
                  val newBlock = newBlockCondition(block, target, loads, condition)
                  newBlocks.append(newBlock)
                  targets.append(newBlock)
                }
              // non-true condition
              case _ =>
                val (currentCondition, currentL) = convertConditionBool(b.condition, false)
                val (condition,load) = if (conditions.isEmpty) {
                  // if this is the first condition then it is the only relevant part of the condition
                  (currentCondition, currentL.toList)
                } else {
                  // if this is not the first condition, then we need to need to add
                  // that all previous conditions are false
                  val conditionsIR = conditions.map(c => {
                    val (e,l) = convertConditionBool(c, true)
                    (e, l.toList)
                  })
                  conditionsIR.tail.foldLeft((currentCondition, currentL.toList)) {
                    (ands, next) => (BinaryExpr(BoolAND, next(0), ands(0)), next(1) ++ ands(1))
                  }
                }
                val newBlock = newBlockCondition(block, target, load, condition)
                newBlocks.append(newBlock)
                targets.append(newBlock)
                conditions.append(b.condition)
            }
          case _ => throw Exception("translation error, call where not expected: " + jumps.mkString(", "))
        }
      }
      (None, GoTo(targets, Some(line)), newBlocks)
    } else {
      jumps.head match {
        case b: BAPDirectCall =>
          val call = Some(DirectCall(nameToProcedure(b.target),Some(b.line)))
          val ft = b.returnTarget.map(t => labelToBlock(t)).map(x => GoTo(Set(x))).getOrElse(Unreachable())
          (call, ft, ArrayBuffer())
        case b: BAPIndirectCall =>
          val call = IndirectCall(translateVar(b.target), Some(b.line))
          val ft = b.returnTarget.map(t => labelToBlock(t)).map(x => GoTo(Set(x))).getOrElse(Unreachable())
          (Some(call), ft, ArrayBuffer())
        case b: BAPGoTo =>
          val target = labelToBlock(b.target)
          b.condition match {
            // condition is true
            case l: BAPLiteral if l.value > BigInt(0) =>
              (None, GoTo(ArrayBuffer(target), Some(b.line)), ArrayBuffer())
            // non-true condition
            case _ =>
              val (condition,load) = convertConditionBool(b.condition, false)
              val newBlock = newBlockCondition(block, target, load.toSeq, condition)
              (None, GoTo(ArrayBuffer(newBlock), Some(b.line)), ArrayBuffer(newBlock))
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
  private def convertConditionBool(expr: BAPExpr, negative: Boolean): (Expr, Option[MemoryLoad]) = {
    val (e,l) = translateExpr(expr)
    val exp = e.getType match {
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
    (exp, l)
  }

  private def newBlockCondition(block: Block, target: Block, condLoad: Seq[MemoryLoad], condition: Expr): Block = {
    val newLabel = s"${block.label}_goto_${target.label}"
    val assume = Assume(condition, checkSecurity = true)
    Block(newLabel, None, condLoad.toSeq ++ Seq(assume), GoTo(ArrayBuffer(target)))
  }

}
