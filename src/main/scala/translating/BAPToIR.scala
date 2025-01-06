package translating

import bap.*
import boogie.UnaryBExpr
import ir.{UnaryExpr, BinaryExpr, *}
import specification.*
import ir.cilvisitor.*

import scala.collection.mutable
import scala.collection.immutable
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
      procedure.formalInParam = mutable.SortedSet.from(s.in.map(translateParam))
      procedure.formalOutParam = mutable.SortedSet.from(s.out.filterNot(_.name.endsWith("_result")).map(translateParam))
      procedure.inParamDefaultBinding = immutable.SortedMap.from(s.in.map(s => translateParam(s)-> paramRegisterRVal(s)))
      procedure.outParamDefaultBinding = immutable.SortedMap.from(s.out.filterNot(_.name.endsWith("_result")).map(s => translateParam(s) -> paramRegisterLVal(s)))

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

    class FixCallParams(subroutines: immutable.Map[String, BAPSubroutine]) extends CILVisitor {
      override def vstmt(st: Statement) = st match {
        case d: DirectCall => {
          if (subroutines.contains(d.target.name)) {
            val s = subroutines(d.target.name)
            ChangeTo(List(DirectCall(d.target, d.label, 
              d.target.outParamDefaultBinding,
              d.target.inParamDefaultBinding)))
          }  else {
            SkipChildren()
          }
        }
        case _ => SkipChildren()
      }
    }

    var prog = Program(procedures, mainProcedure.get, memorySections)
    visit_prog(FixCallParams(program.subroutines.map(s => s.name -> s).toMap), prog)
    prog
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

  private def translateExpr(e: BAPExpr): (Expr, Option[MemoryLoad]) = e match {
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
    case BAPBinOp(operator, lhs, rhs) => operator match {
      case PLUS => (BinaryExpr(BVADD, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case MINUS => (BinaryExpr(BVSUB, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case TIMES => (BinaryExpr(BVMUL, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case DIVIDE => (BinaryExpr(BVUDIV, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case SDIVIDE => (BinaryExpr(BVSDIV, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      // counterintuitive but correct according to BAP source
      case MOD => (BinaryExpr(BVSREM, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      // counterintuitive but correct according to BAP source
      case SMOD => (BinaryExpr(BVUREM, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case LSHIFT => // BAP says caring about this case is necessary?
        if (lhs.size == rhs.size) {
          (BinaryExpr(BVSHL, translateExprOnly(lhs), translateExprOnly(rhs)), None)
        } else {
          (BinaryExpr(BVSHL, translateExprOnly(lhs), ZeroExtend(lhs.size - rhs.size, translateExprOnly(rhs))), None)
        }
      case RSHIFT =>
        if (lhs.size == rhs.size) {
          (BinaryExpr(BVLSHR, translateExprOnly(lhs), translateExprOnly(rhs)), None)
        } else {
          (BinaryExpr(BVLSHR, translateExprOnly(lhs), ZeroExtend(lhs.size - rhs.size, translateExprOnly(rhs))), None)
        }
      case ARSHIFT =>
        if (lhs.size == rhs.size) {
          (BinaryExpr(BVASHR, translateExprOnly(lhs), translateExprOnly(rhs)), None)
        } else {
          (BinaryExpr(BVASHR, translateExprOnly(lhs), ZeroExtend(lhs.size - rhs.size, translateExprOnly(rhs))), None)
        }
      case AND => (BinaryExpr(BVAND, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case OR => (BinaryExpr(BVOR, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case XOR => (BinaryExpr(BVXOR, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case EQ => (BinaryExpr(BVCOMP, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case NEQ => (UnaryExpr(BVNOT, BinaryExpr(BVCOMP, translateExprOnly(lhs), translateExprOnly(rhs))), None)
      case LT => (BinaryExpr(BVULT, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case LE => (BinaryExpr(BVULE, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case SLT => (BinaryExpr(BVSLT, translateExprOnly(lhs), translateExprOnly(rhs)), None)
      case SLE => (BinaryExpr(BVSLE, translateExprOnly(lhs), translateExprOnly(rhs)), None)
    }
    case b: BAPVar => (translateVar(b), None)
    case BAPMemAccess(memory, index, endian, size) =>
      val temp = LocalVar("load" + loadCounter, BitVecType(size))
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

  private def paramRegisterLVal(param: BAPParameter): Variable = translateVar(param.value)
  private def toIROutParam(param: BAPParameter) =  {
    paramRegisterLVal(param) match {
      case r: Register => {
        if (r.size == param.size) then {
          translateParam(param)
        } else {
          LocalVar(param.name, BitVecType(r.size))
        }
      }
      case _ => throw Exception(s"subroutine parameter $this refers to non-register variable ${param.value}")
    }

  }
  
  private def paramRegisterRVal(p: BAPParameter): Expr = {
    paramRegisterLVal(p) match {
      case r: Register => {
        if (r.size == p.size) then {
          r
        } else if (r.size > p.size){
          Extract(p.size, 0, r)
        } else {
          ZeroExtend(p.size - r.size, r)
        }
      }
      case _ => throw Exception(s"subroutine parameter $this refers to non-register variable ${p.value}")
    }
  }
  def paramVariableRVal(p: BAPParameter): Expr = {
    paramRegisterLVal(p) match {
      case r: Register => {
        if (r.size == p.size) then {
          translateParam(p)
        } else {
          ZeroExtend(r.size - p.size, translateParam(p))
        }
      }
      case _ => throw Exception(s"subroutine parameter $this refers to non-register variable ${p.value}")
    }
  }

  def toAssignOut(p: BAPParameter) : LocalAssign = {
    LocalAssign(translateParam(p), paramRegisterRVal(p))
  }

  def toAssignIn(p: BAPParameter) : LocalAssign = {
    LocalAssign(paramRegisterLVal(p), paramVariableRVal(p))
  }

  def translateParam(p: BAPParameter): LocalVar = LocalVar(p.name, BitVecType(p.size))

  private def translateVar(variable: BAPVar): Variable = variable match {
    case BAPRegister(name, size) => Register(name, size)
    case BAPLocalVar(name, size) => LocalVar(name, BitVecType(size))
  }

  private def translateMemory(memory: BAPMemory): Memory = {
    SharedMemory(memory.name, memory.addressSize, memory.valueSize)
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
                  val conditionsIR = conditions.map(c => convertConditionBool(c, true))
                  val condition = conditionsIR.tail.foldLeft(conditionsIR.head) {
                    (ands: Expr, next: Expr) => BinaryExpr(BoolAND, next, ands)
                  }
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
                  conditionsIR.tail.foldLeft(currentCondition) {
                    (ands: Expr, next: Expr) => BinaryExpr(BoolAND, next, ands)
                  }
                }
                val newBlock = newBlockCondition(block, target, condition)
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
          val call = Some(DirectCall(nameToProcedure(b.target), Some(b.line)))
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
              val condition = convertConditionBool(b.condition, false)
              val newBlock = newBlockCondition(block, target, condition)
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
  private def convertConditionBool(expr: BAPExpr, negative: Boolean): Expr = {
    val e = translateExprOnly(expr)
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
