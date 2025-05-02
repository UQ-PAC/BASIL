package translating

import bap.*
import boogie.UnaryBExpr
import ir.*
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
        procedure.addBlock(block)
        if (b.address.isDefined && b.address.isDefined && b.address.get == procedure.address.get) {
          procedure.entryBlock = block
        }
        labelToBlock.addOne(b.label, block)
      }
      procedure.formalInParam = mutable.SortedSet.from(s.in.map(translateParam))
      procedure.formalOutParam = mutable.SortedSet.from(s.out.filterNot(_.name.endsWith("_result")).map(translateParam))
      procedure.inParamDefaultBinding =
        immutable.SortedMap.from(s.in.map(s => translateParam(s) -> paramRegisterRVal(s)))
      procedure.outParamDefaultBinding = immutable.SortedMap.from(
        s.out.filterNot(_.name.endsWith("_result")).map(s => translateParam(s) -> paramRegisterLVal(s))
      )

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
        val (call, jump, newBlocks, atomic) = translate(b.jumps, block)
        block.statements.addAll(atomic)
        procedure.addBlocks(newBlocks)
        call.foreach(c => block.statements.append(c))
        block.replaceJump(jump)
        assert(jump.hasParent)
      }

      if (procedure.blocks.nonEmpty) {
        handleAtomicSections(procedure)
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
            ChangeTo(
              List(DirectCall(d.target, d.label, d.target.outParamDefaultBinding, d.target.inParamDefaultBinding))
            )
          } else {
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

  private def handleAtomicSections(procedure: Procedure): Unit = {
    val queue = mutable.Queue[Block](procedure.entryBlock.get)
    var atomicSectionStart: Option[Block] = None
    var atomicSectionEnd: Option[Block] = None
    val atomicSectionContents: mutable.Set[Block] = mutable.Set()
    val visited = mutable.Set[Block]()

    while (queue.nonEmpty) {
      val block = queue.dequeue()

      if (atomicSectionStart.isDefined) {
        atomicSectionContents.add(block)
      }

      if (block.statements.nonEmpty) {
        var statement = block.statements.head
        var breakLoop = false
        while (!breakLoop) {
          statement match {
            case a: AtomicStart =>
              if (atomicSectionStart.nonEmpty) {
                // this should not happen and if there is ever any combination of instructions that causes it to happen
                // it probably produces undefined behaviour
                throw Exception("nested atomic sections")
              }

              val nextBlock = block.jump.match {
                case g: GoTo if g.targets.size == 1 => g.targets.head
                case _ => throw Exception("expected block with AtomicStart to have GoTo to single target")
              }

              if (block.statements.hasNext(a)) {
                throw Exception("expected AtomicStart to be final statement in block")
              }

              atomicSectionStart = Some(nextBlock)

              block.statements.insertAfter(a, Assert(TrueLiteral, Some("next block is atomic start")))
              block.statements.remove(a)

            case a: AtomicEnd =>
              if (atomicSectionStart.isEmpty) {
                // this should not happen and if there is ever any combination of instructions that causes it to happen
                // it probably produces undefined behaviour on the actual hardware
                throw Exception("nested atomic sections")
              }

              block.jump.match {
                case g: GoTo if g.targets.size == 1 =>
                case _ => throw Exception("expected block with AtomicEnd to have GoTo to single target")
              }

              if (block.statements.hasNext(a)) {
                throw Exception("expected AtomicEnd to be final statement in block")
              }

              atomicSectionEnd = Some(block)

              block.statements.insertAfter(a, Assert(TrueLiteral, Some("this block is atomic end")))
              block.statements.remove(a)

            case _ =>
          }
          if (block.statements.hasNext(statement)) {
            statement = block.statements.getNext(statement)
          } else {
            breakLoop = true
          }
        }
      }

      // only resolve atomic section if all blocks between the start and end of the atomic section have been traversed
      if (atomicSectionEnd.contains(block)) {
        if (queue.isEmpty) {
          val atomicSection = AtomicSection(atomicSectionStart.get, block, atomicSectionContents.clone())
          atomicSectionContents.foreach(_.atomicSection = Some(atomicSection))
          atomicSectionStart = None
          atomicSectionEnd = None
          atomicSectionContents.clear()
          visited.add(block)
        }
      } else {
        visited.add(block)
      }

      // do not traverse blocks past the end of the atomic section unless all blocks in the atomic section have
      // been traversed
      if (visited.contains(block)) {
        block.jump match {
          case g: GoTo =>
            g.targets.foreach { target =>
              if (!visited.contains(target)) {
                queue.enqueue(target)
              }
            }
          case _ =>
        }
      }
    }

    if (atomicSectionStart.isDefined || atomicSectionEnd.isDefined || atomicSectionContents.nonEmpty) {
      throw Exception(
        "error handling atomic sections - left atomic section partially resolved after traversing procedure"
      )
    }

  }

  private def translateStatement(s: BAPStatement): Seq[Statement] = s match {
    case b: BAPMemAssign =>
      val mem = translateMemory(b.lhs)
      if (mem != translateMemory(b.rhs.memory)) {
        throw Exception(s"$b has conflicting lhs ${b.lhs} and rhs ${b.rhs.memory}")
      }
      Seq(
        MemoryStore(
          mem,
          translateExprOnly(b.rhs.index),
          translateExprOnly(b.rhs.value),
          b.rhs.endian,
          b.rhs.size,
          Some(b.line)
        )
      )
    case b: BAPLocalAssign =>
      val lhs = translateVar(b.lhs)
      val (rhs, load) = translateExpr(b.rhs)
      if (load.isDefined) {
        val loadWithLabel =
          MemoryLoad(load.get.lhs, load.get.mem, load.get.index, load.get.endian, load.get.size, Some(b.line + "$0"))
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
    case BAPUnOp(operator, exp) =>
      operator match {
        case NOT => (UnaryExpr(BVNOT, translateExprOnly(exp)), None)
        case NEG => (UnaryExpr(BVNEG, translateExprOnly(exp)), None)
      }
    case BAPBinOp(operator, lhs, rhs) =>
      val (lhsIR, lhsLoad) = translateExpr(lhs)
      val (rhsIR, rhsLoad) = translateExpr(rhs)
      val load: Option[MemoryLoad] = (lhsLoad, rhsLoad) match {
        case (Some(_), Some(_)) => throw Exception("memory load found in both sides of BAP binary expression")
        case (Some(x), None) => Some(x)
        case (None, Some(y)) => Some(y)
        case (None, None) => None
      }
      operator match {
        case PLUS => (BinaryExpr(BVADD, lhsIR, rhsIR), load)
        case MINUS => (BinaryExpr(BVSUB, lhsIR, rhsIR), load)
        case TIMES => (BinaryExpr(BVMUL, lhsIR, rhsIR), load)
        case DIVIDE => (BinaryExpr(BVUDIV, lhsIR, rhsIR), load)
        case SDIVIDE => (BinaryExpr(BVSDIV, lhsIR, rhsIR), load)
        // counterintuitive but correct according to BAP source
        case MOD => (BinaryExpr(BVSREM, lhsIR, rhsIR), load)
        // counterintuitive but correct according to BAP source
        case SMOD => (BinaryExpr(BVUREM, lhsIR, rhsIR), load)
        case LSHIFT => // BAP says caring about this case is necessary?
          if (lhs.size == rhs.size) {
            (BinaryExpr(BVSHL, lhsIR, rhsIR), load)
          } else {
            (BinaryExpr(BVSHL, lhsIR, ZeroExtend(lhs.size - rhs.size, rhsIR)), load)
          }
        case RSHIFT =>
          if (lhs.size == rhs.size) {
            (BinaryExpr(BVLSHR, lhsIR, rhsIR), load)
          } else {
            (BinaryExpr(BVLSHR, lhsIR, ZeroExtend(lhs.size - rhs.size, rhsIR)), load)
          }
        case ARSHIFT =>
          if (lhs.size == rhs.size) {
            (BinaryExpr(BVASHR, lhsIR, rhsIR), load)
          } else {
            (BinaryExpr(BVASHR, lhsIR, ZeroExtend(lhs.size - rhs.size, rhsIR)), load)
          }
        case AND => (BinaryExpr(BVAND, lhsIR, rhsIR), load)
        case OR => (BinaryExpr(BVOR, lhsIR, rhsIR), load)
        case XOR => (BinaryExpr(BVXOR, lhsIR, rhsIR), load)
        case EQ => (BinaryExpr(BVCOMP, lhsIR, rhsIR), load)
        case NEQ => (UnaryExpr(BVNOT, BinaryExpr(BVCOMP, lhsIR, rhsIR)), load)
        case LT => (BinaryExpr(BVULT, lhsIR, rhsIR), load)
        case LE => (BinaryExpr(BVULE, lhsIR, rhsIR), load)
        case SLT => (BinaryExpr(BVSLT, lhsIR, rhsIR), load)
        case SLE => (BinaryExpr(BVSLE, lhsIR, rhsIR), load)
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
  private def toIROutParam(param: BAPParameter) = {
    paramRegisterLVal(param) match {
      case r @ Register(_, size) => {
        if (size == param.size) {
          translateParam(param)
        } else {
          LocalVar(param.name, BitVecType(size))
        }
      }
      case _ => throw Exception(s"subroutine parameter $this refers to non-register variable ${param.value}")
    }

  }

  private def paramRegisterRVal(p: BAPParameter): Expr = {
    paramRegisterLVal(p) match {
      case r @ Register(n, size) => {
        if (size == p.size) {
          r
        } else if (size > p.size) {
          Extract(p.size, 0, r)
        } else {
          ZeroExtend(p.size - size, r)
        }
      }
      case _ => throw Exception(s"subroutine parameter $this refers to non-register variable ${p.value}")
    }
  }
  def paramVariableRVal(p: BAPParameter): Expr = {
    paramRegisterLVal(p) match {
      case r @ Register(_, size) => {
        if (size == p.size) {
          translateParam(p)
        } else {
          ZeroExtend(size - p.size, translateParam(p))
        }
      }
      case _ => throw Exception(s"subroutine parameter $this refers to non-register variable ${p.value}")
    }
  }

  def toAssignOut(p: BAPParameter): LocalAssign = {
    LocalAssign(translateParam(p), paramRegisterRVal(p))
  }

  def toAssignIn(p: BAPParameter): LocalAssign = {
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
  private def translate(
    jumps: List[BAPJump],
    block: Block
  ): (Option[Call], Jump, ArrayBuffer[Block], Iterable[Statement]) = {
    if (jumps.size > 1) {
      val targets = ArrayBuffer[Block]()
      val conditions = ArrayBuffer[Expr]()
      val line = jumps.head.line
      val newBlocks = ArrayBuffer[Block]()
      val newStatements = ArrayBuffer[Statement]()
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
                  val condition = conditionsIR.tail.foldLeft(conditionsIR.head) { (ands: Expr, next: Expr) =>
                    BinaryExpr(BoolAND, next, ands)
                  }
                  val newBlock = newBlockCondition(block, target, condition)
                  newBlocks.append(newBlock)
                  targets.append(newBlock)
                }
              // non-true condition
              case _ =>
                val (expr, load) = translateExpr(b.condition)
                val currentCondition = convertConditionBool(expr, false)
                newStatements.appendAll(load)
                val condition = if (conditions.isEmpty) {
                  // if this is the first condition then it is the only relevant part of the condition
                  currentCondition
                } else {
                  // if this is not the first condition, then we need to need to add
                  // that all previous conditions are false
                  val conditionsIR = conditions.map(c => convertConditionBool(c, true))
                  conditionsIR.tail.foldLeft(currentCondition) { (ands: Expr, next: Expr) =>
                    BinaryExpr(BoolAND, next, ands)
                  }
                }
                val newBlock = newBlockCondition(block, target, condition)
                newBlocks.append(newBlock)
                targets.append(newBlock)
                conditions.append(expr)
            }
          case _ => throw Exception("translation error, call where not expected: " + jumps.mkString(", "))
        }
      }
      (None, GoTo(targets, Some(line)), newBlocks, newStatements)
    } else {
      jumps.head match {
        case b: BAPDirectCall =>
          b.target match {
            case "intrinsic$AtomicStart" =>
              val atomicStart = AtomicStart(Some(b.line))
              val goto =
                b.returnTarget.map(t => labelToBlock(t)).map(x => GoTo(Set(x), Some(b.line))).getOrElse(Unreachable())
              (None, goto, ArrayBuffer(), ArrayBuffer(atomicStart))
            case "intrinsic$AtomicEnd" =>
              val atomicEnd = AtomicEnd(Some(b.line))
              val goto =
                b.returnTarget.map(t => labelToBlock(t)).map(x => GoTo(Set(x), Some(b.line))).getOrElse(Unreachable())
              (None, goto, ArrayBuffer(), ArrayBuffer(atomicEnd))
            case _ =>
              val call = Some(DirectCall(nameToProcedure(b.target), Some(b.line)))
              val ft = b.returnTarget.map(t => labelToBlock(t)).map(x => GoTo(Set(x))).getOrElse(Unreachable())
              (call, ft, ArrayBuffer(), None)
          }
        case b: BAPIndirectCall =>
          val call = IndirectCall(translateVar(b.target), Some(b.line))
          val ft = b.returnTarget.map(t => labelToBlock(t)).map(x => GoTo(Set(x))).getOrElse(Unreachable())
          (Some(call), ft, ArrayBuffer(), None)
        case b: BAPGoTo =>
          val target = labelToBlock(b.target)
          b.condition match {
            // condition is true
            case l: BAPLiteral if l.value > BigInt(0) =>
              (None, GoTo(ArrayBuffer(target), Some(b.line)), ArrayBuffer(), None)
            // non-true condition
            case _ =>
              val (expr, load) = translateExpr(b.condition)
              val condition = convertConditionBool(expr, false)
              val newBlock = newBlockCondition(block, target, condition)
              (None, GoTo(ArrayBuffer(newBlock), Some(b.line)), ArrayBuffer(newBlock), load)
          }
      }
    }
  }

  /**
    * Converts an Expr condition that returns a bitvector of size 1 to an Expr condition that returns a Boolean
    *
    * If negative is true then the negation of the condition is returned
    *
    * If the Expr uses a comparator that returns a Boolean then no further conversion is performed except negation,
    * if necessary.
    */
  private def convertConditionBool(e: Expr, negative: Boolean): Expr = {
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
