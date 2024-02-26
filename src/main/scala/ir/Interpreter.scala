package ir
import analysis.BitVectorEval.*
import util.Logger

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class Interpreter() {
  val regs: mutable.Map[Variable, BitVecLiteral] = mutable.Map()
  val mems: mutable.Map[Int, BitVecLiteral] = mutable.Map()
  private val SP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
  private val FP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
  private val LR: BitVecLiteral = BitVecLiteral(BigInt("FF", 16), 64)
  private var nextBlock: Option[Block] = None
  private val returnBlock: mutable.Stack[Block] = mutable.Stack()

  def eval(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): BitVecLiteral = {
    exp match {
      case id: Variable =>
        env.get(id) match {
          case Some(value) =>
            Logger.debug(s"\t${id.name} == 0x${value.value.toString(16)}[u${value.size}]")
            value
          case _ => throw new Exception(s"$id not found in env")
        }

      case n: Literal =>
        n match {
          case bv: BitVecLiteral =>
            Logger.debug(s"\tBitVecLiteral(0x${bv.value.toString(16)}[u${bv.size}])")
            bv
          case _ => ???
        }

      case ze: ZeroExtend =>
        Logger.debug(s"\t$ze")
        smt_zero_extend(ze.extension, eval(ze.body, env))

      case se: SignExtend =>
        Logger.debug(s"\t$se")
        smt_sign_extend(se.extension, eval(se.body, env))

      case e: Extract =>
        Logger.debug(s"\tExtract($e, ${e.start}, ${e.end})")
        boogie_extract(e.end, e.start, eval(e.body, env))

      case r: Repeat =>
        Logger.debug(s"\t$r")
        ??? // TODO

      case bin: BinaryExpr =>
        val left = eval(bin.arg1, env)
        val right = eval(bin.arg2, env)
        Logger.debug(
          s"\tBinaryExpr(0x${left.value.toString(16)}[u${left.size}] ${bin.op} 0x${right.value.toString(16)}[u${right.size}])"
        )
        bin.op match {
          case BVAND    => smt_bvand(left, right)
          case BVOR     => smt_bvor(left, right)
          case BVADD    => smt_bvadd(left, right)
          case BVMUL    => smt_bvmul(left, right)
          case BVUDIV   => smt_bvudiv(left, right)
          case BVUREM   => smt_bvurem(left, right)
          case BVSHL    => smt_bvshl(left, right)
          case BVLSHR   => smt_bvlshr(left, right)
          case BVNAND   => smt_bvnand(left, right)
          case BVNOR    => smt_bvnor(left, right)
          case BVXOR    => smt_bvxor(left, right)
          case BVXNOR   => smt_bvxnor(left, right)
          case BVCOMP   => smt_bvcomp(left, right)
          case BVSUB    => smt_bvsub(left, right)
          case BVSDIV   => smt_bvsdiv(left, right)
          case BVSREM   => smt_bvsrem(left, right)
          case BVSMOD   => smt_bvsmod(left, right)
          case BVASHR   => smt_bvashr(left, right)
          case BVCONCAT => smt_concat(left, right)
          case _ => ???
        }

      case un: UnaryExpr =>
        val arg = eval(un.arg, env)
        Logger.debug(s"\tUnaryExpr($un)")
        un.op match {
          case BVNEG   => smt_bvneg(arg)
          case BVNOT   => smt_bvnot(arg)
        }

      case m: Memory =>
        Logger.debug(s"\t$m")
        ???

      case ml: MemoryLoad =>
        Logger.debug(s"\t$ml")
        val index: Int = eval(ml.index, env).value.toInt
        getMemory(index, ml.size, ml.endian, mems)

      case ms: MemoryStore =>
        val index: Int = eval(ms.index, env).value.toInt
        val value: BitVecLiteral = eval(ms.value, env)
        Logger.debug(s"\tMemoryStore(mem:${ms.mem}, index:0x${index.toHexString}, value:0x${value.value
          .toString(16)}[u${value.size}], size:${ms.size})")
        setMemory(index, ms.size, ms.endian, value, mems)
    }
  }

  def evalBool(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): BoolLit = {
    exp match {
      case n: BoolLit => n
      case bin: BinaryExpr =>
        bin.op match {
          case b: BoolBinOp =>
            val arg1 = evalBool(bin.arg1, env)
            val arg2 = evalBool(bin.arg2, env)
            b match {
              case BoolEQ =>
                if (arg1 == arg2) {
                  TrueLiteral
                } else {
                  FalseLiteral
                }
              case BoolNEQ =>
                if (arg1 != arg2) {
                  TrueLiteral
                } else {
                  FalseLiteral
                }
              case BoolAND =>
                (arg1, arg2) match {
                  case (TrueLiteral, TrueLiteral) => TrueLiteral
                  case _ => FalseLiteral
                }
              case BoolOR =>
                (arg1, arg2) match {
                  case (FalseLiteral, FalseLiteral) => FalseLiteral
                  case _ => TrueLiteral
                }
              case BoolIMPLIES =>
                (arg1, arg2) match {
                  case (TrueLiteral, FalseLiteral) => FalseLiteral
                  case _ => TrueLiteral
                }
              case BoolEQUIV =>
                if (arg1 == arg2) {
                  TrueLiteral
                } else {
                  FalseLiteral
                }
            }
          case b: BVBinOp =>
            val left = eval(bin.arg1, env)
            val right = eval(bin.arg2, env)
            b match {
              case BVULT => smt_bvult(left, right)
              case BVULE => smt_bvule(left, right)
              case BVUGT => smt_bvugt(left, right)
              case BVUGE => smt_bvuge(left, right)
              case BVSLT => smt_bvslt(left, right)
              case BVSLE => smt_bvsle(left, right)
              case BVSGT => smt_bvsgt(left, right)
              case BVSGE => smt_bvsge(left, right)
              case BVEQ => smt_bveq(left, right)
              case BVNEQ => smt_bvneq(left, right)
              case _ => ???
            }
          case _ => ???
        }

      case un: UnaryExpr =>
        un.op match {
          case BoolNOT => if evalBool(un.arg, env) == TrueLiteral then FalseLiteral else TrueLiteral
          case _ => ???
        }
      case _ => ???
    }
  }

  def getMemory(index: Int, size: Int, endian: Endian, env: mutable.Map[Int, BitVecLiteral]): BitVecLiteral = {
    val end = index + size / 8 - 1
    val memoryChunks = (index to end).map(i => env.getOrElse(i, BitVecLiteral(0, 8)))

    val (newValue, newSize) = memoryChunks.foldLeft(("", 0)) { (acc, current) =>
      val currentString: String = current.value.toString(2).reverse.padTo(8, '0').reverse
      endian match {
        case Endian.LittleEndian => (currentString + acc._1, acc._2 + current.size)
        case Endian.BigEndian    => (acc._1 + currentString, acc._2 + current.size)
      }
    }

    BitVecLiteral(BigInt(newValue, 2), newSize)
  }

  def setMemory(
      index: Int,
      size: Int,
      endian: Endian,
      value: BitVecLiteral,
      env: mutable.Map[Int, BitVecLiteral]
  ): BitVecLiteral = {
    val binaryString: String = value.value.toString(2).reverse.padTo(size, '0').reverse

    val data: List[BitVecLiteral] = endian match {
      case Endian.LittleEndian =>
        binaryString.grouped(8).toList.map(chunk => BitVecLiteral(BigInt(chunk, 2), 8)).reverse
      case Endian.BigEndian =>
        binaryString.grouped(8).toList.map(chunk => BitVecLiteral(BigInt(chunk, 2), 8))
    }

    data.zipWithIndex.foreach { case (bv, i) =>
      env(index + i) = bv
    }

    value
  }

  private def interpretProcedure(p: Procedure): Unit = {
    Logger.debug(s"Procedure(${p.name}, ${p.address.getOrElse("None")})")

    // Procedure.in
    for ((in, index) <- p.in.zipWithIndex) {
      Logger.debug(s"\tin[$index]:${in.name} ${in.size} ${in.value}")
    }

    // Procedure.out
    for ((out, index) <- p.out.zipWithIndex) {
      Logger.debug(s"\tout[$index]:${out.name} ${out.size} ${out.value}")
    }

    // Procedure.Block
    p.entryBlock match {
      case Some(block) => nextBlock = Some(block)
      case None        => nextBlock = Some(returnBlock.pop())
    }
  }

  private def interpretBlock(b: Block): Unit = {
    Logger.debug(s"Block:${b.label} ${b.address}")
    // Block.Statement
    for ((statement, index) <- b.statements.zipWithIndex) {
      Logger.debug(s"statement[$index]:")
      interpretStatement(statement)
    }

    // Block.Jump
    breakable {
      Logger.debug(s"jump:")
      b.jump match {
        case gt: GoTo =>
          Logger.debug(s"$gt")
          for (g <- gt.targets) {
            val condition: Option[Expr] = g.statements.headOption.collect { case a: Assume => a.body }
            condition match {
              case Some(e) => evalBool(e, regs) match {
                case TrueLiteral =>
                  nextBlock = Some(g)
                  break
                case _ =>
              }
              case None =>
                nextBlock = Some(g)
                break
            }
          }
        case dc: DirectCall =>
          Logger.debug(s"$dc")
          if (dc.returnTarget.isDefined) {
            returnBlock.push(dc.returnTarget.get)
          }
          interpretProcedure(dc.target)
          break
        case ic: IndirectCall =>
          Logger.debug(s"$ic")
          if (ic.target == Register("R30", BitVecType(64)) && ic.returnTarget.isEmpty) {
            if (returnBlock.nonEmpty) {
              nextBlock = Some(returnBlock.pop())
            } else {
              //Exit Interpreter
              nextBlock = None
            }
            break
          } else {
            ???
          }
      }
    }
  }

  private def interpretStatement(s: Statement): Unit = {
    s match {
      case assign: LocalAssign =>
        Logger.debug(s"LocalAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        Logger.debug(s"LocalAssign ${assign.lhs} := 0x${evalRight.value.toString(16)}[u${evalRight.size}]\n")
        regs += (assign.lhs -> evalRight)

      case assign: MemoryAssign =>
        Logger.debug(s"MemoryAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        evalRight match {
          case BitVecLiteral(value, size) =>
            Logger.debug(s"MemoryAssign ${assign.lhs} := 0x${value.toString(16)}[u$size]\n")
        }
      case _ : NOP =>
      case assert: Assert =>
        Logger.debug(assert)
        // TODO

      case assume: Assume =>
        Logger.debug(assume)
        // TODO, but already taken into effect if it is a branch condition
    }
  }

  def interpret(IRProgram: Program): mutable.Map[Variable, BitVecLiteral] = {
    // initialize memory array from IRProgram
    var currentAddress = 0
    IRProgram.initialMemory
      .sortBy(_.address)
      .foreach { im =>
        if (im.address + im.size > currentAddress) {
          val start = im.address.max(currentAddress)
          val data = if (im.address < currentAddress) im.bytes.slice(currentAddress - im.address, im.size) else im.bytes
          data.zipWithIndex.foreach { (byte, index) =>
            mems(start + index) = byte
          }
          currentAddress = im.address + im.size
        }
      }

    // Initial SP, FP and LR to regs
    regs += (Register("R31", BitVecType(64)) -> SP)
    regs += (Register("R29", BitVecType(64)) -> FP)
    regs += (Register("R30", BitVecType(64)) -> LR)

    // Program.Procedure
    interpretProcedure(IRProgram.mainProcedure)
    while (nextBlock.isDefined) {
      interpretBlock(nextBlock.get)
    }

    regs
  }
}