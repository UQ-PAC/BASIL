package ir
import ir.eval.BitVectorEval.*
import util.Logger

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}


// enum Asssumption:
//   case Assume(x: Expr)
//   case Jump(choice: Block)
//   case Not(a: Assumption)
// 
// 
// class State() {
//   // stack of assumptions
//   var assumptions: mutable.Stack[Assumption] = mutable.Stack()
//   var memory: mutable.Map[Memory, Map[BigInt, BigInt]] = Map()
//   var bvValues: mutable.Map[Variable, BigInt] = Map()
//   var intValues: mutable.Map[Variable, BigInt] = Map()
// 
//   var nextCmd: Option[Command] = None
//   var returnCmd: mutable.Stack[Command] = mutable.Stack()
// }


class Interpreter() {
  val regs: mutable.Map[Variable, BitVecLiteral] = mutable.Map()
  val mems: mutable.Map[Int, BitVecLiteral] = mutable.Map()
  private val SP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
  private val FP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
  private val LR: BitVecLiteral = BitVecLiteral(BigInt("FF", 16), 64)
  private var nextCmd: Option[Command] = None
  private val returnCmd: mutable.Stack[Command] = mutable.Stack()

  def eval(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): BitVecLiteral = {
    def load(m: Memory, index: Expr, endian: Endian, size: Int) = {
      val idx = evalInt(index, env).toInt
      Some(getMemory(idx, size, endian, mems))
    }

    ir.eval.evalBVExpr(exp, x => env.get(x), load) match {
      case Right(b) => b
      case Left(e) => throw Exception(s"Failed to evaluate expr: residual $exp")
    }
  }

  def evalBool(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): Boolean = {
    ir.eval.evalLogExpr(exp, x => env.get(x)) match {
      case Right(b) => b
      case Left(e) => throw Exception(s"Failed to evaluate expr: residual $e")
    }
  }

  def evalInt(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): BigInt = {
    ir.eval.evalIntExpr(exp, x => env.get(x)) match {
      case Right(b) => b
      case Left(e) => throw Exception(s"Failed to evaluate expr: residual $e")
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
      case Some(block) => nextCmd = Some(block.statements.headOption.getOrElse(block.jump))
      case None        => nextCmd = Some(returnCmd.pop())
    }
  }

  private def interpretJump(j: Jump) : Unit = {
    Logger.debug(s"jump:")
    breakable {
      j match {
        case gt: GoTo =>
          Logger.debug(s"$gt")
          for (g <- gt.targets) {
            val condition: Option[Expr] = g.statements.headOption.collect { case a: Assume => a.body }
            condition match {
              case Some(e) => if (evalBool(e, regs)) {
                  nextCmd = Some(g.statements.headOption.getOrElse(g.jump))
                  break
              }
              case None =>
                nextCmd = Some(g.statements.headOption.getOrElse(g.jump))
                break
            }
          }
        case r: Return => {
          nextCmd = Some(returnCmd.pop())
        }
        case h: Unreachable => {
          Logger.debug("Unreachable")
          nextCmd = None
        }
      }
    }
  }

  private def interpretStatement(s: Statement): Unit = {
    Logger.debug(s"statement[$s]:")
    s match {
      case assign: Assign =>
        Logger.debug(s"LocalAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        Logger.debug(s"LocalAssign ${assign.lhs} := 0x${evalRight.value.toString(16)}[u${evalRight.size}]\n")
        regs += (assign.lhs -> evalRight)

      case assign: MemoryAssign =>
        Logger.debug(s"MemoryAssign ${assign.mem}[${assign.index}] = ${assign.value}")

        val index: Int = eval(assign.index, regs).value.toInt
        val value: BitVecLiteral = eval(assign.value, regs)
        Logger.debug(s"\tMemoryStore(mem:${assign.mem}, index:0x${index.toHexString}, value:0x${
          value.value
            .toString(16)
        }[u${value.size}], size:${assign.size})")

        val evalStore = setMemory(index, assign.size, assign.endian, value, mems)
        evalStore match {
          case BitVecLiteral(value, size) =>
            Logger.debug(s"MemoryAssign ${assign.mem} := 0x${value.toString(16)}[u$size]\n")
        }
      case _ : NOP => ()
      case assert: Assert =>
        // TODO
        Logger.debug(assert)
        if (!evalBool(assert.body, regs)) {
          throw Exception(s"Assertion failed ${assert}")
        }
      case assume: Assume =>
        // TODO, but already taken into effect if it is a branch condition
        Logger.debug(assume)
        if (!evalBool(assume.body, regs)) {
            nextCmd = None
            Logger.debug(s"Assumption not satisfied: $assume")
        }
      case dc: DirectCall =>
        Logger.debug(s"$dc")
        returnCmd.push(dc.successor)
        interpretProcedure(dc.target)
        break
      case ic: IndirectCall =>
        Logger.debug(s"$ic")
        if (ic.target == Register("R30", 64)) {
          if (returnCmd.nonEmpty) {
            nextCmd = Some(returnCmd.pop())
          } else {
            //Exit Interpreter
            nextCmd = None
          }
          break
        } else {
          ???
        }
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
    regs += (Register("R31", 64) -> SP)
    regs += (Register("R29", 64) -> FP)
    regs += (Register("R30", 64) -> LR)

    // Program.Procedure
    interpretProcedure(IRProgram.mainProcedure)
    while (nextCmd.isDefined) {
      nextCmd.get match  {
        case c: Statement => interpretStatement(c) 
        case c: Jump => interpretJump(c)
      }
    }

    regs
  }
}
