package ir
import ir.eval.BitVectorEval.*
import util.Logger

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

// enum Asssumption:
//   case Assume(x: Expr)
//   case Jump(choice: Block)
//   case Not(a: Assumption)

sealed trait ExecutionState
case class FailedAssertion(a: Assert) extends ExecutionState
case class Stopped() extends ExecutionState
case class Run(val next: Command) extends ExecutionState
case class EscapedControlFlow(val call: IndirectCall) extends ExecutionState
case class Errored(val message: String = "") extends ExecutionState

// case class Execution() extends State {
//   // stack of assumptions
//   // var assumptions: mutable.Stack[] = mutable.Stack()
//   var memory: mutable.Map[Memory, Map[BigInt, BigInt]] = mutable.Map()
//   var bvValues: mutable.Map[Variable, BigInt] = mutable.Map()
//   var intValues: mutable.Map[Variable, BigInt] = mutable.Map()
// 
//   var nextCmd: ExecutionState = Stopped()
//   var callStack: mutable.Stack[Command] = mutable.Stack()
// }


case class InterpreterError(condinue: ExecutionState) extends Exception()

class Interpreter() {
  val regs: mutable.Map[Variable, BitVecLiteral] = mutable.Map()
  val mems: mutable.Map[Int, BitVecLiteral] = mutable.Map()
  private val SP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
  private val FP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
  private val LR: BitVecLiteral = BitVecLiteral(BigInt("FF", 16), 64)
  var nextCmd: ExecutionState = Stopped()
  private val callStack: mutable.Stack[Command] = mutable.Stack()

  def eval(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): BitVecLiteral = {
    def load(m: Memory, index: Expr, endian: Endian, size: Int) = {
      val idx = eval(index, env).value.toInt
      Some(getMemory(idx, size, endian, mems))
    }

    ir.eval.evalBVExpr(exp, x => env.get(x), load) match {
      case Right(b) => b
      case Left(e)  => throw InterpreterError(Errored(s"Failed to evaluate bv expr: residual $exp"))
    }
  }

  def doReturn() = {
    if (callStack.nonEmpty) {
      nextCmd = Run(callStack.pop())
    } else {
      nextCmd = Stopped()
    }
  }

  def evalBool(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): Boolean = {
    ir.eval.evalLogExpr(exp, x => env.get(x)) match {
      case Right(b) => b
      case Left(e)  => throw InterpreterError(Errored(s"Failed to evaluate logical expr: residual $e"))
    }
  }

  def evalInt(exp: Expr, env: mutable.Map[Variable, BitVecLiteral]): BigInt = {
    ir.eval.evalIntExpr(exp, x => env.get(x)) match {
      case Right(b) => b
      case Left(e)  => throw InterpreterError(Errored(s"Failed to evaluate int expr: residual $e"))
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
    Logger.debug(s"Regs $regs")

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
      case Some(block) => nextCmd = Run(block.statements.headOption.getOrElse(block.jump))
      case None        => doReturn()
    }
  }

  private def interpretJump(j: Jump): Unit = {
    Logger.debug(s"jump: $j")
    breakable {
      j match {
        case gt: GoTo =>
          for (g <- gt.targets) {
            val condition: Option[Expr] = g.statements.headOption.collect { case a: Assume => a.body }
            condition match {
              case Some(e) =>
                if (evalBool(e, regs)) {
                  Logger.debug(s"chosen ${g.label}")
                  nextCmd = Run(g.statements.headOption.getOrElse(g.jump))
                  break
                }
              case None =>
                nextCmd = Run(g.statements.headOption.getOrElse(g.jump))
                break
            }
          }
        case r: Return => doReturn()
        case h: Unreachable => {
          Logger.debug("Unreachable")
          nextCmd = Stopped()
        }
      }
    }
  }

  private def interpretStatement(s: Statement): Unit = {
    Logger.debug(s"Regs $regs")
    Logger.debug(s"statement[$s]:")

    nextCmd = Run(s.successor)

    s match {
      case assign: Assign =>
        //Logger.debug(s"LocalAssign ${assign.lhs} = ${assign.rhs}")
        val evalRight = eval(assign.rhs, regs)
        //Logger.debug(s"LocalAssign ${assign.lhs} := 0x${evalRight.value.toString(16)}[u${evalRight.size}]\n")
        regs += (assign.lhs -> evalRight)

      case assign: MemoryAssign =>
        //Logger.debug(s"MemoryAssign ${assign.mem}[${assign.index}] = ${assign.value}")

        val index: Int = eval(assign.index, regs).value.toInt
        val value: BitVecLiteral = eval(assign.value, regs)
        //Logger.debug(s"\tMemoryStore(mem:${assign.mem}, index:0x${index.toHexString}, value:0x${value.value
          // .toString(16)}[u${value.size}], size:${assign.size})")

        val evalStore = setMemory(index, assign.size, assign.endian, value, mems)
        evalStore match {
          case BitVecLiteral(value, size) =>
            //Logger.debug(s"MemoryAssign ${assign.mem} := 0x${value.toString(16)}[u$size]\n")
        }
      case assert: Assert =>
        // Logger.debug(assert)
        if (!evalBool(assert.body, regs)) {
          nextCmd = FailedAssertion(assert)
        }
      case assume: Assume =>
        // TODO, but already taken into effect if it is a branch condition
        // Logger.debug(assume)
        if (!evalBool(assume.body, regs)) {
          nextCmd = Errored(s"Assumption not satisfied: $assume")
        }
      case dc: DirectCall =>
        // Logger.debug(s"$dc")
        callStack.push(dc.successor)
        interpretProcedure(dc.target)
      case ic: IndirectCall =>
        // Logger.debug(s"$ic")
        if (ic.target == Register("R30", 64)) {
          doReturn()
          //Exit Interpreter
        } else {
          nextCmd = EscapedControlFlow(ic)
        }
      case _: NOP => ()
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
    while (
      try {nextCmd match {
          case Run(c: Statement) => {
            interpretStatement(c)
            true
          }
          case Run(c: Jump)      => {
            interpretJump(c)
            true
          }
          case Stopped() => {
            false
          }
          case errorstop => {
            Logger.error(s"Interpreter $errorstop")
            false
          } 
        }
    } catch {
      case InterpreterError(e) => {
        nextCmd = e 
        true
      }
    }

    ) {}

    regs
  }
}
