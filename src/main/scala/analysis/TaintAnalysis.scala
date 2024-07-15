package analysis

import analysis.solvers.ForwardIDESolver
import ir.*
import boogie.*
import util.Logger

/**
 * A value which can propogate taint/be tainted.
 */
type Taintable = Variable | GlobalVariable /*| LocalStackVariable*/ | UnknownMemory

// TODO global and stack variables should just be `Variable`s after an IL transformation, in the future they shouldn't need to be defined here.

/**
 * A global variable in memory.
 */
case class GlobalVariable(val mem: Memory, val address: BitVecLiteral, val size: Int, val identifier: String) {
  override def toString(): String = {
    s"GlobalVariable($mem, $identifier, $size, $address)"
  }

  /**
   * The security classification of this global variable as a boogie expression.
   */
  def L: BExpr = {
    val bAddr = BVariable("$" + s"${identifier}_addr", BitVecBType(64), Scope.Const)
    BFunctionCall("L", List(mem.toBoogie, bAddr), BoolBType)
  }

  /**
   * The boogie expression corresponding to the gamma of this global variable.
   */
  def toGamma: BExpr = {
    val bAddr = BVariable("$" + s"${identifier}_addr", BitVecBType(64), Scope.Const)
    GammaLoad(mem.toGamma, bAddr, size, size / mem.valueSize)
  }
}

// TODO this is does not account for different stack pointer indices. We could have two `LocalStackVariable`s from
// different functions with the same offset, and they would compare to be equal, which leads to unsound analysis! :(
/*
/**
 * A variable stored on the stack that was initialized in its own procedure (i.e. not a function argument).
 */
case class LocalStackVariable(val address: BitVecLiteral, val size: Int) {
  override def toString(): String = {
    s"StackVariable($address, $size)"
  }
}*/

/**
 * Represents a memory address with no known information.
 */
case class UnknownMemory() {
  override def toString(): String = {
    "UnknownMemory"
  }
}

def getMemoryVariable(
  n: CFGPosition, mem: Memory, expression: Expr, size: Int,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  globals: Map[BigInt, String],
): Option[GlobalVariable/*| LocalStackVariable*/] = {
  // TODO unsoundly gets stack variable which can lead to unsound analysis

  val stackPointer = Register("R31", 64)

  expression match {
    // TODO assumes stack var accesses are all of the form R31 + n, or just R31, when in reality they could be more complex.
    case BinaryExpr(BVADD, arg1, arg2) if arg1 == stackPointer => {
      evaluateExpression(arg2, constProp(n)) match
        // TODO This assumes that all stack variables are initialized local variables, which is not necessarily the case.
        //      If a stack address is read, without being assigned a value in this procedure, it will be
        //      assumed untainted, when in reality it may be UnknownMemory.
        //case Some(addr) => Some(LocalStackVariable(addr, size))
        case Some(addr) => None
        case None => None
    }
    //case v: Variable if v == stackPointer => Some(LocalStackVariable(BitVecLiteral(0, 64), size))
    case v: Variable if v == stackPointer => None
    case _ => {
      // TOOD check that the global access has the right size
      evaluateExpression(expression, constProp(n)) match
        case Some(addr) => globals.get(addr.value) match
          case Some(global) => Some(GlobalVariable(mem, addr, size, global))
          case None => None
        case None => None
    }
  }
}

trait TaintAnalysisFunctions(
  globals: Map[BigInt, String],
  mmm: MemoryModelMap,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  tainted: Map[CFGPosition, Set[Taintable]],
) extends ForwardIDEAnalysis[Taintable, TwoElement, TwoElementLattice] {
  val valuelattice = TwoElementLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  private val stackPointer = Register("R31", 64)

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: IndirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    def containsValue(expression: Expr, value: Taintable): Boolean = {
      value match {
        case (v: Variable) => expression.variables.contains(v)
        case v => {
          expression.loads.map {
            load => getMemoryVariable(n, load.mem, load.index, load.size, constProp, globals).getOrElse(UnknownMemory())
          }.contains(v)
        }
      }
    }

    (n match {
      case Assign(variable, expression, _) => {
        d match {
          case Left(v) => {
            if containsValue(expression, v) then Map(d -> IdEdge(), Left(variable) -> IdEdge())
            else if v == variable then Map()
            else Map(d -> IdEdge())
          }
          case _ => Map(d -> IdEdge())
        }
      }
      case MemoryAssign(mem, index, expression, _, size, _) => {
        val variable = getMemoryVariable(n, mem, index, size, constProp, globals).getOrElse(UnknownMemory())
        d match {
          case Left(v) => {
            if containsValue(expression, v) then Map(d -> IdEdge(), Left(variable) -> IdEdge())
            else if variable == v && v != UnknownMemory() then Map()
            else Map(d -> IdEdge())
          }
          case Right(_) => Map(d -> IdEdge())
        }
      }
      case _ => Map(d -> IdEdge())
    }) ++ (
      d match
        case Left(_) => Map()
        case Right(_) => tainted.getOrElse(n, Set()).foldLeft(Map[DL, EdgeFunction[TwoElement]]()) {
          (m, t) => m + (Left(t) -> ConstEdge(valuelattice.top))
        }
    )
  }
}

/**
 * Performs taint analysis on a program. Variables (`Taintable`s) are marked as tainted at points in the program as
 * specified by `tainted`, and propogate their taint throughout the program. Assignments containing tainted variables
 * mark the assigned value as tainted.
 */
class TaintAnalysis(
  program: Program,
  globals: Map[BigInt, String],
  mmm: MemoryModelMap,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  tainted: Map[CFGPosition, Set[Taintable]],
) extends ForwardIDESolver[Taintable, TwoElement, TwoElementLattice](program),
    TaintAnalysisFunctions(globals, mmm, constProp, tainted)
