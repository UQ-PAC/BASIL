package analysis

import analysis.solvers.ForwardIDESolver
import ir.*
import boogie.*

/**
 * SOUNDNESS
 *
 * The GlobalVariable type unsoundly represents global variables, in the case that there is a misaligned memory operation.
 * Consider the following example c program (assuming a long is 8 bytes and an int is 4 bytes).
 * ```c
 * long x;
 * long y;
 *
 * void f(int a) {
 *     *((int*)(&x) + 1) = a;
 * }
 *
 * int main(int argc, char **argv) {
 *     f(y);
 *     return 0;
 * }
 * ```
 * Here, the global variable x should be tainted by the input parameter a in f. With the current implementation however,
 * this isn't the case, since we taint based on base addresses. To solve this we can work on individual bytes of memory,
 * but this would be extremely expensive. It would be ideal to instead taint memory regions acquired from MRA or DSA.
 * For use in summary generation however, this requires a sensible way to express the gamma of a memory region.
 */

/**
 * A value which can propogate taint/be tainted.
 */
type Taintable = Variable | GlobalVariable /*| LocalStackVariable*/ | UnknownMemory

// TODO global and stack variables should just be `Variable`s after an IL transformation, in the future they shouldn't need to be defined here.

/** A global variable in memory.
  */
case class GlobalVariable(mem: Memory, address: BitVecLiteral, size: Int, identifier: String) {
  override def toString(): String = {
    s"GlobalVariable($mem, $identifier, $size, $address)"
  }

  /** The security classification of this global variable as a boogie expression.
    */
  def L: BExpr = {
    val bAddr = BVariable("$" + s"${identifier}_addr", BitVecBType(64), Scope.Const)
    BFunctionCall("L", List(mem.toBoogie, bAddr), BoolBType)
  }

  /** The boogie expression corresponding to the gamma of this global variable.
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

/** Represents a memory address with no known information.
  */
case class UnknownMemory() {
  override def toString(): String = {
    "UnknownMemory"
  }
}

def getMemoryVariable(
  n: CFGPosition,
  mem: Memory,
  expression: Expr,
  size: Int,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  globals: Map[BigInt, String]
): Option[GlobalVariable /*| LocalStackVariable*/ ] = {
  // TODO unsoundly gets stack variable which can lead to unsound analysis

  val stackPointer = Register("R31", 64)

  expression match {
    // TODO assumes stack var accesses are all of the form R31 + n, or just R31, when in reality they could be more complex.
    case BinaryExpr(BVADD, arg1, arg2) if arg1 == stackPointer =>
      evaluateExpression(arg2, constProp(n)) match
        // TODO This assumes that all stack variables are initialized local variables, which is not necessarily the case.
        //      If a stack address is read, without being assigned a value in this procedure, it will be
        //      assumed untainted, when in reality it may be UnknownMemory.
        // case Some(addr) => Some(LocalStackVariable(addr, size))
        case Some(_) => None
        case None => None
    // case v: Variable if v == stackPointer => Some(LocalStackVariable(BitVecLiteral(0, 64), size))
    case v: Variable if v == stackPointer => None
    case _ =>
      // TOOD check that the global access has the right size
      evaluateExpression(expression, constProp(n)) match
        case Some(addr) =>
          globals.get(addr.value) match
            case Some(global) => Some(GlobalVariable(mem, addr, size, global))
            case None => None
        case None => None
  }
}

trait TaintAnalysisFunctions(
  globals: Map[BigInt, String],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  tainted: Map[CFGPosition, Set[Taintable]]
) extends ForwardIDEAnalysis[Taintable, TwoElement, TwoElementLattice] {
  val valuelattice = TwoElementLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Return, aftercall: Command)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: Command)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    (n match {
      case LocalAssign(variable, expression, _) =>
        d match {
          case Left(v: Variable) =>
            if (expression.variables.contains(v)) {
              Map(d -> IdEdge(), Left(variable) -> IdEdge())
            } else if (v == variable) {
              Map()
            } else {
              Map(d -> IdEdge())
            }
          case _ => Map(d -> IdEdge())
        }
      case MemoryStore(mem, index, expression, _, size, _) =>
        val variable: Taintable = getMemoryVariable(n, mem, index, size, constProp, globals).getOrElse(UnknownMemory())
        d match {
          case Left(v: Variable) if expression.variables.contains(v) => Map(d -> IdEdge(), Left(variable) -> IdEdge())
          case Left(v: GlobalVariable) if variable == v => Map()
          case _ => Map(d -> IdEdge())
        }
      case MemoryLoad(lhs, mem, index, _, size, _) =>
        val memoryVariable: Taintable =
          getMemoryVariable(n, mem, index, size, constProp, globals).getOrElse(UnknownMemory())
        d match {
          case Left(v: Variable) if index.variables.contains(v) => Map(d -> IdEdge(), Left(lhs) -> IdEdge())
          case Left(v: Variable) if v == lhs => Map()
          case Left(v: Taintable) if memoryVariable == v => Map(d -> IdEdge(), Left(lhs) -> IdEdge())
          case _ => Map(d -> IdEdge())
        }
      case _ => Map(d -> IdEdge())
    }) ++ (
      d match
        case Left(_) => Map()
        case Right(_) =>
          tainted.getOrElse(n, Set()).foldLeft(Map[DL, EdgeFunction[TwoElement]]()) { (m, t) =>
            m + (Left(t) -> ConstEdge(valuelattice.top))
          }
    )
  }
}

/** Performs taint analysis on a program. Variables (`Taintable`s) are marked as tainted at points in the program as
  * specified by `tainted`, and propogate their taint throughout the program. Assignments containing tainted variables
  * mark the assigned value as tainted.
  */
class TaintAnalysis(
  program: Program,
  globals: Map[BigInt, String],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  tainted: Map[CFGPosition, Set[Taintable]]
) extends ForwardIDESolver[Taintable, TwoElement, TwoElementLattice](program),
      TaintAnalysisFunctions(globals, constProp, tainted)
