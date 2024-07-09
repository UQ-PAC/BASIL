package analysis

import analysis.solvers.ForwardIDESolver
import ir.*
import boogie.*
import util.Logger

type Taintable = Variable | GlobalVariable | LocalStackVariable | UnknownMemory

case class GlobalVariable(val mem: Memory, val address: BitVecLiteral, val size: Int, val identifier: String) {
  override def toString(): String = {
    s"GlobalVariable($mem, $identifier, $size, $address)"
  }

  def L: BExpr = {
    val bAddr = BVariable("$" + s"${identifier}_addr", BitVecBType(64), Scope.Const)
    BFunctionCall("L", List(mem.toBoogie, bAddr), BoolBType)
  }

  def toGamma: BExpr = {
    val bAddr = BVariable("$" + s"${identifier}_addr", BitVecBType(64), Scope.Const)
    GammaLoad(mem.toGamma, bAddr, size, size / mem.valueSize)
  }
}

case class LocalStackVariable(val address: BitVecLiteral, val size: Int) {
  override def toString(): String = {
    s"StackVariable($address, $size)"
  }
}

case class UnknownMemory() {
  override def toString(): String = {
    "UnknownMemory"
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

  val stackPointer = Register("R31", 64)

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

    def getMemoryVariable(mem: Memory, expression: Expr, size: Int): GlobalVariable | LocalStackVariable | UnknownMemory = {
      // TODO use the mmm to check memory regions
      expression match
        // TODO this is (overly) assuming that stack variable accesses come from addition, and always use R31
        case BinaryExpr(BVADD, arg1, arg2) if arg1 == stackPointer => {
          evaluateExpression(arg2, constProp(n)) match
            // TODO This assumes that all stack variables are initialized local variables, which is not necessarily the case.
            //      If a stack address is read, without being assigned a value in this procedure, it will be
            //      assumed untainted, when in reality it may be UnknownMemory.
            case Some(addr) => LocalStackVariable(addr, size)
            case None => UnknownMemory()
        }
        case _ => {
          evaluateExpression(expression, constProp(n)) match
            case Some(addr) => globals.get(addr.value) match
              case Some(global) => GlobalVariable(mem, addr, size, global)
              case None => UnknownMemory()
            case None => UnknownMemory()
        }
    }

    def containsValue(expression: Expr, value: Taintable): Boolean = {
      value match {
        case (v: Variable) => expression.variables.contains(v)
        case v => {
          expression.loads.map {
            load => {
              getMemoryVariable(load.mem, load.index, load.size)
            }
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
        val variable = getMemoryVariable(mem, index, size)
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
    }) ++ {
      d match
        case Left(_) => Map()
        case Right(_) => tainted.getOrElse(n, Set()).foldLeft(Map[DL, EdgeFunction[TwoElement]]()) {
          (m, t) => m + (Left(t) -> ConstEdge(valuelattice.top))
        }
    }
  }
}

class TaintAnalysis(
  program: Program,
  globals: Map[BigInt, String],
  mmm: MemoryModelMap,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  tainted: Map[CFGPosition, Set[Taintable]],
) extends ForwardIDESolver[Taintable, TwoElement, TwoElementLattice](program),
    TaintAnalysisFunctions(globals, mmm, constProp, tainted)
