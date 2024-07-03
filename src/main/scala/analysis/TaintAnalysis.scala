package analysis

import analysis.solvers.ForwardIDESolver
import ir.*

type Taintable = Variable | GlobalVariable | StackVariable

class GlobalVariable(val address: BitVecLiteral, val identifier: String) {
  override def toString(): String = {
    s"GlobalVariable($identifier, $address)"
  }
}

class StackVariable(val address: BitVecLiteral) {
  override def toString(): String = {
    s"StackVariable($address)"
  }
  override def equals(that: Any): Boolean = {
    that match
      case that: StackVariable => address == that.address
      case _ => false
  }
}

trait TaintAnalysisFunctions(
  globals: Map[BigInt, String],
  globalOffsets: Map[BigInt, BigInt],
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

    def getMemoryVariable(expression: Expr): Option[GlobalVariable | StackVariable] = {
      expression match
        case BinaryExpr(BVADD, arg1, arg2) if arg1 == stackPointer => {
          evaluateExpression(arg2, constProp(n)) match
            case Some(addr) => Some(StackVariable(addr))
            case None => None
        }
        case _ => {
          evaluateExpression(expression, constProp(n)) match
            case Some(addr) => Some(GlobalVariable(addr, globals(addr.value)))
            case None => None
        }
    }

    def containsValue(expression: Expr, value: Taintable): Boolean = {
      value match {
        case (v: Variable) => expression.variables.contains(v)
        case v => {
          expression.loads.map {
            load => {
              getMemoryVariable(load.index)
            }
          }.flatten.contains(v)
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
        getMemoryVariable(index) match {
          case Some(variable) => {
            d match {
              case Left(v) => {
                if containsValue(expression, v) then Map(d -> IdEdge(), Left(variable) -> IdEdge())
                else if variable == v then Map()
                else Map(d -> IdEdge())
              }
              case Right(_) => Map(d -> IdEdge())
            }
          }
          case None => Map(d -> IdEdge())
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
  globalOffsets: Map[BigInt, BigInt],
  mmm: MemoryModelMap,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  tainted: Map[CFGPosition, Set[Taintable]],
) extends ForwardIDESolver[Taintable, TwoElement, TwoElementLattice](program),
    TaintAnalysisFunctions(globals, globalOffsets, mmm, constProp, tainted)
