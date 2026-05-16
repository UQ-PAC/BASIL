package analysis

import analysis.solvers.BackwardIDESolver
import ir.*

class StronglyLiveBitsEdgeFunctionLattice(valuelattice: StronglyLiveBitsLattice)
  extends EdgeFunctionLattice[Map[Int, TwoElement], StronglyLiveBitsLattice](valuelattice) {
  case class ShiftThenJoinEdge(values: Set[Int], constant: Map[Int, TwoElement])
    extends EdgeFunction[Map[Int, TwoElement]] {
    def apply(x: Map[Int, TwoElement]): Map[Int, TwoElement] =
      valuelattice.lub(valuelattice.shiftMap(values, x), constant)

    /** Composes this function with the given one. The resulting function first applies `e` then this function.
     */
    def composeWith(e: EdgeFunction[Map[Int, TwoElement]]): EdgeFunction[Map[Int, TwoElement]] =
      e match {
        case IdEdge() => this
        case ConstEdge(c) => ConstEdge(this.apply(c))
        case ShiftThenJoinEdge(b, d) => ShiftThenJoinEdge(valuelattice.cartesianAdd(values, b),
          valuelattice.lub(valuelattice.shiftMap(values, d), constant))
        case _ => ??? // Other edges will not be used
      }

    /** Finds the least upper bound of this function and the given one.
     */
    def joinWith(e: EdgeFunction[Map[Int, TwoElement]]): EdgeFunction[Map[Int, TwoElement]] =
      e match {
        case IdEdge() => ShiftThenJoinEdge(values + 0, constant)
        case ConstEdge(c) => ShiftThenJoinEdge(values, valuelattice.lub(constant, c))
        case ShiftThenJoinEdge(b, d) => ShiftThenJoinEdge(values ++ b, valuelattice.lub(constant, d))
        case _ => ??? // Unimplemented as other edges will not be used
      }

    override def toString(): String = s"ShiftThenJoinEdge($values, $constant)"
  }
}

trait StronglyLiveBitsAnalysisFunctions
  extends BackwardIDEAnalysis[Variable, Map[Int, TwoElement], StronglyLiveBitsLattice] {
  override val valuelattice: StronglyLiveBitsLattice = StronglyLiveBitsLattice()

  override val edgelattice: StronglyLiveBitsEdgeFunctionLattice =
    StronglyLiveBitsEdgeFunctionLattice(valuelattice)

  val subBot = valuelattice.sublattice.bottom
  val subTop = valuelattice.sublattice.top

  import edgelattice.{IdEdge, ConstEdge, ShiftThenJoinEdge}

  def edgesCallToAfterCall(call: Command, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[Map[Int, TwoElement]]] = ???

  def edgesCallToEntry(call: Command, entry: Return)(d: DL): Map[DL, EdgeFunction[Map[Int, TwoElement]]] = ???

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[Map[Int, TwoElement]]] =
    Map(d -> IdEdge())

  def mapToTop(body: Expr): Map[Variable, EdgeFunction[Map[Int, TwoElement]]] = {
    body.variables.foldLeft(Map(): Map[Variable, EdgeFunction[Map[Int, TwoElement]]])
      ((x, y) => x + (y -> ConstEdge(valuelattice.top))) // TODO: Make this more precise
  }

  // Helps with assignment operation
  def assignHelper(expr: Expr): Map[Variable, EdgeFunction[Map[Int, TwoElement]]] = {
    expr match {
      case literal: Literal => Map()
      case Extract(end, start, body) => body match {
        case literal: Literal => Map()
        case variable: Variable => Map(variable -> ShiftThenJoinEdge(Set(start), Map().withDefaultValue(subBot)))
        case expr: Expr =>
          assignHelper(expr).map({
            case (x, edgeFunc) => x -> edgeFunc.composeWith(ShiftThenJoinEdge(Set(start), Map().withDefaultValue(subBot)))
          })
      }
      case Repeat(repeats, body) => mapToTop(body)
      case ZeroExtend(extension, body) => assignHelper(body)
      case SignExtend(extension, body) => ???
      case UnaryExpr(op, arg) => arg match {
        case variable: Variable => Map(variable->IdEdge())
        case literal: Literal => Map()
        case expr: Expr => assignHelper(expr)
      }
      case BinaryExpr(op, arg1, arg2) => expr.getType match {
        case BitVecType(size) => op match {
          case op: BVBinOp => op match {
            case op: BVCmpOp => ???
            case BVAND => (arg1, arg2) match {
              case (var1: Variable, var2: Variable) => Map(var1 -> IdEdge(), var2 -> IdEdge())
              case (var1: Variable, var2: Literal) => Map(var1 -> IdEdge()) // TODO: More precise?
              case (var1: Literal, var2: Variable) => Map(var2 -> IdEdge())
              case (var1: Literal, var2: Literal) => Map()
              case _ => mapToTop(arg1) ++ mapToTop(arg2)
            }
            case BVOR => ???
            case BVADD => ???
            case BVSHL => ???
            case BVLSHR => ???
            case BVCONCAT => ???
            case BVMUL => ???
            case BVUDIV => ???
            case BVUREM => ???
            case BVNAND => ???
            case BVNOR => ???
            case BVXOR => ???
            case BVXNOR => ???
            case BVCOMP => ???
            case BVSUB => ???
            case BVSDIV => ???
            case BVSREM => ???
            case BVSMOD => ???
            case BVASHR => ???
          }
          case _ => Map()
        }
        case _ => Map()
      }
      case variable: Variable => Map(variable -> IdEdge())
      case _ => mapToTop(expr)
    }
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[Map[Int, TwoElement]]] =
    n match {
      case LocalAssign(variable, expr, _) =>
        d match {
          case Left(value) => value.getType match {
            case BitVecType(_) => assignHelper(expr).foldLeft(Map[DL, EdgeFunction[Map[Int, TwoElement]]]())
              ((x, y) => x + (y match {
              case (a, b) => Left(a) -> b
            }))
            case _ => Map() // Non-BitVector type does not have live bits
          }
          case Right(value) => Map().withDefaultValue(edgelattice.bottom)
        }
      case MemoryAssign(variable, expr, _) =>
        d match {
          case Left(value) => value.getType match {
            case BitVecType(_) => assignHelper(expr).foldLeft(Map[DL, EdgeFunction[Map[Int, TwoElement]]]())
              ((x, y) => x + (y match {
                case (a, b) => Left(a) -> b
              }))
            case _ => Map() // Non-BitVector type does not have live bits
          }
          case Right(value) => Map().withDefaultValue(edgelattice.bottom)
        }
      case MemoryLoad(variable, _, index, _, _, _) =>
        d match {
          case Left(value) => value.getType match {
            case BitVecType(_) => assignHelper(index).foldLeft(Map[DL, EdgeFunction[Map[Int, TwoElement]]]())
              ((x, y) => x + (y match {
                case (a, b) => Left(a) -> b
              }))
            case _ => Map() // Non-BitVector type does not have live bits
          }
          case Right(value) => Map().withDefaultValue(edgelattice.bottom)
        }
      case MemoryStore(_, index, value, _, _, _) => ???
      case Assert(expr, _, _) => ???
      case Assume(expr, _, _, _) => ???
      case Return(_, varExpr) => varExpr.foldLeft(Map[DL, EdgeFunction[Map[Int, TwoElement]]]())
        ((x, y) => y match {
          case (_, retExpr) => x ++ mapToTop(retExpr).foldLeft(Map[DL, EdgeFunction[Map[Int, TwoElement]]]())(
          (a, b) => b match {
            case (var1, eFunc) => a + (Left[Variable, Nothing](var1) -> eFunc)
          }
          )
        }
        // TODO: Make this more precise
        
      )
      case _ => Map(d -> IdEdge()).withDefaultValue(edgelattice.bottom)
    }
}

class StronglyLiveBitsAnalysis(program: Program) extends
  BackwardIDESolver[Variable, Map[Int, TwoElement], StronglyLiveBitsLattice](program),
  StronglyLiveBitsAnalysisFunctions