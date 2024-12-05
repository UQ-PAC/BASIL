package analysis.data_structure_analysis

import analysis.data_structure_analysis.SymBase.{Global, Heap, Par, Stack, Unknown}
import analysis.solvers.SimplePushDownWorklistFixpointSolver
import analysis.{Analysis, FlatEl, FlatElement, IRIntraproceduralForwardDependencies, MapLattice, SetLatticeWithTop, evaluateExpression}
import ir.{BinaryExpr, BitVecLiteral, CFGPosition, DirectCall, Expr, IntraProcIRCursor, LocalAssign, MemoryLoad, Procedure, Program, Register, Variable, computeDomain}


object Counter {
  private var unknownCounter: Int = 0
  private var heapCounter: Int = 0


  def getHeapCounter: Int = {
    heapCounter = heapCounter + 1
    heapCounter
  }

  def getUnknownCounter: Int = {
    unknownCounter = unknownCounter + 1
    unknownCounter
  }

  def reset() : Unit = {
    unknownCounter = 0
    heapCounter = 0
  }

}

enum SymBase:
  case Heap(id: Int = Counter.getHeapCounter) extends SymBase
  case Unknown(id: Int = Counter.getUnknownCounter) extends SymBase
  case Stack(name: String) extends SymBase
  case Par(name: String) extends SymBase
  case Ret extends SymBase
  case Global extends SymBase


abstract class SV(program: Program,  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends
  Analysis[Map[CFGPosition, Map[Variable, Map[SymBase, FlatElement[Set[BitVecLiteral]]]]]] {

  Counter.reset()

  private val stackPointer: Register = Register("R31", 64)
  private val mallocRegister: Register = Register("R0", 64)

  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, program.procedures).toSet

  val offsetSetLattice: SetLatticeWithTop[BitVecLiteral] = SetLatticeWithTop[BitVecLiteral]()
  val symValMapLattice: MapLattice[SymBase, FlatElement[Set[BitVecLiteral]], offsetSetLattice.type] =
    MapLattice[SymBase, FlatElement[Set[BitVecLiteral]], offsetSetLattice.type](offsetSetLattice)
  val lattice: MapLattice[CFGPosition, Map[Variable, Map[SymBase, FlatElement[Set[BitVecLiteral]]]],
    MapLattice[Variable, Map[SymBase, FlatElement[Set[BitVecLiteral]]],
      symValMapLattice.type]] = MapLattice(MapLattice(symValMapLattice))


  private def initDef(base: SymBase, value: BitVecLiteral = BitVecLiteral(0, 64)): Map[SymBase, FlatElement[Set[BitVecLiteral]]] = {
    Map(base -> FlatEl(Set(value))).withDefaultValue(offsetSetLattice.bottom)
  }

  private def exprToSymValMap(pos: CFGPosition, expr: Expr, svs: Map[Variable, Map[SymBase, FlatElement[Set[BitVecLiteral]]]]): Map[SymBase, FlatElement[Set[BitVecLiteral]]] = {

    if evaluateExpression(expr, constProp(pos)).nonEmpty then
      val const = evaluateExpression(expr, constProp(pos)).get
      initDef(Global, const)
    else
      expr match
        case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(pos)).nonEmpty =>
          val test = svs(arg1)
          svs(arg1).map {
            case (base: SymBase, set: FlatEl[Set[BitVecLiteral]]) =>
              val newSet = set.el.map {
                case el =>
                  val binOp = BinaryExpr(op,el, arg2)
                  evaluateExpression(binOp, constProp(pos)).get
                case p => p
              }
              base -> FlatEl(newSet)
            case top => top
          }.withDefaultValue(offsetSetLattice.bottom)
        case variable: Variable => svs(variable)
        case _ =>
          val symSet = expr.variables.foldLeft(Set[SymBase]()) {
            (s, v) =>
              s ++ svs(v).keys
          }

           assert(!symSet.contains(Global))
          symSet.foldLeft(Map[SymBase, FlatElement[Set[BitVecLiteral]]]()) {
            (m, base) =>
              m + (base -> offsetSetLattice.top)
          }.withDefaultValue(offsetSetLattice.bottom)
  }


  def transfer(n: CFGPosition, s: Map[Variable, Map[SymBase, FlatElement[Set[BitVecLiteral]]]]): Map[Variable, Map[SymBase, FlatElement[Set[BitVecLiteral]]]] = {

    n match
      case procedure: Procedure => // entry
        (s + (stackPointer -> initDef(Stack(procedure.name)))) ++ procedure.in.foldLeft(Map[Variable, Map[SymBase, FlatElement[Set[BitVecLiteral]]]]()) {
          (m, param) =>

           m + (param.value -> initDef(Par(param.name)))
        }
      case pos @ LocalAssign(lhs: Variable, rhs, _) => s + (lhs -> exprToSymValMap(pos, rhs, s)) // local
      case MemoryLoad(lhs, _, index, _, _, _) => s + (lhs -> initDef(Unknown())) // load
      case DirectCall(target, _) if target.name == "malloc" => s + (mallocRegister -> initDef(Heap()))
      case _ => s
  }
}


class SVA(program: Program, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends SV(program, constProp), IRIntraproceduralForwardDependencies,
  SimplePushDownWorklistFixpointSolver[CFGPosition, Map[Variable, Map[SymBase, FlatElement[Set[BitVecLiteral]]]],
    MapLattice[Variable, Map[SymBase,  FlatElement[Set[BitVecLiteral]]],
      MapLattice[SymBase, FlatElement[Set[BitVecLiteral]], SetLatticeWithTop[BitVecLiteral]]]]
