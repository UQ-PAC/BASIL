package analysis.data_structure_analysis

import analysis.BitVectorEval.isNegative
import analysis.data_structure_analysis.SymBase.{Global, Heap, Par, Ret, Stack, Unknown}
import analysis.solvers.{SimplePushDownWorklistFixpointSolver, SimpleWorklistFixpointSolver}
import analysis.{Analysis, FlatElement, IRIntraproceduralForwardDependencies, MapLattice, PowerSetLatticeWithTop, evaluateExpression}
import ir.{BVADD, BVSUB, BinaryExpr, BitVecLiteral, CFGPosition, DirectCall, Expr, IntraProcIRCursor, LocalAssign, MemoryLoad, Procedure, Program, Register, Variable, computeDomain, toShortString}

import scala.Option


object Counter {
  private var unknownCounter: Int = 0
  private var heapCounter: Int = 0
  private var retCounter: Int = 0


  def getHeapCounter: Int = {
    heapCounter = heapCounter + 1
    heapCounter
  }

  def getRetCounter: Int = {
    retCounter = retCounter + 1
    retCounter
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
  case Ret(name: String, id: Int) extends SymBase
  case Global extends SymBase


abstract class SV(proc: Procedure,  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends
  Analysis[Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]]]] {

  Counter.reset()

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val implicitFormals: Set[Variable] = Set(linkRegister, framePointer)

  private val mallocRegister: Register = Register("R0", 64)

  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, Set(proc)).toSet

  val offsetSetLattice: PowerSetLatticeWithTop[BitVecLiteral] = PowerSetLatticeWithTop[BitVecLiteral]()
  val symValMapLattice: MapLattice[SymBase, Option[Set[BitVecLiteral]], offsetSetLattice.type] =
    MapLattice[SymBase, Option[Set[BitVecLiteral]], offsetSetLattice.type](offsetSetLattice)
  val lattice: MapLattice[CFGPosition, Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]],
    MapLattice[Variable, Map[SymBase, Option[Set[BitVecLiteral]]],
      symValMapLattice.type]] = MapLattice(MapLattice(symValMapLattice))


  private def initDef(base: SymBase, value: BitVecLiteral = BitVecLiteral(0, 64)): Map[SymBase, Option[Set[BitVecLiteral]]] = {
    Map(base -> Some(Set(value))).withDefaultValue(offsetSetLattice.bottom)
  }

  def exprToSymValMap(pos: CFGPosition, expr: Expr, svs: Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]]): Map[SymBase, Option[Set[BitVecLiteral]]] = {

    if evaluateExpression(expr, constProp(pos)).nonEmpty then
      val const = evaluateExpression(expr, constProp(pos)).get
      initDef(Global, const)
    else
      expr match
        case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(pos)).nonEmpty =>
          val test = svs(arg1)
          svs(arg1).map {
            case (base: SymBase, set: Some[Set[BitVecLiteral]]) =>
              val newSet = set.get.map {
                case el =>
                  val binOp = BinaryExpr(op,el, arg2)
                  evaluateExpression(binOp, constProp(pos)).get
                case p => p
              }
              base -> Some(newSet)
            case top => top
          }.withDefaultValue(offsetSetLattice.bottom)
        case variable: Variable => svs(variable)
        case _ =>
          val symSet = expr.variables.foldLeft(Set[SymBase]()) {
            (s, v) =>
              s ++ svs(v).keys
          }


          if (symSet.size == 1 && symSet.contains(Global)) then // TODO this is a work aorund the constProp setting all registers to top after a call
            expr.variables.foldLeft(Map[SymBase, Option[Set[BitVecLiteral]]]()) {
              (m, v) =>
                m ++ svs(v)
            }
          else
            assert(!symSet.contains(Global))
            symSet.foldLeft(Map[SymBase, Option[Set[BitVecLiteral]]]()) {
              (m, base) =>
                m + (base -> offsetSetLattice.top)
            }.withDefaultValue(offsetSetLattice.bottom)
  }

  def transfer(n: CFGPosition, s: Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]]): Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]] = {

    n match
      case procedure: Procedure => // entry
        (s + (stackPointer -> initDef(Stack(procedure.name))) + (linkRegister -> initDef(Par("link"))) + (framePointer -> initDef(Par("frame")))) ++
          procedure.in.foldLeft(Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]]()) {
          (m, param) =>
           m + (param.value -> initDef(Par(param.name)))
        }

      case alloc @ LocalAssign(lhs: Variable, rhs: BinaryExpr, _) if rhs.arg1 == stackPointer && rhs.op == BVADD &&
        evaluateExpression(rhs.arg2, constProp(alloc)).nonEmpty && isNegative(evaluateExpression(rhs.arg2, constProp(alloc)).get) => s // ignore stack allocations
      case pos @ LocalAssign(lhs: Variable, rhs, label) =>
        s + (lhs -> exprToSymValMap(pos, rhs, s)) // local
      case MemoryLoad(lhs, _, index, _, _, _) =>
        s + (lhs -> initDef(Unknown())) // load
      case DirectCall(target, _) if target.name == "malloc" => s + (mallocRegister -> initDef(Heap()))
      case DirectCall(target, _) => s ++ target.out.foldLeft(Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]]()) {
        (m, param) =>
          m + (param.value -> initDef(Ret(f"${target.name}_${param.name}", Counter.getRetCounter)))
      }
      case _ => s
  }
}


class SVAHelper(proc: Procedure, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends SV(proc, constProp), IRIntraproceduralForwardDependencies,
  SimpleWorklistFixpointSolver[CFGPosition, Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]],
    MapLattice[Variable, Map[SymBase,  Option[Set[BitVecLiteral]]],
      MapLattice[SymBase, Option[Set[BitVecLiteral]], PowerSetLatticeWithTop[BitVecLiteral]]]]


class SVA(proc: Procedure, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends Analysis[Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]]]] {

  private val sva = SVAHelper(proc, constProp)
  private val svaMap = sva.analyze()
  override def analyze(): Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[BitVecLiteral]]]]] = svaMap
  def exprToSymValSet(pos: CFGPosition, expr: Expr): Map[SymBase, Option[Set[BitVecLiteral]]] = {
    sva.exprToSymValMap(pos, expr, svaMap(pos))
  }
}

