package analysis.data_structure_analysis

import analysis.BitVectorEval.{bv2SignedInt, isNegative, signedInt2bv}
import analysis.data_structure_analysis.SymBase.{Global, Heap, Par, Ret, Stack, Unknown}
import analysis.solvers.{SimplePushDownWorklistFixpointSolver, SimpleWorklistFixpointSolver}
import analysis.{Analysis, FlatElement, IRIntraproceduralForwardDependencies, MapLattice, PowerSetLatticeWithTop, evaluateExpression}
import ir.{BVADD, BVSUB, BinaryExpr, BitVecLiteral, CFGPosition, DirectCall, Expr, Extract, IntraProcIRCursor, Literal, LocalAssign, MemoryLoad, Procedure, Program, Register, Repeat, SignExtend, UnaryExpr, UninterpretedFunction, Variable, ZeroExtend, computeDomain, toDot, toShortString}
import util.writeToFile

import scala.Option
import scala.collection.mutable



object HeapCounter extends Counter
object UnknownCounter extends Counter
object RetCounter extends  Counter

enum SymBase:
  case Heap(name: String) extends SymBase
  case Unknown(name: String) extends SymBase
  case Stack(name: String) extends SymBase
  case Par(name: String) extends SymBase
  case Ret(name: String) extends SymBase
  case Global extends SymBase


abstract class SV(proc: Procedure,  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], inParams: Map[Procedure, Set[Variable]], outParams: Map[Procedure, Set[Register]]) extends
  Analysis[Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]]]] {

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)


  private final val callerPreservedRegisters: Set[Variable] = Set("R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
    "R11", "R12", "R13", "R14", "R15", "R16", "R17", "R18").map(n => Register(n, 64))

  private val implicitFormals: Set[Variable] = Set(linkRegister, framePointer)

  private val mallocRegister: Register = Register("R0", 64)

  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, Set(proc)).toSet

  val offsetSetLattice: PowerSetLatticeWithTop[Int] = PowerSetLatticeWithTop[Int]()
  val symValMapLattice: MapLattice[SymBase, Option[Set[Int]], offsetSetLattice.type] =
    MapLattice[SymBase, Option[Set[Int]], offsetSetLattice.type](offsetSetLattice)
  val lattice: MapLattice[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]],
    MapLattice[Variable, Map[SymBase, Option[Set[Int]]],
      symValMapLattice.type]] = MapLattice(MapLattice(symValMapLattice))

  private def initDef(base: SymBase, value: Int = 0): Map[SymBase, Option[Set[Int]]] = {
    Map(base -> Some(Set(value))).withDefaultValue(offsetSetLattice.bottom)
  }

  private def labelToPC(label: Option[String]): String = {
    assert(label.nonEmpty)
    label.get.takeWhile(_ != ':')
  }
  def exprToSymValMap(pos: CFGPosition, expr: Expr, svs: Map[Variable, Map[SymBase, Option[Set[Int]]]]): Map[SymBase, Option[Set[Int]]] = {
    //val expr = unwrapPaddingAndSlicing(expression)
    if evaluateExpression(expr, constProp(pos)).nonEmpty then
      val const = bv2SignedInt(evaluateExpression(expr, constProp(pos)).get).toInt
      initDef(Global, const)
    else
      expr match
        case ZeroExtend(_, body) =>
          exprToSymValMap(pos, body, svs)
        case Extract(32, 0, body) => // TODO UNSOUND assume 32 bit extract is maintaining value
          exprToSymValMap(pos, body, svs) //.map(f => (f._1, f._2.map(s => s.map(b => Int(b.value, 32)))))
        case BinaryExpr(op, arg1, arg2) if evaluateExpression(arg2, constProp(pos)).nonEmpty =>
          exprToSymValMap(pos, arg1, svs).map {
            case (base: SymBase, set: Some[Set[Int]]) =>
              val newSet = set.get.map {
                case el =>
                  val operand = evaluateExpression(arg2, constProp(pos)).get
                  val binOp = BinaryExpr(op, signedInt2bv(el, operand.size), operand)
                  bv2SignedInt(evaluateExpression(binOp, constProp(pos)).get).toInt
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
          if (symSet.size == 1 && symSet.contains(Global)) then // TODO this is a work around the constProp setting all registers to top after a call
            expr.variables.foldLeft(Map[SymBase, Option[Set[Int]]]()) {
              (m, v) =>
                m ++ svs(v)
            }
          else
            assert(!symSet.contains(Global))
            symSet.foldLeft(Map[SymBase, Option[Set[Int]]]()) {
              (m, base) =>
                m + (base -> offsetSetLattice.top)
            }.withDefaultValue(offsetSetLattice.bottom)
  }

  def transfer(n: CFGPosition, s: Map[Variable, Map[SymBase, Option[Set[Int]]]]): Map[Variable, Map[SymBase, Option[Set[Int]]]] = {
     n match
        case procedure: Procedure => // entry
          (s + (stackPointer -> initDef(Stack(procedure.name))) + (linkRegister -> initDef(Par("link"))) + (framePointer -> initDef(Par("frame")))) ++
            inParams.getOrElse(procedure, callerPreservedRegisters).foldLeft(Map[Variable, Map[SymBase, Option[Set[Int]]]]()) {
            (m, param) =>
             m + (param -> initDef(Par(param.name)))
          }

        case alloc @ LocalAssign(lhs: Variable, rhs: BinaryExpr, label) if rhs.arg1 == stackPointer && rhs.op == BVADD &&
          evaluateExpression(rhs.arg2, constProp(alloc)).nonEmpty && isNegative(evaluateExpression(rhs.arg2, constProp(alloc)).get) =>
          s + (lhs -> s(stackPointer)) // ignore stack allocations
        case pos @ LocalAssign(lhs: Variable, rhs, label) =>
          s + (lhs -> exprToSymValMap(pos, rhs, s)) // local
        case MemoryLoad(lhs, _, index, _, _, label)  =>
          s + (lhs -> initDef(Unknown(s"Unknown_${proc.name}_${labelToPC(label)}"))) // load
        case DirectCall(target, label) if target.name == "malloc" => s + (mallocRegister -> initDef(Heap(f"Heap_${proc.name}_${labelToPC(label)}")))
        case DirectCall(target, label) => s ++ outParams.getOrElse(target, callerPreservedRegisters).foldLeft(Map[Variable, Map[SymBase, Option[Set[Int]]]]()) {
          (m, param) =>
            m + (param -> initDef(Ret(f"${proc.name}_${target.name}_${param.name}_${labelToPC(label)}")))
        }
        case _ => s
  }
}

class SVAHelper(proc: Procedure, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], inParams: Map[Procedure, Set[Variable]], outParams: Map[Procedure, Set[Register]]) extends SV(proc, constProp, inParams, outParams), IRIntraproceduralForwardDependencies,
  SimpleWorklistFixpointSolver[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]],
    MapLattice[Variable, Map[SymBase,  Option[Set[Int]]],
      MapLattice[SymBase, Option[Set[Int]], PowerSetLatticeWithTop[Int]]]]

class SVA(proc: Procedure, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], inParams: Map[Procedure, Set[Variable]], outParams: Map[Procedure, Set[Register]]) extends Analysis[Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]]]] {

  private val sva = SVAHelper(proc, constProp, inParams, outParams)
  val svaMap: Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]]] = sva.analyze()
  override def analyze(): Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]]] = {

    val domain: mutable.Set[CFGPosition] = computeDomain(IntraProcIRCursor, Set(proc))

    val labels: Map[CFGPosition, String] = svaMap.map(f => (f._1 -> f._2.toString()))
    writeToFile(toDot[CFGPosition](domain, IntraProcIRCursor, labels), s"${proc.name}.dot")
    svaMap
  }
  def exprToSymValSet(pos: CFGPosition, expr: Expr): Map[SymBase, Option[Set[Int]]] = {
    sva.exprToSymValMap(pos, expr, svaMap(pos))
  }
}

