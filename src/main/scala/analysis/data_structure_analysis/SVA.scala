package analysis.data_structure_analysis

import analysis.data_structure_analysis.SymBase.{Global, Heap, Par, Ret, Stack, Unknown}
import analysis.solvers.{SimplePushDownWorklistFixpointSolver, SimpleWorklistFixpointSolver}
import analysis.{Analysis, FlatElement, IRIntraproceduralForwardDependencies, Loop, MapLattice, PowerSetLatticeWithTop, evaluateExpression}
import ir.eval.BitVectorEval.*
import ir.{toDot, BVADD, BVSUB, BinaryExpr, BitVecLiteral, BitVecType, Block, CFGPosition, DirectCall, Expr, Extract, IntraProcIRCursor, Literal, LocalAssign, LocalVar, MemoryLoad, Procedure, Program, Register, Repeat, SignExtend, UnaryExpr, UninterpretedFunction, Variable, ZeroExtend, computeDomain, toShortString}
import util.writeToFile

import scala.Option

enum SymBase:
  case Heap(name: String) extends SymBase
  case Unknown(name: String) extends SymBase
  case Stack(name: String) extends SymBase
  case Par(name: String) extends SymBase
  case Ret(name: String) extends SymBase
  case Global extends SymBase


abstract class SV(proc: Procedure,  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], loops: Set[Loop]) extends
  Analysis[Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]]]] {

  private val stackPointer = LocalVar("R31_in", BitVecType(64)) //Register("R31", 64)
  private val linkRegister = LocalVar("R30_in", BitVecType(64)) //Register("R30", 64)
  private val framePointer = LocalVar("R29_in", BitVecType(64)) //Register("R29", 64)

  private val implicitFormals: Set[LocalVar] = Set(linkRegister, framePointer, framePointer)

  private val mallocRegister: Register = Register("R0", 64)

  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, Set(proc)).toSet

  // Map from a loop header to the last known state of the SV at the header
  var loopHeaderToPrev: Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]]] = Map.empty

  // Map from a loop header to the number of times the header is seen
  var loopHeaderCount: Map[CFGPosition, Int] = Map.empty.withDefaultValue(0)

  val offsetSetLattice: PowerSetLatticeWithTop[Int] = PowerSetLatticeWithTop[Int]()
  val symValMapLattice: MapLattice[SymBase, Option[Set[Int]], offsetSetLattice.type] =
    MapLattice[SymBase, Option[Set[Int]], offsetSetLattice.type](offsetSetLattice)
  val lattice: MapLattice[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]],
    MapLattice[Variable, Map[SymBase, Option[Set[Int]]],
      symValMapLattice.type]] = MapLattice(MapLattice(symValMapLattice))

  private def initDef(base: SymBase, value: Int = 0): Map[SymBase, Option[Set[Int]]] = {
    Map(base -> Some(Set(value))).withDefaultValue(offsetSetLattice.bottom)
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
            // assert(!symSet.contains(Global))
            symSet.foldLeft(Map[SymBase, Option[Set[Int]]]()) {
              (m, base) =>
                m + (base -> offsetSetLattice.top)
            }.withDefaultValue(offsetSetLattice.bottom)
  }

  // It widens the SymValSets at loop headers to avoid positive loop cycles
  // it only looks at the number of times loop header has been seen and if it exceeds cut off
  // it sets the symbases changed since last seen to top
  def widenOnLoopHeader(n: CFGPosition, s: Map[Variable, Map[SymBase, Option[Set[Int]]]], cutOff: Int = 10): Map[Variable, Map[SymBase, Option[Set[Int]]]] = {
    if loopHeaderToPrev.contains(n) && loopHeaderToPrev(n) != s && loopHeaderCount(n) >= cutOff then
      loopHeaderCount += n -> (loopHeaderCount(n) + 1)
      val oldSVs = loopHeaderToPrev(n)
      val newSV = s.foldLeft(Map[Variable, Map[SymBase, Option[Set[Int]]]]()) {
        (newS, sv) =>
          val variable = sv._1
          val symBases = sv._2
          if oldSVs.contains(variable) && oldSVs(variable) != symBases then
            val update = variable -> symBases.foldLeft(Map[SymBase, Option[Set[Int]]]()) {
              (updatedSV, currentSV) =>
                val base = currentSV._1
                val valueSet = currentSV._2
                if valueSet.isEmpty || (!oldSVs(variable).contains(base)) then
                  updatedSV + currentSV
                else
                  val oldValueSet = oldSVs(variable)(base).get // shouldn't be TOP (None), since no narrowing is implemented
                  if oldValueSet.diff(valueSet.get).nonEmpty then
                    updatedSV + currentSV + (base -> offsetSetLattice.top)
                  else
                    updatedSV + currentSV
            }
            newS + sv + update
          else
            newS + sv
      }
      loopHeaderToPrev += n -> newSV
      newSV
    else
      loopHeaderCount += n -> (loopHeaderCount(n) + 1)
      loopHeaderToPrev += n -> s
      s
  }

  def labelToPC(label: Option[String]): String = {
    assert(label.nonEmpty)
    label.get.takeWhile(_ != ':')
  }

  def transfer(n: CFGPosition, s: Map[Variable, Map[SymBase, Option[Set[Int]]]]): Map[Variable, Map[SymBase, Option[Set[Int]]]] = {
//    if loops.exists(_.header == n) then
//      assert(n.isInstanceOf[Block])
//      widenOnLoopHeader(n, s)
//
//    else
      n match
        case procedure: Procedure => // entry
          (s + (stackPointer -> initDef(Stack(procedure.name))) + (linkRegister -> initDef(Par("link"))) + (framePointer -> initDef(Par("frame")))) ++
            procedure.formalInParam.diff(implicitFormals).foldLeft(Map[Variable, Map[SymBase, Option[Set[Int]]]]()) {
            (m, param) =>
             m + (param -> initDef(Par(param.name)))
          }

        case alloc @ LocalAssign(lhs: Variable, rhs: BinaryExpr, label) if rhs.arg1 == stackPointer && rhs.op == BVADD &&
          evaluateExpression(rhs.arg2, constProp(alloc)).nonEmpty && isNegative(evaluateExpression(rhs.arg2, constProp(alloc)).get) =>
          // stack allocation
          // set lhs to stack param
          s + (lhs -> exprToSymValMap(alloc, stackPointer, s))
        case pos @ LocalAssign(lhs: Variable, rhs, label) =>
          s + (lhs -> exprToSymValMap(pos, rhs, s)) // local
        case MemoryLoad(lhs, _, index, _, _, label)  =>
          s + (lhs -> initDef(Unknown(s"${proc.name}_${labelToPC(label)}"))) // load
        case DirectCall(target, _, _ , label) if target.name == "malloc" => s + (mallocRegister -> initDef(Heap(f"${proc.name}_${labelToPC(label)}")))
        case DirectCall(target, _, _ , label) => s ++ target.formalOutParam.foldLeft(Map[Variable, Map[SymBase, Option[Set[Int]]]]()) {
          (m, param) =>
            m + (param -> initDef(Ret(f"${proc.name}_${target.name}_${param.name}_${labelToPC(label)}")))
        }
        case _ => s
  }
}

class SVAHelper(proc: Procedure, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], loops: Set[Loop]) extends SV(proc, constProp, loops), IRIntraproceduralForwardDependencies,
  SimpleWorklistFixpointSolver[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]],
    MapLattice[Variable, Map[SymBase,  Option[Set[Int]]],
      MapLattice[SymBase, Option[Set[Int]], PowerSetLatticeWithTop[Int]]]]

class SVA(proc: Procedure, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], loops: Set[Loop]) extends Analysis[Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]]]] {

  private val sva = SVAHelper(proc, constProp, loops)
  val svaMap = sva.analyze()
  override def analyze(): Map[CFGPosition, Map[Variable, Map[SymBase, Option[Set[Int]]]]] = {
    writeToFile(toDot[CFGPosition](sva.domain, IntraProcIRCursor, svaMap.map(f => (f._1, f._2.toString))), s"${proc.name}_SVA.dot")
    svaMap
  }
  def exprToSymValSet(pos: CFGPosition, expr: Expr): Map[SymBase, Option[Set[Int]]] = {
    sva.exprToSymValMap(pos, expr, svaMap(pos))
  }
}

