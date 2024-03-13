package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.{Assert, Assume, BVADD, BVAND, BVASHR, BVBinOp, BVCOMP, BVCONCAT, BVEQ, BVLSHR, BVMUL, BVNAND, BVNEQ, BVNOR, BVOR, BVSDIV, BVSGE, BVSGT, BVSHL, BVSLE, BVSLT, BVSMOD, BVSREM, BVSUB, BVUDIV, BVUGE, BVUGT, BVULE, BVULT, BVUREM, BVXNOR, BVXOR, BinOp, BinaryExpr, BitVecLiteral, BitVecType, BoolBinOp, CFGPosition, Call, DirectCall, Expr, Extract, GoTo, IntBinOp, IntraProcIRCursor, Literal, LocalAssign, Memory, MemoryAssign, MemoryLoad, MemoryStore, NOP, Procedure, Program, Register, Repeat, SignExtend, Statement, UnaryExpr, Variable, ZeroExtend, computeDomain}
import util.Logger

import scala.collection.mutable

class SymbolicAccess(val symbolicBase: MemoryRegion2, val offset: BigInt) {
  override def toString: String = s"SymbolicAccess($symbolicBase, $offset)"
}

class PrePass(program: Program,
              constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]])

  extends Analysis[Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]]],
    IRIntraproceduralForwardDependencies,
    SimpleWorklistFixpointSolver[CFGPosition, Map[Variable, Set[SymbolicAccess]],
      MapLattice[Variable, Set[SymbolicAccess], PowersetLattice[SymbolicAccess]]] {


  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, program.procedures).toSet
  val lattice: MapLattice[CFGPosition, Map[Variable, Set[SymbolicAccess]],
    MapLattice[Variable, Set[SymbolicAccess], PowersetLattice[SymbolicAccess]]] = MapLattice(MapLattice(PowersetLattice()))

//  private var lastR0: MemoryRegion2 | BitVecLiteral = _

  var mallocCount: Int
  = 0
//  private var stackCount: Int = 0
//  val stackMap: mutable.Map[Procedure, mutable.Map[Expr, StackRegion2]] = mutable.Map()

  private def nextMallocCount() = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

//  private def nextStackCount() = {
//    stackCount += 1
//    s"stack_$stackCount"
//  }


  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))
  private val mallocVariable = Register("R0", BitVecType(64))

  def updateOffsets(variable: Variable, arg: Variable, offsetChange:BigInt, op: BinOp, s: Map[Variable, Set[SymbolicAccess]]): Map[Variable, Set[SymbolicAccess]] = {
    val newSyms: mutable.Set[SymbolicAccess] = mutable.Set()
    s(arg).foreach(
      sym =>
        op match
          case BVADD => newSyms.add(SymbolicAccess(sym.symbolicBase, sym.offset + offsetChange))
          case BVSUB => newSyms.add(SymbolicAccess(sym.symbolicBase, sym.offset - offsetChange))
          case _ =>  ???// check if this happens often
    )
    s.updated(variable, newSyms.toSet)
  }

  def transfer(n: CFGPosition, s: Map[Variable, Set[SymbolicAccess]]): Map[Variable, Set[SymbolicAccess]] = {
    n match
      case LocalAssign(variable, expr, maybeString) =>
        expr match
          case BinaryExpr(op, arg1: Variable, arg2) if s.contains(arg1) => // arg1 is a symbolic access variable
            evaluateExpression(arg2, constProp(n)) match
              case Some(value) => // arg2 is some constant
                val newSyms: mutable.Set[SymbolicAccess] = mutable.Set()
                  updateOffsets(variable, arg1, value.value, op, s)
              case None => // couldn't evaluate R2 to a constant
                arg2 match
                  case vari:Variable if s.contains(vari) =>
                    evaluateExpression(arg1, constProp(n)) match
                      case Some(value) =>
                        updateOffsets(variable, vari, value.value, op, s)
                      case None => s.removed(variable)
                  case _ =>
                    s.removed(variable)
          case BinaryExpr(op, arg1, arg2: Variable) if s.contains(arg2) =>
            evaluateExpression(arg1, constProp(n)) match
              case Some(value) => // arg1 is some constant
                val newSyms: mutable.Set[SymbolicAccess] = mutable.Set()
                updateOffsets(variable, arg2, value.value, op, s)
              case None => s.removed(variable) // couldn't evaluate R1 to a constant
          case vari: Variable if s.contains(vari) =>
            s.updated(variable, s(vari))
          case _ =>
            s.removed(variable)
      case entryPoint: Procedure =>
        s + (stackPointer -> Set(SymbolicAccess(StackRegion2(s"Stack_${entryPoint.name}", entryPoint), 0)))
      case directCall: DirectCall if directCall.target.name == "malloc" =>
        nextMallocCount()
        evaluateExpression(mallocVariable, constProp(n)) match
          case Some(value) =>
            s.updated(mallocVariable, Set(SymbolicAccess(HeapRegion2(s"Malloc-${mallocCount}", value), 0)))
          case None =>
            s.updated(mallocVariable, Set(SymbolicAccess(HeapRegion2(s"Malloc-${mallocCount}", BitVecLiteral(-1, 64)), 0)))
      case _ => s
  }



}

trait MemoryRegion2 {
  val regionIdentifier: String
}

case class StackRegion2(override val regionIdentifier: String, parent: Procedure) extends MemoryRegion2 {
  override def toString: String = s"Stack($regionIdentifier, ${parent.name})"
}

case class HeapRegion2(override val regionIdentifier: String, size: BitVecLiteral) extends MemoryRegion2 {
  override def toString: String = s"Heap($regionIdentifier, $size)"
}

case class DataRegion2(override val regionIdentifier: String, start: BitVecLiteral) extends MemoryRegion2 {
  override def toString: String = s"Data($regionIdentifier, $start)"
}



