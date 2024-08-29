package analysis

import ir._
import analysis.solvers._

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable
import util.Logger

/** ValueSets are PowerSet of possible values */
trait Value {
  val expr: BitVecLiteral
}
trait AddressValue extends Value {
  val name: String
}

case class GlobalAddress(override val expr: BitVecLiteral, override val name: String) extends AddressValue {
  override def toString: String = "GlobalAddress(" + expr + ", " + name + ")"
}

case class LocalAddress(override val expr: BitVecLiteral, override val name: String) extends AddressValue {
  override def toString: String = "LocalAddress(" + expr + ", " + name + ")"
}

case class LiteralValue(expr: BitVecLiteral) extends Value {
  override def toString: String = "Literal(" + expr + ")"
}

trait ValueSetAnalysis(program: Program,
                        globals: Map[BigInt, String],
                        externalFunctions: Map[BigInt, String],
                        globalOffsets: Map[BigInt, BigInt],
                        subroutines: Map[BigInt, String],
                        mmm: MemoryModelMap,
                        constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) {

  val powersetLattice: PowersetLattice[Value] = PowersetLattice()

  val mapLattice: MapLattice[Variable | MemoryRegion, Set[Value], PowersetLattice[Value]] = MapLattice(powersetLattice)

  val liftedLattice: LiftLattice[Map[Variable | MemoryRegion, Set[Value]], mapLattice.type] = LiftLattice(mapLattice)

  val lattice: MapLattice[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]], LiftLattice[Map[Variable | MemoryRegion, Set[Value]], mapLattice.type]] = MapLattice(liftedLattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  val first: Set[CFGPosition] = Set.empty ++ program

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)

  private val mallocVariable = Register("R0", 64)

  private def resolveGlobalOffset(address: BigInt): String = {
    val tableAddress = globalOffsets(address)
    if (globals.contains(tableAddress)) {
      globals(tableAddress)
    } else if (subroutines.contains(tableAddress)) {
      subroutines(tableAddress)
    } else {
      //throw Exception("Error: cannot resolve global offset " + address + " -> " + tableAddress)
      "@ERROR"
    }
  }

  def exprToRegion(expr: Expr, n: CFGPosition): Option[MemoryRegion] = {
    expr match {
      case binOp: BinaryExpr if binOp.arg1 == stackPointer =>
        evaluateExpression(binOp.arg2, constantProp(n)) match {
          case Some(b: BitVecLiteral) => mmm.findStackObject(b.value)
          case None => None
        }
      case _ =>
        evaluateExpression(expr, constantProp(n)) match {
          case Some(b: BitVecLiteral) => mmm.findDataObject(b.value)
          case None => None
        }
    }
  }

  private def getValueType(bitVecLiteral: BitVecLiteral): Value = {
    if (externalFunctions.contains(bitVecLiteral.value)) {
      LocalAddress(bitVecLiteral, externalFunctions(bitVecLiteral.value))
    } else if (globals.contains(bitVecLiteral.value)) {
      GlobalAddress(bitVecLiteral, globals(bitVecLiteral.value))
    } else if (globalOffsets.contains(bitVecLiteral.value)) {
      GlobalAddress(bitVecLiteral, resolveGlobalOffset(bitVecLiteral.value))
    } else if (subroutines.contains(bitVecLiteral.value)) {
      GlobalAddress(bitVecLiteral, subroutines(bitVecLiteral.value))
    } else {
      LiteralValue(bitVecLiteral)
    }
  }

  /** Default implementation of eval.
    */
  def eval(cmd: Command, s: Map[Variable | MemoryRegion, Set[Value]], n: CFGPosition): Map[Variable | MemoryRegion, Set[Value]] = {
    Logger.debug(s"eval: $cmd")
    Logger.debug(s"state: $s")
    Logger.debug(s"node: $n")
    var m = s
    cmd match
      case localAssign: Assign =>
        localAssign.rhs match
          case memoryLoad: MemoryLoad =>
            exprToRegion(memoryLoad.index, n) match
              case Some(r: MemoryRegion) =>
                // this is an exception to the rule and only applies to data regions
                evaluateExpression(memoryLoad.index, constantProp(n)) match
                  case Some(bitVecLiteral: BitVecLiteral) =>
                    m = m + (r -> Set(getValueType(bitVecLiteral)))
                    m = m + (localAssign.lhs -> m(r))
                    m
                  case None =>
                    m = m + (localAssign.lhs -> m(r))
                    m
              case None =>
                Logger.debug("could not find region for " + localAssign)
                m
          case e: Expr =>
            evaluateExpression(e, constantProp(n)) match {
              case Some(bv: BitVecLiteral) =>
                m = m + (localAssign.lhs -> Set(getValueType(bv)))
                m
              case None =>
                Logger.debug("could not evaluate expression" + e)
                m
            }
      case memAssign: MemoryAssign =>
        memAssign.index match
          case binOp: BinaryExpr =>
            val region: Option[MemoryRegion] = exprToRegion(binOp, n)
            region match
              case Some(r: MemoryRegion) =>
                val storeValue = memAssign.value
                evaluateExpression(storeValue, constantProp(n)) match
                  case Some(bitVecLiteral: BitVecLiteral) =>
                    m = m + (r -> Set(getValueType(bitVecLiteral)))
                    m
                    /*
                  // TODO constant prop returned BOT OR TOP. Merge regions because RHS could be a memory loaded address
                  case variable: Variable =>
                    s + (r -> s(variable))
                    */
                  case None =>
                    storeValue.match {
                      case v: Variable =>
                        m = m + (r -> m(v))
                        m
                      case _ =>
                        Logger.debug(s"Too Complex: $storeValue") // do nothing
                        m
                    }
              case None =>
                Logger.debug("could not find region for " + memAssign)
                m
          case _ =>
            m
      case _ =>
        m
  }

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CFGPosition, s: Map[Variable | MemoryRegion, Set[Value]]): Map[Variable | MemoryRegion, Set[Value]] =
    if (IRWalk.procedure(n) == n) {
      mmm.pushContext(n.asInstanceOf[Procedure].name)
      s
    } else if (IRWalk.lastInProc(IRWalk.procedure(n)) == n) {
      mmm.popContext()
      s
    } else n match
      case command: Command =>
        eval(command, s, n)
      case _ =>
        s

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transferUnlifted(n: CFGPosition, s: Map[Variable | MemoryRegion, Set[Value]]): Map[Variable | MemoryRegion, Set[Value]] = localTransfer(n, s)
}

class ValueSetAnalysisSolver(
    program: Program,
    globals: Map[BigInt, String],
    externalFunctions: Map[BigInt, String],
    globalOffsets: Map[BigInt, BigInt],
    subroutines: Map[BigInt, String],
    mmm: MemoryModelMap,
    constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
) extends ValueSetAnalysis(program, globals, externalFunctions, globalOffsets, subroutines, mmm, constantProp)
    with IRInterproceduralForwardDependencies
    with Analysis[Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]]
    with WorklistFixpointSolverWithReachability[CFGPosition, Map[Variable | MemoryRegion, Set[Value]], MapLattice[Variable | MemoryRegion, Set[Value], PowersetLattice[Value]]] {

  override def funsub(n: CFGPosition, x: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]): LiftedElement[Map[Variable | MemoryRegion, Set[Value]]] = {
    n match {
      // function entry nodes are always reachable as this is intraprocedural
      case _: Procedure => liftedLattice.lift(mapLattice.bottom)
      // all other nodes are processed with join+transfer
      case _ => super.funsub(n, x)
    }
  }
}
