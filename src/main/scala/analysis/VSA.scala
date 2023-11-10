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

type VSALatticeElem = MapLattice[Variable | MemoryRegion, PowersetLattice[Value]]

trait MemoryRegionValueSetAnalysis:

  val cfg: ProgramCfg
  val globals: Map[BigInt, String]
  val externalFunctions: Map[BigInt, String]
  val globalOffsets: Map[BigInt, BigInt]
  val subroutines: Map[BigInt, String]
  val mmm: MemoryModelMap
  val constantProp: Map[CfgNode, Map[Variable, ConstantPropagationLattice.Element]]

  /** The lattice of abstract values.
    */
  val powersetLattice: VSALatticeElem

  /** The lattice of abstract states.
    */
  val lattice: MapLattice[CfgNode, VSALatticeElem] = MapLattice(powersetLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))

  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)

  private val mallocVariable = Register("R0", BitVecType(64))

  def resolveGlobalOffset(address: BigInt): String = {
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

  def exprToRegion(expr: Expr, n: CfgNode): Option[MemoryRegion] = {
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

  def getValueType(bitVecLiteral: BitVecLiteral): Value = {
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
  def eval(cmd: Command, s: lattice.sublattice.Element, n: CfgNode): lattice.sublattice.Element = {
    Logger.debug(s"eval: $cmd")
    Logger.debug(s"state: $s")
    Logger.debug(s"node: $n")
    cmd match
      case localAssign: LocalAssign =>
        localAssign.rhs match
          case memoryLoad: MemoryLoad =>
            exprToRegion(memoryLoad.index, n) match
              case Some(r: MemoryRegion) =>
                // this is an exception to the rule and only applies to data regions
                evaluateExpression(memoryLoad.index, constantProp(n)) match
                  case Some(bitVecLiteral: BitVecLiteral) =>
                    val m = s + (r -> Set(getValueType(bitVecLiteral)))
                    m + (localAssign.lhs -> m(r))
                  case None =>
                    s + (localAssign.lhs -> s(r))
              case None =>
                Logger.warn("could not find region for " + localAssign)
                s
          case e: Expr =>
            evaluateExpression(e, constantProp(n)) match {
              case Some(bv: BitVecLiteral) => s + (localAssign.lhs -> Set(getValueType(bv)))
              case None =>
                Logger.warn("could not evaluate expression" + e)
                s
            }
      case memAssign: MemoryAssign =>
        memAssign.rhs.index match
          case binOp: BinaryExpr =>
            val region: Option[MemoryRegion] = exprToRegion(binOp, n)
            region match
              case Some(r: MemoryRegion) =>
                val storeValue = memAssign.rhs.value
                evaluateExpression(storeValue, constantProp(n)) match
                  case Some(bitVecLiteral: BitVecLiteral) =>
                    s + (r -> Set(getValueType(bitVecLiteral)))
                    /*
                  // TODO constant prop returned BOT OR TOP. Merge regions because RHS could be a memory loaded address
                  case variable: Variable =>
                    s + (r -> s(variable))
                    */
                  case None =>
                    storeValue.match {
                      case v: Variable =>
                        s + (r -> s(v))
                      case _ =>
                        Logger.warn(s"Too Complex: $storeValue") // do nothing
                        s
                    }
              case None =>
                Logger.warn("could not find region for " + memAssign)
                s
          case _ =>
            s
      case _ =>
        s
  }

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case entry: CfgFunctionEntryNode =>
        mmm.pushContext(entry.data.name)
        s
      case _: CfgFunctionExitNode =>
        mmm.popContext()
        s
      case cmd: CfgCommandNode =>
        eval(cmd.data, s, n)
      case _ => s // ignore other kinds of nodes
    }

/** Base class for memory region analysis (non-lifted) lattice.
  */
abstract class ValueSetAnalysis(
    val cfg: ProgramCfg,
    val globals: Map[BigInt, String],
    val externalFunctions: Map[BigInt, String],
    val globalOffsets: Map[BigInt, BigInt],
    val subroutines: Map[BigInt, String],
    val mmm: MemoryModelMap,
    val constantProp: Map[CfgNode, Map[Variable, ConstantPropagationLattice.Element]]
) extends FlowSensitiveAnalysis(true)
    with MemoryRegionValueSetAnalysis {

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = localTransfer(n, s)

}

abstract class IntraprocValueSetAnalysisWorklistSolver[L <: VSALatticeElem](
    cfg: ProgramCfg,
    globals: Map[BigInt, String],
    externalFunctions: Map[BigInt, String],
    globalOffsets: Map[BigInt, BigInt],
    subroutines: Map[BigInt, String],
    mmm: MemoryModelMap,
    constantProp: Map[CfgNode, Map[Variable, ConstantPropagationLattice.Element]],
    val powersetLattice: L
) extends ValueSetAnalysis(cfg, globals, externalFunctions, globalOffsets, subroutines, mmm, constantProp)
    with SimpleMonotonicSolver[CfgNode]
    with ForwardDependencies
    with Dependencies[CfgNode](true) 

object ValueSetAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
    */
  class WorklistSolver(
      cfg: ProgramCfg,
      globals: Map[BigInt, String],
      externalFunctions: Map[BigInt, String],
      globalOffsets: Map[BigInt, BigInt],
      subroutines: Map[BigInt, String],
      mmm: MemoryModelMap,
      constantProp: Map[CfgNode, Map[Variable, ConstantPropagationLattice.Element]]
  ) extends IntraprocValueSetAnalysisWorklistSolver(
        cfg,
        globals,
        externalFunctions,
        globalOffsets,
        subroutines,
        mmm,
        constantProp,
        MapLattice[Variable | MemoryRegion, PowersetLattice[Value]](PowersetLattice[Value])
      )
