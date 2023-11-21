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

trait ValueSetAnalysisMisc:

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
  val stateLattice: VSALatticeElem = powersetLattice

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
  def eval(cmd: Command, s: stateLattice.Element, n: CfgNode): stateLattice.Element = {
    Logger.debug(s"eval: $cmd")
    Logger.debug(s"state: $s")
    Logger.debug(s"node: $n")
    var m = s
    cmd match
      case localAssign: LocalAssign =>
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
                Logger.warn("could not find region for " + localAssign)
                m
          case e: Expr =>
            evaluateExpression(e, constantProp(n)) match {
              case Some(bv: BitVecLiteral) =>
                m = m + (localAssign.lhs -> Set(getValueType(bv)))
                m
              case None =>
                Logger.warn("could not evaluate expression" + e)
                m
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
                        Logger.warn(s"Too Complex: $storeValue") // do nothing
                        m
                    }
              case None =>
                Logger.warn("could not find region for " + memAssign)
                m
          case _ =>
            m
      case _ =>
        m
  }

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CfgNode, s: stateLattice.Element): stateLattice.Element =
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

/**
 * Base class for value analysis with lifted lattice, where the extra bottom element represents "unreachable".
 */
abstract class LiftedValueSetAnalysis[P <: ProgramCfg] (
    val cfg: P,
    val globals: Map[BigInt, String],
    val externalFunctions: Map[BigInt, String],
    val globalOffsets: Map[BigInt, BigInt],
    val subroutines: Map[BigInt, String],
    val mmm: MemoryModelMap,
    val constantProp: Map[CfgNode, Map[Variable, ConstantPropagationLattice.Element]],
    stateAfterNode: Boolean)
  extends FlowSensitiveAnalysis(stateAfterNode)
    with MapLatticeSolver[CfgNode]
    with ValueSetAnalysisMisc {

  /**
   * Lifted state lattice, with new bottom element representing "unreachable".
   */
  val liftedstatelattice: LiftLattice[stateLattice.type] = new LiftLattice(stateLattice)

  /**
   * The analysis lattice.
   */
  val lattice: MapLattice[CfgNode, liftedstatelattice.type] = new MapLattice(liftedstatelattice)

  override val domain: Set[CfgNode] = cfg.nodes.toSet

  /**
   * The worklist is initialized with all function entry nodes.
   */
  val first: Set[CfgNode] = cfg.funEntries.toSet[CfgNode]

  /**
   * Overrides `funsub` from [[tip.solvers.MapLatticeSolver]], treating function entry nodes as reachable.
   */
  override def funsub(n: CfgNode, x: lattice.Element, intra: Boolean): liftedstatelattice.Element = {
    import liftedstatelattice._
      n match {
        // function entry nodes are always reachable (if intra-procedural analysis)
        case entryNode: CfgFunctionEntryNode =>
          if entryNode.data.name.equals("main") then return lift(stateLattice.bottom)
        // all other nodes are processed with join+transfer
        case _ => return super.funsub(n, x, intra)
      }
    super.funsub(n, x, intra)
  }
}

/**
 * Functionality for basic analyses with lifted state lattice.
 */
trait LiftedValueSetAnalysisMisc extends ValueSetAnalysisMisc {

  /**
   * Transfer function for state lattice elements.
   * (Same as `localTransfer` for basic analyses with lifted state lattice.)
   */
  def transferUnlifted(n: CfgNode, s: stateLattice.Element): stateLattice.Element = localTransfer(n, s)
}

abstract class InterprocValueSetAnalysisWorklistSolver[L <: VSALatticeElem](
    cfg: ProgramCfg,
    globals: Map[BigInt, String],
    externalFunctions: Map[BigInt, String],
    globalOffsets: Map[BigInt, BigInt],
    subroutines: Map[BigInt, String],
    mmm: MemoryModelMap,
    constantProp: Map[CfgNode, Map[Variable, ConstantPropagationLattice.Element]],
    val powersetLattice: L
) extends LiftedValueSetAnalysis(
    cfg,
    globals,
    externalFunctions,
    globalOffsets, subroutines,
    mmm,
    constantProp,
    true)
    with LiftedValueSetAnalysisMisc
    with WorklistFixpointSolverWithReachability[CfgNode]
    with ForwardDependencies

object ValueSetAnalysis:

  /** Interprocedural analysis that uses the worklist solver.
    */
  class WorklistSolver(
      cfg: ProgramCfg,
      globals: Map[BigInt, String],
      externalFunctions: Map[BigInt, String],
      globalOffsets: Map[BigInt, BigInt],
      subroutines: Map[BigInt, String],
      mmm: MemoryModelMap,
      constantProp: Map[CfgNode, Map[Variable, ConstantPropagationLattice.Element]]
  ) extends InterprocValueSetAnalysisWorklistSolver(
        cfg,
        globals,
        externalFunctions,
        globalOffsets,
        subroutines,
        mmm,
        constantProp,
        MapLattice[Variable | MemoryRegion, PowersetLattice[Value]](PowersetLattice[Value])
      )
