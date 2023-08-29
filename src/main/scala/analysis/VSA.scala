package analysis

import ir._
import analysis.solvers._

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable
// import bitVector
import analysis.util.*

/** ValueSets are PowerSet of possible values */
trait Value
trait AddressValue(val expr: Expr, val name: String) extends Value

case class GlobalAddress(override val expr: Expr, override val name: String) extends AddressValue(expr, name) {
  override def toString: String = "GlobalAddress(" + expr + ")"
}

case class LocalAddress(override val expr: Expr, override val name: String) extends AddressValue(expr, name) {
  override def toString: String = "LocalAddress(" + expr + ")"
}

case class LiteralValue(expr: BitVecLiteral) extends Value {
  override def toString: String = "Literal(" + expr + ")"
}


trait MemoryRegionValueSetAnalysis:
  val regionContentMap: mutable.HashMap[MemoryRegion, mutable.Set[Value]] = mutable.HashMap.empty

  val cfg: ProgramCfg
  val globals: Map[BigInt, String]
  val externalFunctions: Map[BigInt, String]
  val globalOffsets: Map[BigInt, BigInt]
  val subroutines: Map[BigInt, String]
  val mmm: MemoryModelMap
  val constantProp: Map[CfgNode, Map[Variable, Any]]

  /** The lattice of abstract values.
   */
  val powersetLattice: MapLattice[Expr, PowersetLattice[Value]]

  /** The lattice of abstract states.
   */
  val lattice: MapLattice[CfgNode, MapLattice[Expr, PowersetLattice[Value]]] = MapLattice(powersetLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  private val stackPointer = Variable("R31", BitVecType(64))
  private val linkRegister = Variable("R30", BitVecType(64))
  private val framePointer = Variable("R29", BitVecType(64))

  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)

  private val mallocVariable = Variable("R0", BitVecType(64))

  def resolveGlobalOffset(address: BigInt): String = {
    val tableAddress = globalOffsets(address)
    if (globals.contains(tableAddress)) {
      globals(tableAddress )
    } else if (subroutines.contains(tableAddress)) {
      subroutines(tableAddress)
    } else {
      //throw Exception("Error: cannot resolve global offset " + address + " -> " + tableAddress)
      "@ERROR"
    }
  }


  def exprToRegion(expr: Expr, n: CfgNode): Option[MemoryRegion] = {
    expr match
      case binOp: BinaryExpr =>
        if (binOp.arg1 == stackPointer) {
          val rhs: Expr = evaluateExpression(binOp.arg2, n, constantProp)
          mmm.findObject(rhs.asInstanceOf[BitVecLiteral].value, "stack") match
            case Some(obj: MemoryRegion) => Some(obj)
            case _ => None
        } else {
          val evaluation: Expr = evaluateExpression(binOp, n, constantProp)
          if (!evaluation.isInstanceOf[BitVecLiteral]) {
            return None
          }
          mmm.findObject(evaluation.asInstanceOf[BitVecLiteral].value, "data") match
            case Some(obj: MemoryRegion) => Some(obj)
            case _ => None
        }
      case _ =>
        None
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
    println(s"eval: $cmd")
    println(s"state: $s")
    println(s"node: $n")
    cmd match
      case localAssign: LocalAssign =>
        localAssign.rhs match
          case memoryLoad: MemoryLoad =>
            val region: Option[MemoryRegion] = exprToRegion(memoryLoad.index, n)
            region match
              case Some(r: MemoryRegion) =>
                evaluateExpression(memoryLoad.index, n, constantProp) match
                  case bitVecLiteral: BitVecLiteral =>
                    regionContentMap.getOrElseUpdate(r, mutable.Set.empty[Value]).add(getValueType(bitVecLiteral))
                s + (localAssign.lhs -> regionContentMap.getOrElseUpdate(r, mutable.Set.empty[Value]).toSet)
              case None =>
                println("Warning: could not find region for " + localAssign)
                s
          case _ => s
      case memAssign: MemoryAssign =>
        memAssign.rhs.index match
          case binOp: BinaryExpr =>
            val region: Option[MemoryRegion] = exprToRegion(binOp, n)
            region match
              case Some(r: MemoryRegion) =>
                evaluateExpression(memAssign.rhs.value, n, constantProp) match
                  case bitVecLiteral: BitVecLiteral =>
                    regionContentMap.getOrElseUpdate(r, mutable.Set.empty[Value]).add(getValueType(bitVecLiteral))
                  case _ =>
                    println("Warning: could not find region for " + memAssign)
                s
              case None =>
                println("Warning: could not find region for " + memAssign)
                s
          case _ =>
            s
      case _ =>
        s
  }


//  /** Default implementation of eval.
//   */
//  def eval(stmt: Statement, s: lattice.sublattice.Element, n: CfgNode): lattice.sublattice.Element = {
//    stmt match {
//      case memAssign: MemoryAssign =>
//        if (ignoreRegions.contains(memAssign.rhs.value)) {
//          return s
//        }
//        memAssign.rhs.index match
//          case binOp: BinaryExpr =>
//            val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
//            val rhs: Expr = evaluateExpression(binOp.arg2, n)
//            rhs match {
//              case rhs: BitVecLiteral =>
//                lhs match {
//                  case lhs: Variable if lhs.equals(stackPointer) =>
//                    mmm.findObject(rhs.value, "stack") match {
//                      case Some(obj: MemoryRegion) =>
//                        evaluateExpression(memAssign.rhs.value, n) match {
//                          case bitVecLiteral: BitVecLiteral =>
//                            val map = regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value])
//                            if (externalFunctions.contains(bitVecLiteral.value)) {
//                              map.add(LocalAddress(bitVecLiteral, externalFunctions(bitVecLiteral.value)))
//                            } else if (globals.contains(bitVecLiteral.value)) {
//                              map.add(GlobalAddress(bitVecLiteral, globals(bitVecLiteral.value)))
//                            } else if (globalOffsets.contains(bitVecLiteral.value)) {
//                              map.add(GlobalAddress(bitVecLiteral, resolveGlobalOffset(bitVecLiteral.value)))
//                            } else if (subroutines.contains(bitVecLiteral.value)) {
//                              map.add(GlobalAddress(bitVecLiteral, subroutines(bitVecLiteral.value)))
//                            } else {
//                              map.add(LiteralValue(bitVecLiteral))
//                            }
//                            s
//                          case _ => s
//                        }
//                      case _ => s
//                    }
//                  case lhs: BitVecLiteral =>
//                    binOp.op match {
//                      case BVADD =>
//                        // should do something here
//                        val summation = lhs.value + rhs.value
//                        mmm.findObject(summation, "data") match {
//                          case Some(obj: MemoryRegion) =>
//                            evaluateExpression(memAssign.rhs.value, n) match {
//                              case bitVecLiteral: BitVecLiteral =>
//                                val map = regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value])
//                                if (externalFunctions.contains(bitVecLiteral.value)) {
//                                  map.add(LocalAddress(bitVecLiteral, externalFunctions(bitVecLiteral.value)))
//                                } else if (globals.contains(bitVecLiteral.value)) {
//                                  map.add(GlobalAddress(bitVecLiteral, globals(bitVecLiteral.value)))
//                                } else if (globalOffsets.contains(bitVecLiteral.value)) {
//                                  map.add(GlobalAddress(bitVecLiteral, resolveGlobalOffset(bitVecLiteral.value)))
//                                } else if (subroutines.contains(bitVecLiteral.value)) {
//                                  map.add(GlobalAddress(bitVecLiteral, subroutines(bitVecLiteral.value)))
//                                } else {
//                                  map.add(LiteralValue(bitVecLiteral))
//                                }
//                                s
//                              case _ => s
//                            }
//                          case _ => s
//                        }
//                      case _ =>
//                        println("ERROR: tried to eval operator: " + binOp.op + " in " + binOp)
//                        s
//                    }
//                  case _ => s
//                }
//              case _ =>
//                println("WARNING: RHS is not BitVecLiteral and is skipped " + rhs)
//                s
//            }
//          case _ => s
//      case localAssign: LocalAssign =>
//        localAssign.rhs match
//          case memLoad: MemoryLoad =>
//            memLoad.index match
//              case binOp: BinaryExpr =>
//                evaluateExpression(binOp.arg2, n) match {
//                  case rhs: BitVecLiteral =>
//                    val lhs = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
//                    lhs match {
//                      case lhs: Variable if lhs.equals(stackPointer) =>
//                        mmm.findObject(rhs.value, "stack") match
//                          case Some(obj: MemoryRegion) =>
//                            s + (localAssign.lhs -> regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).toSet)
//                          case _ => s
//                      case lhs: BitVecLiteral =>
//                        binOp.op match {
//                          case BVADD =>
//                            val summation = lhs.value + rhs.value
//                            mmm.findObject(summation, "data") match {
//                              case Some(obj: MemoryRegion) =>
//                                val setToAdd = mutable.Set.empty[Value]
//                                if (globals.contains(summation)) {
//                                  setToAdd.add(GlobalAddress(BitVecLiteral(summation, lhs.size), globals(summation)))
//                                } else if (globalOffsets.contains(summation)) {
//                                  setToAdd.add(GlobalAddress(BitVecLiteral(summation, lhs.size), resolveGlobalOffset(summation)))
//                                } else if (subroutines.contains(summation)) {
//                                  setToAdd.add(GlobalAddress(BitVecLiteral(summation, lhs.size), subroutines(summation)))
//                                } else if (externalFunctions.contains(summation)) {
//                                  setToAdd.add(LocalAddress(BitVecLiteral(summation, lhs.size), externalFunctions(summation)))
//                                } else {
//                                  setToAdd.add(LiteralValue(BitVecLiteral(summation, lhs.size)))
//                                }
//                                s + (localAssign.lhs -> regionContentMap.getOrElseUpdate(obj, setToAdd).toSet)
//                              case _ => s
//                            }
//                          case _ =>
//                            println("ERROR: tried to eval operator: " + binOp.op + " in " + binOp)
//                            s
//                        }
//                      case _ => s
//                    }
//                  case rhs: _ =>
//                    println("WARNING: RHS is not BitVecLiteral and is skipped " + rhs)
//                    s
//                }
//              case _ => s
//          case variable: Variable =>
//            s + (localAssign.lhs -> s.getOrElse(variable, Set.empty))
//          case _ => s
//      case _ =>
//        println(s"type: ${stmt.getClass} $stmt")
//        throw new Exception("Unknown type")
//    }
//  }

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
//        cmd.data match {
//          case memAssign: MemoryAssign =>
//            eval(memAssign, s, n)
//          // local assign is just lhs assigned to rhs we only need this information to track a prior register operation
//          // AKA: R1 <- R1 + 8; mem(R1) <- 0x1234
//          case localAssign: LocalAssign =>
//            eval(localAssign, s, n)
//          case _ => s
//        }
      case _ => s // ignore other kinds of nodes
    }


/** Base class for memory region analysis (non-lifted) lattice.
 */
abstract class ValueSetAnalysis(val cfg: ProgramCfg,
                                val globals: Map[BigInt, String],
                                val externalFunctions: Map[BigInt, String],
                                val globalOffsets: Map[BigInt, BigInt],
                                val subroutines: Map[BigInt, String],
                                val mmm: MemoryModelMap,
                                val constantProp: Map[CfgNode, Map[Variable, Any]])
  extends FlowSensitiveAnalysis(true) with MemoryRegionValueSetAnalysis {

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = localTransfer(n, s)

}


/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
 */
abstract class IntraprocValueSetAnalysisWorklistSolver[L <: MapLattice[Expr, PowersetLattice[Value]]]
(
  cfg: ProgramCfg,
  globals: Map[BigInt, String],
  externalFunctions: Map[BigInt, String],
  globalOffsets: Map[BigInt, BigInt],
  subroutines: Map[BigInt, String],
  mmm: MemoryModelMap,
  constantProp: Map[CfgNode, Map[Variable, Any]],
  val powersetLattice: L
)
  extends ValueSetAnalysis(cfg, globals, externalFunctions, globalOffsets, subroutines, mmm, constantProp)
    with SimpleMonotonicSolver[CfgNode]
    with ForwardDependencies

object ValueSetAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
   */
  class WorklistSolver(cfg: ProgramCfg,
                       globals: Map[BigInt, String],
                       externalFunctions: Map[BigInt, String],
                       globalOffsets: Map[BigInt, BigInt],
                       subroutines: Map[BigInt, String],
                       mmm: MemoryModelMap,
                       constantProp: Map[CfgNode, Map[Variable, Any]])
    extends IntraprocValueSetAnalysisWorklistSolver(
      cfg,
      globals,
      externalFunctions,
      globalOffsets,
      subroutines,
      mmm,
      constantProp,
      MapLattice[Expr, PowersetLattice[Value]](PowersetLattice[Value])
    )