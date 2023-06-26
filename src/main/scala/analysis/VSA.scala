package analysis

import ir.{DirectCall, UnaryExpr, *}
import analysis.solvers.*
import boogie.BExpr
import specification.{InternalFunction, SpecGlobal}

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable

/** ValueSets are PowerSet of possible values */

abstract class Value
class AddressValue(var _expr: Expr, var _name: String) extends Value:
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Address(")
    sb.append(_expr)
    sb.append(")")
    sb.toString()
  }

case class GlobalAddress(expr: Expr, name: String) extends AddressValue(expr, name):
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("GlobalAddress(")
    sb.append(expr)
    sb.append(")")
    sb.toString()
  }
case class LocalAddress(expr: Expr, name: String) extends AddressValue(expr, name):
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("LocalAddress(")
    sb.append(expr)
    sb.append(")")
    sb.toString()
  }

class LiteralValue(expr: Expr) extends Value:
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Literal(")
    sb.append(expr)
    sb.append(")")
    sb.toString()
  }

//class ValueSet(expr: Expr, values: mutable.Set[Value]) {
//  override def toString: String = {
//    val sb = new StringBuilder
//    sb.append(expr)
//    sb.append(" -> {")
//    sb.append(values.mkString(", "))
//    sb.append("}")
//    sb.toString()
//  }
//}


trait ValueSetAnalysisMisc:
  val assigmentsMap: mutable.HashMap[(Expr, CfgNode), Expr] = mutable.HashMap.empty
  val regionContentMap: mutable.HashMap[MemoryRegion, mutable.Set[Value]] = mutable.HashMap.empty

  val cfg: ProgramCfg
  val globals: Set[SpecGlobal]
  val internalFunctions: Set[InternalFunction]
  val globalOffsets: Map[BigInt, BigInt]
  val mmm: MemoryModelMap

  /** The lattice of abstract values.
   */
  val powersetLattice: MapLattice[Expr, PowersetLattice[Value]]

  /** The lattice of abstract states.
   */
  val lattice: MapLattice[CfgNode, MapLattice[Expr, PowersetLattice[Value]]] = MapLattice(powersetLattice)

  val domain: Set[CfgNode] = cfg.nodes

  private val stackPointer = Variable("R31", BitVecType(64))
  private val linkRegister = Variable("R30", BitVecType(64))
  private val framePointer = Variable("R29", BitVecType(64))

  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)

  private val mallocVariable = Variable("R0", BitVecType(64))

  private val loopEscapeSet: mutable.Set[CfgNode] = mutable.Set.empty

  def loopEscape(n: CfgNode): Boolean = {
    if (loopEscapeSet.contains(n)) {
      return true
    }
    loopEscapeSet.add(n)
    false
  }

  /** Find decl of variables from node predecessors */
  def findDecl(variable: Variable, n: CfgNode): mutable.ListBuffer[CfgNode] = {
    val decls: mutable.ListBuffer[CfgNode] = mutable.ListBuffer.empty
    // if we have a temporary variable then ignore it
    if (variable.name.contains("#")) {
      return decls
    }
    for (pred <- n.pred) {
      if (loopEscape(pred)) {
        return mutable.ListBuffer.empty
      }
      pred match {
        case cmd: CfgCommandNode =>
          cmd.data match {
            case localAssign: LocalAssign =>
              if (localAssign.lhs == variable) {
                decls.addOne(pred)
              } else {
                decls.addAll(findDecl(variable, pred))
              }
            case _ => decls.addAll(findDecl(variable, pred))
          }
        case _ => decls.addAll(findDecl(variable, pred))
      }
    }
    decls
  }

  def is_global(bigInt: BigInt): Boolean = {
    for (global <- globals) {
      if (global.address == bigInt) {
        return true
      }
    }

    for (global <- globalOffsets) {
      if (global._1 == bigInt) {
        return true
      }
    }
    false
  }

  def getName_global(bigInt: BigInt): String = {
    for (global <- globals) {
      if (global.address == bigInt) {
        return global.name
      }
    }

    for (global <- globalOffsets) {
      if (global._1 == bigInt) {
        return getName_global(global._2)
      }
    }
    throw new RuntimeException("Not possible to get name of global offset" + bigInt)
  }

  def is_internalFunction(bigInt: BigInt): Boolean = {
    for (internalFunction <- internalFunctions) {
      if (internalFunction.offset == bigInt) {
        return true
      }
    }
    false
  }

  def getName_internalFunction(bigInt: BigInt): String = {
    for (internalFunction <- internalFunctions) {
      if (internalFunction.offset == bigInt) {
        return internalFunction.name
      }
    }
    "ERROR" // not possible
  }

  /**
   * Evaluate an expression in a hope of finding a global variable.
   *
   * @param exp : The expression to evaluate (e.g. R1 + 0x1234)
   * @param n   : The node where the expression is evaluated (e.g. mem[R1 + 0x1234] <- ...)
   * @return: The evaluated expression (e.g. 0x69632)
   */
  def evaluateExpression(exp: Expr, n: CfgNode): Expr = {
    exp match {
      case binOp: BinaryExpr =>
        binOp.arg1 match {
          case variable: Variable =>
            loopEscapeSet.clear()
            for (pred <- findDecl(variable, n)) {
              assigmentsMap.get(variable, pred) match
                case Some(value) =>
                  value match
                    case bitVecLiteral: BitVecLiteral =>
                      if (!binOp.arg2.isInstanceOf[BitVecLiteral]) {
                        return exp
                      }
                      val calculated: BigInt = bitVecLiteral.value.+(binOp.arg2.asInstanceOf[BitVecLiteral].value)
                      return BitVecLiteral(calculated, bitVecLiteral.size)
                    case _ => return exp
                case _ =>
                  print("ERROR: CASE NOT HANDLED: " + assigmentsMap.get(variable, pred) + " FOR " + binOp + "\n")
                  return exp
            }
          case _ => return evaluateExpression(binOp.arg1, n)
        }
        exp
      case memLoad: MemoryLoad =>
        evaluateExpression(memLoad.index, n)
      case bitVecLiteral: BitVecLiteral =>
        bitVecLiteral
      case extend: ZeroExtend =>
        evaluateExpression(extend.body, n)
      case variable: Variable =>
        loopEscapeSet.clear()
        val ssss = findDecl(variable, n)
        loopEscapeSet.clear()
        for (pred <- findDecl(variable, n)) {
          if (!assigmentsMap.contains((variable, pred))) {
            pred match
              case cmd: CfgCommandNode =>
                cmd.data match
                  case localAssign: LocalAssign =>
                    assigmentsMap.addOne((localAssign.lhs, pred) -> evaluateExpression(localAssign.rhs, pred))
                    return evaluateExpression(localAssign.rhs, pred)
          } else {
            assigmentsMap(variable, pred) match
              case bitVecLiteral: BitVecLiteral =>
                return bitVecLiteral
              case any: Expr => return exp
          }
        }
        exp
      case extract: Extract =>
        evaluateExpression(extract.body, n)
      case _ =>
        //throw new RuntimeException("ERROR: CASE NOT HANDLED: " + exp + "\n")
        exp
    }
  }


  def exprToRegion(expr: Expr, n: CfgNode): Option[MemoryRegion] = {
    expr match
      case binOp: BinaryExpr =>
        val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
        val rhs: Expr = evaluateExpression(binOp.arg2, n)
        if (lhs.equals(stackPointer)) {
          mmm.findObject(rhs.asInstanceOf[BitVecLiteral].value, "stack") match
            case Some(obj: MemoryRegion) =>
              return Some(obj)
            case _ =>
              return None
        } else {
            mmm.findObject(lhs.asInstanceOf[BitVecLiteral].value, "data") match
                case Some(obj: MemoryRegion) =>
                  return Some(obj)
                case _ =>
                return None
            }
        None
      case _ =>
        None
  }


  /** Default implementation of eval.
   */
  def eval(stmt: Statement, s: lattice.sublattice.Element, n: CfgNode): lattice.sublattice.Element = {
    stmt match {
      case memAssign: MemoryAssign =>
        if (ignoreRegions.contains(memAssign.rhs.value)) {
          return s
        }
        memAssign.rhs.index match
          case binOp: BinaryExpr =>
            val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
            val rhs: Expr = evaluateExpression(binOp.arg2, n)
            if (!rhs.isInstanceOf[BitVecLiteral]) {
              println("WARNING: RHS is not BitVecLiteral and is skipped " + rhs + "\n")
              return s
            }
            if (lhs.equals(stackPointer)) {
              mmm.findObject(rhs.asInstanceOf[BitVecLiteral].value, "stack") match
                case Some(obj: MemoryRegion) =>
                  val evaluatedResults = evaluateExpression(memAssign.rhs.value, n)
                  evaluatedResults match
                    case bitVecLiteral: BitVecLiteral =>
                      if (is_internalFunction(bitVecLiteral.value)) {
                        regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).add(LocalAddress(bitVecLiteral, getName_internalFunction(bitVecLiteral.value)))
                      } else if (is_global(bitVecLiteral.value)) {
                        regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).add(GlobalAddress(bitVecLiteral, getName_global(bitVecLiteral.value)))
                      }
                      else {
                        regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).add(LiteralValue(bitVecLiteral))
                      }
                    case _ => return s
                case _ =>
            } else if (lhs.isInstanceOf[BitVecLiteral]) {
              // should do something here
              val summation = lhs.asInstanceOf[BitVecLiteral].value + rhs.asInstanceOf[BitVecLiteral].value
              mmm.findObject(summation, "data") match
                case Some(obj: MemoryRegion) =>
                  val evaluatedResults = evaluateExpression(memAssign.rhs.value, n)
                  evaluatedResults match
                    case bitVecLiteral: BitVecLiteral =>
                      if (is_internalFunction(bitVecLiteral.value)) {
                        regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).add(LocalAddress(bitVecLiteral, getName_internalFunction(bitVecLiteral.value)))
                      } else if (is_global(bitVecLiteral.value)) {
                        regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).add(GlobalAddress(bitVecLiteral, getName_global(bitVecLiteral.value)))
                      }
                      else {
                        regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).add(LiteralValue(bitVecLiteral))
                      }
                    case _ => return s
                case _ =>
            }
          case _ =>
          s
      case localAssign: LocalAssign =>
        localAssign.rhs match
          case memLoad: MemoryLoad =>
            memLoad.index match
              case binOp: BinaryExpr =>
                if (binOp.arg1.isInstanceOf[Variable] && binOp.arg1.asInstanceOf[Variable].name.equals("R21")) {
                  print("\n")
                }
                val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
                val rhs: Expr = evaluateExpression(binOp.arg2, n)
                if (!rhs.isInstanceOf[BitVecLiteral]) {
                  println("WARNING: RHS is not BitVecLiteral and is skipped " + rhs + "\n")
                  return s
                }
                if (lhs.equals(stackPointer)) {
                  mmm.findObject(rhs.asInstanceOf[BitVecLiteral].value, "stack") match
                    case Some(obj: MemoryRegion) =>
                      s + (localAssign.lhs -> regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).toSet)
                    case _ =>
                } else if (lhs.isInstanceOf[BitVecLiteral]) {
                  val summation = lhs.asInstanceOf[BitVecLiteral].value + rhs.asInstanceOf[BitVecLiteral].value
                  mmm.findObject(summation, "data") match
                    case Some(obj: MemoryRegion) =>
                      if (is_global(summation)) {
                        val setToAdd = mutable.Set.empty[Value]
                        setToAdd.add(GlobalAddress(BitVecLiteral(summation, lhs.asInstanceOf[BitVecLiteral].size), getName_global(summation)))
                        return s + (localAssign.lhs -> regionContentMap.getOrElseUpdate(obj, setToAdd).toSet)
                      } else if (is_internalFunction(summation)) {
                        val setToAdd = mutable.Set.empty[Value]
                        setToAdd.add(LocalAddress(BitVecLiteral(summation, lhs.asInstanceOf[BitVecLiteral].size), getName_internalFunction(summation)))
                        return s + (localAssign.lhs -> regionContentMap.getOrElseUpdate(obj, setToAdd).toSet)
                      } else {
                        val setToAdd = mutable.Set.empty[Value]
                        setToAdd.add(LiteralValue(BitVecLiteral(summation, lhs.asInstanceOf[BitVecLiteral].size)))
                        return s + (localAssign.lhs -> regionContentMap.getOrElseUpdate(obj, setToAdd).toSet)
                      }
                    case _ =>
                }
              case _ =>
          case variable: Variable =>
            return s + (localAssign.lhs -> s.getOrElse(variable, Set.empty))
          case _ =>
        s
      case _ =>
        print(s"type: ${stmt.getClass} $stmt\n")
        throw new Exception("Unknown type")
    }
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
        cmd.data match {
//          case directCall: DirectCall =>
//            throw new RuntimeException("ERROR: CASE NOT HANDLED: " + directCall + "\n")
          case memAssign: MemoryAssign =>
            lattice.sublattice.lub(s, eval(memAssign, s, n))

          // local assign is just lhs assigned to rhs we only need this information to track a prior register operation
          // AKA: R1 <- R1 + 8; mem(R1) <- 0x1234
          case localAssign: LocalAssign =>
            assigmentsMap.addOne((localAssign.lhs, n) -> evaluateExpression(localAssign.rhs, n))
            lattice.sublattice.lub(s, eval(localAssign, s, n))
          case _ => s
        }
      case _ => s // ignore other kinds of nodes
    }


/** Base class for memory region analysis (non-lifted) lattice.
 */
abstract class ValueSetAnalysis(val cfg: ProgramCfg, val globals: Set[SpecGlobal], val internalFunctions: Set[InternalFunction], val globalOffsets: Map[BigInt, BigInt], val mmm: MemoryModelMap) extends FlowSensitiveAnalysis(true) with ValueSetAnalysisMisc:

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
   */
  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = localTransfer(n, s)

/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
 */
abstract class IntreprocValueSetAnalysisWorklistSolver[L <: MapLattice[Expr, PowersetLattice[Value]]](cfg: InterproceduralProgramCfg, globals: Set[SpecGlobal], internalFunctions: Set[InternalFunction], globalOffsets: Map[BigInt, BigInt], mmm: MemoryModelMap, val powersetLattice: L)
  extends ValueSetAnalysis(cfg, globals, internalFunctions, globalOffsets, mmm)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies

object ValueSetAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
   */
  class WorklistSolver(cfg: InterproceduralProgramCfg, globals: Set[SpecGlobal], internalFunctions: Set[InternalFunction], globalOffsets: Map[BigInt, BigInt], mmm: MemoryModelMap)
    extends IntreprocValueSetAnalysisWorklistSolver(cfg, globals, internalFunctions, globalOffsets, mmm, MapLattice[Expr, PowersetLattice[Value]](PowersetLattice[Value]))