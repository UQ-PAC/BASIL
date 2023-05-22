package analysis

import ir.{DirectCall, *}
import analysis.solvers.*
import boogie.BExpr
import specification.{InternalFunction, SpecGlobal}

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable

/** ValueSets are PowerSet of possible values */

abstract class Value
class AddressValue(expr: Expr) extends Value:
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Address(")
    sb.append(expr)
    sb.append(")")
    sb.toString()
  }

case class GlobalAddress(expr: Expr) extends AddressValue(expr):
  override def toString: String = {
    val sb = new StringBuilder
    sb.append("GlobalAddress(")
    sb.append(expr)
    sb.append(")")
    sb.toString()
  }
case class LocalAddress(expr: Expr) extends AddressValue(expr):
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

class ValueSet(expr: Expr, values: mutable.Set[Value]) {
  override def toString: String = {
    val sb = new StringBuilder
    sb.append(expr)
    sb.append(" -> {")
    sb.append(values.mkString(", "))
    sb.append("}")
    sb.toString()
  }
}


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
  val powersetLattice: PowersetLattice[ValueSet]

  /** The lattice of abstract states.
   */
  val lattice: MapLattice[CfgNode, PowersetLattice[ValueSet]] = MapLattice(powersetLattice)

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
            case _ =>
          }
        case _ =>
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

  def is_internalFunction(bigInt: BigInt): Boolean = {
    for (internalFunction <- internalFunctions) {
      if (internalFunction.offset == bigInt) {
        return true
      }
    }
    false
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
                      val calculated: BigInt = bitVecLiteral.value.+(binOp.arg2.asInstanceOf[BitVecLiteral].value)
                      return BitVecLiteral(calculated, bitVecLiteral.size)
                    case _ => evaluateExpression(value, pred)
                case _ =>
                  print("ERROR: CASE NOT HANDLED: " + assigmentsMap.get(variable, pred) + " FOR " + binOp + "\n")
            }
          case _ => return exp
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
        for (pred <- findDecl(variable, n)) {
          assigmentsMap(variable, pred) match
            case bitVecLiteral: BitVecLiteral =>
              return bitVecLiteral
            case any: Expr => return evaluateExpression(any, n)
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
      case bitVecLiteral: BitVecLiteral =>
        mmm.findObject(bitVecLiteral.value, "stack") match
          case Some(obj: MemoryRegion) =>
            return Some(obj)
          case _ =>
            return None
      case binOp: BinaryExpr =>
        val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
        val rhs: Expr = evaluateExpression(binOp.arg2, n)
        if (lhs.equals(stackPointer)) {
          mmm.findObject(rhs.asInstanceOf[BitVecLiteral].value, "stack") match
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
  def eval(exp: Expr, env: lattice.sublattice.Element, n: CfgNode): lattice.sublattice.Element = {
    exp match {
      case binOp: BinaryExpr =>
        val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
        val rhs: Expr = evaluateExpression(binOp.arg2, n)

        env

      case zeroExtend: ZeroExtend =>
        eval(zeroExtend.body, env, n)
      case memoryLoad: MemoryLoad =>
        eval(memoryLoad.index, env, n)
      case variable: Variable =>
        lattice.sublattice.bottom
      case extract: Extract =>
        eval(extract.body, env, n)
      case unaryExpr: UnaryExpr =>
        lattice.sublattice.bottom
      case signExtend: SignExtend =>
        eval(signExtend.body, env, n)
      case bitVecLiteral: BitVecLiteral =>
        print(s"Saw a bit vector literal ${bitVecLiteral}\n")
        lattice.sublattice.bottom
      case _ =>
        print(s"type: ${exp.getClass} $exp\n")
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
          case directCall: DirectCall =>
            throw new RuntimeException("ERROR: CASE NOT HANDLED: " + directCall + "\n")
          case memAssign: MemoryAssign =>
            if (ignoreRegions.contains(memAssign.rhs.value)) {
              return s
            }
            lattice.sublattice.lub(s, eval(memAssign.rhs.value, s, n))
            memAssign.rhs.index match
              case binOp: BinaryExpr =>
                val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
                val rhs: Expr = evaluateExpression(binOp.arg2, n)
                if (lhs.equals(stackPointer)) {
                  mmm.findObject(rhs.asInstanceOf[BitVecLiteral].value, "stack") match
                    case Some(obj: MemoryRegion) =>
                      val evaluatedResults = evaluateExpression(memAssign.rhs.value, n)
                      evaluatedResults match
                        case bitVecLiteral: BitVecLiteral =>
                          print(s"Found a bit vector literal ${bitVecLiteral}\n")
                          if (is_internalFunction(bitVecLiteral.value)) {
                            regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).add(LocalAddress(bitVecLiteral))
                          } else if (is_global(bitVecLiteral.value)) {
                            regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).add(GlobalAddress(bitVecLiteral))
                          }
                          else {
                            regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).add(LiteralValue(bitVecLiteral))
                          }
                        case _ =>
                          val region = exprToRegion(evaluatedResults, n)
                          region match {
                            case Some(obj: MemoryRegion) =>
                                regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).addAll(regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]))
                            case _ =>
                              throw new RuntimeException("Not a value type: " + evaluatedResults + "\n")
                          }
                    case _ =>
                }
                s

          // local assign is just lhs assigned to rhs we only need this information to track a prior register operation
          // AKA: R1 <- R1 + 8; mem(R1) <- 0x1234
          case localAssign: LocalAssign =>
            assigmentsMap.addOne((localAssign.lhs, n) -> evaluateExpression(localAssign.rhs, n))
            localAssign.rhs match
              case memLoad: MemoryLoad =>
                memLoad.index match
                  case binOp: BinaryExpr =>
                    val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
                    val rhs: Expr = evaluateExpression(binOp.arg2, n)
                    if (lhs.equals(stackPointer)) {
                      mmm.findObject(rhs.asInstanceOf[BitVecLiteral].value, "stack") match
                        case Some(obj: MemoryRegion) =>

                          return Set(ValueSet(localAssign.lhs, regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value])))
                        case _ =>
                    } else if (lhs.isInstanceOf[BitVecLiteral]) {
                      val summation = lhs.asInstanceOf[BitVecLiteral].value + rhs.asInstanceOf[BitVecLiteral].value
                      mmm.findObject(summation, "data") match
                        case Some(obj: MemoryRegion) =>
                          if (is_global(summation)) {
                            val setToAdd = mutable.Set.empty[Value]
                            setToAdd.add(GlobalAddress(BitVecLiteral(summation, lhs.asInstanceOf[BitVecLiteral].size)))
                            return Set(ValueSet(localAssign.lhs, regionContentMap.getOrElseUpdate(obj, setToAdd)))
                          } else if (is_internalFunction(summation)) {
                            val setToAdd = mutable.Set.empty[Value]
                            setToAdd.add(LocalAddress(BitVecLiteral(summation, lhs.asInstanceOf[BitVecLiteral].size)))
                            return Set(ValueSet(localAssign.lhs, regionContentMap.getOrElseUpdate(obj, setToAdd)))
                          } else {
                            val setToAdd = mutable.Set.empty[Value]
                            setToAdd.add(LiteralValue(BitVecLiteral(summation, lhs.asInstanceOf[BitVecLiteral].size)))
                            return Set(ValueSet(localAssign.lhs, regionContentMap.getOrElseUpdate(obj, setToAdd)))
                          }
                        case _ =>
                    }
//                    } else if (lhs.isInstanceOf[BitVecLiteral]) {
//                      val summation = lhs.asInstanceOf[BitVecLiteral].value + rhs.asInstanceOf[BitVecLiteral].value
//                      if (is_global(summation)) {
//                        return lattice.sublattice.lub(s, Set(ValueSet(localAssign.lhs, mutable.Set(GlobalAddress(BitVecLiteral(summation, lhs.asInstanceOf[BitVecLiteral].size))))))
//                      } else if (is_internalFunction(summation)) {
//                        return lattice.sublattice.lub(s, Set(ValueSet(localAssign.lhs, mutable.Set(LocalAddress(BitVecLiteral(summation, lhs.asInstanceOf[BitVecLiteral].size))))))
//                      } else {
//                        return lattice.sublattice.lub(s, Set(ValueSet(localAssign.lhs, mutable.Set(LiteralValue(BitVecLiteral(summation, lhs.asInstanceOf[BitVecLiteral].size))))))
//                      }
//                    }
                  case _ =>
              case _ =>
            s
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
abstract class IntreprocValueSetAnalysisWorklistSolver[L <: PowersetLattice[ValueSet]](cfg: InterproceduralProgramCfg, globals: Set[SpecGlobal], internalFunctions: Set[InternalFunction], globalOffsets: Map[BigInt, BigInt], mmm: MemoryModelMap, val powersetLattice: L)
  extends ValueSetAnalysis(cfg, globals, internalFunctions, globalOffsets, mmm)
    with SimpleMonotonicSolver[CfgNode]
    with ForwardDependencies

object ValueSetAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
   */
  class WorklistSolver(cfg: InterproceduralProgramCfg, globals: Set[SpecGlobal], internalFunctions: Set[InternalFunction], globalOffsets: Map[BigInt, BigInt], mmm: MemoryModelMap)
    extends IntreprocValueSetAnalysisWorklistSolver(cfg, globals, internalFunctions, globalOffsets, mmm, PowersetLattice[ValueSet])