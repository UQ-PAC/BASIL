package analysis

import ir.{DirectCall, *}
import analysis.solvers.*
import boogie.BExpr
import specification.SpecGlobal

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable

/** ValueSets are PowerSet of possible values */
//class ValueSet extends PowersetLattice[Expr] {
//}


trait ValueSetAnalysisMisc:
  val assigmentsMap: mutable.HashMap[(Expr, CfgNode), Expr] = mutable.HashMap.empty

  val cfg: ProgramCfg
  val globals: Set[SpecGlobal]
  val mmm: MemoryModelMap

  /** The lattice of abstract values.
   */
  val powersetLattice: PowersetLattice[Expr]

  /** The lattice of abstract states.
   */
  val lattice: MapLattice[CfgNode, PowersetLattice[Expr]] = MapLattice(powersetLattice)

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
      case _ =>
        //throw new RuntimeException("ERROR: CASE NOT HANDLED: " + exp + "\n")
        exp
    }
  }


  /** Default implementation of eval.
   */
  def eval(exp: Expr, env: lattice.sublattice.Element, n: CfgNode): lattice.sublattice.Element = {
    exp match {
      case binOp: BinaryExpr =>
        val lhs: Expr = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
        val rhs: Expr = evaluateExpression(binOp.arg2, n)
        if (lhs.equals(stackPointer)) {
          //mmm.findObject(rhs, StackRegion("", rhs))
        }
        env

      case zeroExtend: ZeroExtend =>
        eval(zeroExtend.body, env, n)
      case memoryLoad: MemoryLoad => // TODO: Pointer access here
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
      case cmd: CfgCommandNode =>
        cmd.data match {
          case directCall: DirectCall =>
            throw new RuntimeException("ERROR: CASE NOT HANDLED: " + directCall + "\n")
          case memAssign: MemoryAssign =>
            if (ignoreRegions.contains(memAssign.rhs.value)) {
              return s
            }
            lattice.sublattice.lub(s, eval(memAssign.rhs.value, s, n))

          // local assign is just lhs assigned to rhs we only need this information to track a prior register operation
          // AKA: R1 <- R1 + 8; mem(R1) <- 0x1234
          case localAssign: LocalAssign =>
            assigmentsMap.addOne((localAssign.lhs, n) -> evaluateExpression(localAssign.rhs, n))
            s
          case _ => s
        }
      case _ => s // ignore other kinds of nodes
    }


/** Base class for memory region analysis (non-lifted) lattice.
 */
abstract class ValueSetAnalysis(val cfg: ProgramCfg, val globals: Set[SpecGlobal], val mmm: MemoryModelMap) extends FlowSensitiveAnalysis(true) with ValueSetAnalysisMisc:

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
   */
  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = localTransfer(n, s)

/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
 */
abstract class IntreprocValueSetAnalysisWorklistSolver[L <: PowersetLattice[Expr]](cfg: InterproceduralProgramCfg, globals: Set[SpecGlobal], mmm: MemoryModelMap, val powersetLattice: L)
  extends ValueSetAnalysis(cfg, globals, mmm)
    with SimpleMonotonicSolver[CfgNode]
    with ForwardDependencies

object ValueSetAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
   */
  class WorklistSolver(cfg: InterproceduralProgramCfg, globals: Set[SpecGlobal], mmm: MemoryModelMap)
    extends IntreprocValueSetAnalysisWorklistSolver(cfg, globals, mmm, PowersetLattice[Expr])