package analysis

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable

import analysis.lattices._
import analysis.solvers._
import ir._

trait MemoryRegionAnalysisMisc:

  val assigmentsMap: mutable.HashMap[(Expr, CfgNode), Expr] = mutable.HashMap.empty

  var mallocCount: Int = 0
  var stackCount: Int = 0
  var stackPool: mutable.Map[Expr, StackRegion] = mutable.HashMap()
  private def getNextMallocCount(): String = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  private def getNextStackCount(): String = {
    stackCount += 1
    s"stack_$stackCount"
  }

  def poolMaster(expr: Expr): StackRegion = {
    if (stackPool.contains(expr)) {
      stackPool(expr)
    } else {
      val newRegion = StackRegion(getNextStackCount(), expr)
      stackPool += (expr -> newRegion)
      newRegion
    }
  }

  val cfg: ProgramCfg
  val globals: Map[BigInt, String]
  val globalOffsets: Map[BigInt, BigInt]
  val subroutines: Map[BigInt, String]

  /** The lattice of abstract values.
   */
  val powersetLattice: PowersetLattice[MemoryRegion]

  /** The lattice of abstract states.
   */
  val lattice: MapLattice[CfgNode, PowersetLattice[MemoryRegion]] = MapLattice(powersetLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

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
    for (pred <- n.pred(intra = false)) {
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

  /**
   * Evaluate an expression in a hope of finding a global variable.
   * @param exp: The expression to evaluate (e.g. R1 + 0x1234)
   * @param n: The node where the expression is evaluated (e.g. mem[R1 + 0x1234] <- ...)
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
              case any:Expr => return evaluateExpression(any, n)
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
            lhs match {
              case bitVecLiteral: BitVecLiteral =>
                if (globals.contains(bitVecLiteral.value)) {
                  var tempLattice: lattice.sublattice.Element = env
                  val globalName = globals(bitVecLiteral.value)
                  tempLattice = lattice.sublattice.lub(tempLattice, Set(DataRegion(globalName, bitVecLiteral)))
                  return lattice.sublattice.lub(tempLattice, Set(RegionAccess(globalName, binOp.arg2)))
                } else if (subroutines.contains(bitVecLiteral.value)) {
                  var tempLattice: lattice.sublattice.Element = env
                  val subroutineName = subroutines(bitVecLiteral.value)
                  tempLattice = lattice.sublattice.lub(tempLattice, Set(DataRegion(subroutineName, bitVecLiteral)))
                  return lattice.sublattice.lub(tempLattice, Set(RegionAccess(subroutineName, binOp.arg2)))
                }
              case binOp2: BinaryExpr =>
                  // special case: we do not want to get a unique stack name so we try to find it in the pool
                  print("Warning: fragile code! Assumes array by default due to double binary operation\n")
                  var tempLattice: lattice.sublattice.Element = env
                  tempLattice = lattice.sublattice.lub(tempLattice, Set(poolMaster(binOp2.arg2)))
                  return lattice.sublattice.lub(tempLattice, Set(RegionAccess(poolMaster(binOp2.arg2).regionIdentifier, rhs)))
              case _ =>
            }
          Set(StackRegion(getNextStackCount(), binOp.arg2))

        case zeroExtend: ZeroExtend =>
          eval(zeroExtend.body, env, n)
        case memoryLoad: MemoryLoad => // TODO: Pointer access here
          //eval(memoryLoad.index, memType, env)
          lattice.sublattice.bottom
        case variable: Variable =>
          val eval = evaluateExpression(variable, n)
          eval match {
            case literal: BitVecLiteral =>
              if (globals.contains(literal.value)) {
                return Set(DataRegion(globals(literal.value), literal))
              } else if (subroutines.contains(literal.value)) {
                return Set(DataRegion(subroutines(literal.value), literal))
              }
              lattice.sublattice.bottom
            case _ =>
              lattice.sublattice.bottom
          }
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
            if (directCall.target.name == "malloc") {
              val decl = findDecl(mallocVariable, n).headOption
              val recentMallocSize = assigmentsMap.get(mallocVariable, decl.get).get
              return lattice.sublattice.lub(s, Set(HeapRegion(getNextMallocCount(), recentMallocSize)))
            }
            s
          case memAssign: MemoryAssign =>
            if (ignoreRegions.contains(memAssign.rhs.value)) {
              return s
            }
            lattice.sublattice.lub(s, eval(memAssign.rhs.index, s, n))
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
abstract class MemoryRegionAnalysis(val cfg: ProgramCfg, val globals: Map[BigInt, String], val globalOffsets: Map[BigInt, BigInt], val subroutines: Map[BigInt, String]) extends FlowSensitiveAnalysis(true) with MemoryRegionAnalysisMisc:

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
   */
  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = localTransfer(n, s)

/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
 */
abstract class IntraprocMemoryRegionAnalysisWorklistSolver[L <: PowersetLattice[MemoryRegion]](cfg: ProgramCfg, globals: Map[BigInt, String], globalOffsets: Map[BigInt, BigInt], subroutines: Map[BigInt, String], val powersetLattice: L)
  extends MemoryRegionAnalysis(cfg, globals, globalOffsets, subroutines)
  with SimpleMonotonicSolver[CfgNode]
  with ForwardDependencies

object MemoryRegionAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
   */
  class WorklistSolver(cfg: ProgramCfg, globals: Map[BigInt, String], globalOffsets: Map[BigInt, BigInt], subroutines: Map[BigInt, String])
    extends IntraprocMemoryRegionAnalysisWorklistSolver(cfg, globals, globalOffsets, subroutines, PowersetLattice[MemoryRegion])








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
  val assigmentsMap: mutable.HashMap[(Expr, CfgNode), Expr] = mutable.HashMap.empty
  val regionContentMap: mutable.HashMap[MemoryRegion, mutable.Set[Value]] = mutable.HashMap.empty

  val cfg: ProgramCfg
  val globals: Map[BigInt, String]
  val externalFunctions: Map[BigInt, String]
  val globalOffsets: Map[BigInt, BigInt]
  val subroutines: Map[BigInt, String]
  val mmm: MemoryModelMap

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
    for (pred <- n.pred(intra = false)) {
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
              assigmentsMap.get(variable, pred) match {
                case Some(value) =>
                  value match {
                    case bitVecLiteral: BitVecLiteral =>
                      binOp.op match {
                        case BVADD =>
                          binOp.arg2 match {
                            case arg2: BitVecLiteral =>
                              return BitVecLiteral(bitVecLiteral.value + arg2.value, bitVecLiteral.size)
                            case _ => return exp
                          }
                        case _ =>
                          println("ERROR: OPERATOR " + binOp.op + " NOT HANDLED: " + binOp)
                          return exp
                      }
                    case _ => return exp
                  }
                case _ =>
                  println("ERROR: CASE NOT HANDLED: " + assigmentsMap.get(variable, pred) + " FOR " + binOp)
                  return exp
              }
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
            rhs match {
              case rhs: BitVecLiteral =>
                lhs match {
                  case lhs: Variable if lhs.equals(stackPointer) =>
                    mmm.findObject(rhs.value, "stack") match {
                      case Some(obj: MemoryRegion) =>
                        evaluateExpression(memAssign.rhs.value, n) match {
                          case bitVecLiteral: BitVecLiteral =>
                            val map = regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value])
                            if (externalFunctions.contains(bitVecLiteral.value)) {
                              map.add(LocalAddress(bitVecLiteral, externalFunctions(bitVecLiteral.value)))
                            } else if (globals.contains(bitVecLiteral.value)) {
                              map.add(GlobalAddress(bitVecLiteral, globals(bitVecLiteral.value)))
                            } else if (globalOffsets.contains(bitVecLiteral.value)) {
                              map.add(GlobalAddress(bitVecLiteral, resolveGlobalOffset(bitVecLiteral.value)))
                            } else if (subroutines.contains(bitVecLiteral.value)) {
                              map.add(GlobalAddress(bitVecLiteral, subroutines(bitVecLiteral.value)))
                            } else {
                              map.add(LiteralValue(bitVecLiteral))
                            }
                            s
                          case _ => s
                        }
                      case _ => s
                    }
                  case lhs: BitVecLiteral =>
                    binOp.op match {
                      case BVADD =>
                        // should do something here
                        val summation = lhs.value + rhs.value
                        mmm.findObject(summation, "data") match {
                          case Some(obj: MemoryRegion) =>
                            evaluateExpression(memAssign.rhs.value, n) match {
                              case bitVecLiteral: BitVecLiteral =>
                                val map = regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value])
                                if (externalFunctions.contains(bitVecLiteral.value)) {
                                  map.add(LocalAddress(bitVecLiteral, externalFunctions(bitVecLiteral.value)))
                                } else if (globals.contains(bitVecLiteral.value)) {
                                  map.add(GlobalAddress(bitVecLiteral, globals(bitVecLiteral.value)))
                                } else if (globalOffsets.contains(bitVecLiteral.value)) {
                                  map.add(GlobalAddress(bitVecLiteral, resolveGlobalOffset(bitVecLiteral.value)))
                                } else if (subroutines.contains(bitVecLiteral.value)) {
                                  map.add(GlobalAddress(bitVecLiteral, subroutines(bitVecLiteral.value)))
                                } else {
                                  map.add(LiteralValue(bitVecLiteral))
                                }
                                s
                              case _ => s
                            }
                          case _ => s
                        }
                      case _ =>
                        println("ERROR: tried to eval operator: " + binOp.op + " in " + binOp)
                        s
                    }
                  case _ => s
                }
              case _ =>
                println("WARNING: RHS is not BitVecLiteral and is skipped " + rhs)
                s
            }
          case _ => s
      case localAssign: LocalAssign =>
        localAssign.rhs match
          case memLoad: MemoryLoad =>
            memLoad.index match
              case binOp: BinaryExpr =>
                evaluateExpression(binOp.arg2, n) match {
                  case rhs: BitVecLiteral =>
                    val lhs = if binOp.arg1.equals(stackPointer) then binOp.arg1 else evaluateExpression(binOp.arg1, n)
                    lhs match {
                      case lhs: Variable if lhs.equals(stackPointer) =>
                        mmm.findObject(rhs.value, "stack") match
                          case Some(obj: MemoryRegion) =>
                            s + (localAssign.lhs -> regionContentMap.getOrElseUpdate(obj, mutable.Set.empty[Value]).toSet)
                          case _ => s
                      case lhs: BitVecLiteral =>
                        binOp.op match {
                          case BVADD =>
                            val summation = lhs.value + rhs.value
                            mmm.findObject(summation, "data") match {
                              case Some(obj: MemoryRegion) =>
                                val setToAdd = mutable.Set.empty[Value]
                                if (globals.contains(summation)) {
                                  setToAdd.add(GlobalAddress(BitVecLiteral(summation, lhs.size), globals(summation)))
                                } else if (globalOffsets.contains(summation)) {
                                  setToAdd.add(GlobalAddress(BitVecLiteral(summation, lhs.size), resolveGlobalOffset(summation)))
                                } else if (subroutines.contains(summation)) {
                                  setToAdd.add(GlobalAddress(BitVecLiteral(summation, lhs.size), subroutines(summation)))
                                } else if (externalFunctions.contains(summation)) {
                                  setToAdd.add(LocalAddress(BitVecLiteral(summation, lhs.size), externalFunctions(summation)))
                                } else {
                                  setToAdd.add(LiteralValue(BitVecLiteral(summation, lhs.size)))
                                }
                                s + (localAssign.lhs -> regionContentMap.getOrElseUpdate(obj, setToAdd).toSet)
                              case _ => s
                            }
                          case _ =>
                            println("ERROR: tried to eval operator: " + binOp.op + " in " + binOp)
                            s
                        }
                      case _ => s
                    }
                  case rhs: _ =>
                    println("WARNING: RHS is not BitVecLiteral and is skipped " + rhs)
                    s
                }
              case _ => s
          case variable: Variable =>
            s + (localAssign.lhs -> s.getOrElse(variable, Set.empty))
          case _ => s
      case _ =>
        println(s"type: ${stmt.getClass} $stmt")
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
