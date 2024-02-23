package analysis

import ir.*
import analysis.solvers.*

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable
import util.Logger

/** Trait for program analyses.
  *
  * @tparam R
  *   the type of the analysis result
  */
trait Analysis[+R]:

  /** Performs the analysis and returns the result.
    */
  def analyze(): R

/** Base class for value analysis with simple (non-lifted) lattice.
  */
trait ConstantPropagation(val cfg: ProgramCfg) {
  /** The lattice of abstract states.
    */

  val valuelattice: ConstantPropagationLattice = ConstantPropagationLattice()

  val statelattice: MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice] = MapLattice(valuelattice)

  /** Default implementation of eval.
    */
  def eval(exp: Expr, env: Map[Variable, FlatElement[BitVecLiteral]]): FlatElement[BitVecLiteral] =
    import valuelattice._
    exp match
      case id: Variable => env(id)
      case n: BitVecLiteral => bv(n)
      case ze: ZeroExtend => zero_extend(ze.extension, eval(ze.body, env))
      case se: SignExtend => sign_extend(se.extension, eval(se.body, env))
      case e: Extract => extract(e.end, e.start, eval(e.body, env))
      case bin: BinaryExpr =>
        val left = eval(bin.arg1, env)
        val right = eval(bin.arg2, env)
        bin.op match
          case BVADD => bvadd(left, right)
          case BVSUB => bvsub(left, right)
          case BVMUL => bvmul(left, right)
          case BVUDIV => bvudiv(left, right)
          case BVSDIV => bvsdiv(left, right)
          case BVSREM => bvsrem(left, right)
          case BVUREM => bvurem(left, right)
          case BVSMOD => bvsmod(left, right)
          case BVAND => bvand(left, right)
          case BVOR => bvor(left, right)
          case BVXOR => bvxor(left, right)
          case BVNAND => bvnand(left, right)
          case BVNOR => bvnor(left, right)
          case BVXNOR => bvxnor(left, right)
          case BVSHL => bvshl(left, right)
          case BVLSHR => bvlshr(left, right)
          case BVASHR => bvashr(left, right)
          case BVCOMP => bvcomp(left, right)
          case BVCONCAT => concat(left, right)

      case un: UnaryExpr =>
        val arg = eval(un.arg, env)

        un.op match
          case BVNOT => bvnot(arg)
          case BVNEG => bvneg(arg)

      case _ => valuelattice.top

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CfgNode, s: Map[Variable, FlatElement[BitVecLiteral]]): Map[Variable, FlatElement[BitVecLiteral]] =
    n match
      case r: CfgCommandNode =>
        r.data match
          // assignments
          case la: Assign =>
            s + (la.lhs -> eval(la.rhs, s))
          // all others: like no-ops
          case _ => s
      case _ => s

  /** The analysis lattice.
    */
  val lattice: MapLattice[CfgNode, Map[Variable, FlatElement[BitVecLiteral]], MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice]] = MapLattice(statelattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CfgNode, s: Map[Variable, FlatElement[BitVecLiteral]]): Map[Variable, FlatElement[BitVecLiteral]] = localTransfer(n, s)
}

class ConstantPropagationSolver(cfg: ProgramCfg) extends ConstantPropagation(cfg)
    with SimplePushDownWorklistFixpointSolver[CfgNode, Map[Variable, FlatElement[BitVecLiteral]], MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice]]
    with IntraproceduralForwardDependencies
    with Analysis[Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]]]


trait MemoryRegionAnalysis(val cfg: ProgramCfg,
                           val globals: Map[BigInt, String],
                           val globalOffsets: Map[BigInt, BigInt],
                           val subroutines: Map[BigInt, String],
                           val constantProp: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]]) {

  var mallocCount: Int = 0
  var stackCount: Int = 0
  val stackMap: mutable.Map[CfgFunctionEntryNode, mutable.Map[Expr, StackRegion]] = mutable.Map()

  private def nextMallocCount() = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  private def nextStackCount() = {
    stackCount += 1
    s"stack_$stackCount"
  }

  /**
   * Controls the pool of stack regions. Each pool is unique to a function.
   * If the offset has already been defined in the context of the function, then the same region is returned.
   *
   * @param expr   : the offset
   * @param parent : the function entry node
   * @return the stack region corresponding to the offset
   */
  def poolMaster(expr: BitVecLiteral, parent: CfgFunctionEntryNode): StackRegion = {
    val stackPool = stackMap.getOrElseUpdate(parent, mutable.HashMap())
    if (stackPool.contains(expr)) {
      stackPool(expr)
    } else {
      val newRegion = StackRegion(nextStackCount(), expr)
      stackPool += (expr -> newRegion)
      newRegion
    }
  }

  def unwrapExpr(expr: Expr): ListBuffer[Expr] = {
    val buffers = ListBuffer[Expr]()
    expr match {
      case e: Extract => unwrapExpr(e.body)
      case e: SignExtend => unwrapExpr(e.body)
      case e: ZeroExtend => unwrapExpr(e.body)
      case repeat: Repeat => unwrapExpr(repeat.body)
      case unaryExpr: UnaryExpr => unwrapExpr(unaryExpr.arg)
      case binaryExpr: BinaryExpr =>
        unwrapExpr(binaryExpr.arg1)
        unwrapExpr(binaryExpr.arg2)
      case memoryLoad: MemoryLoad =>
        buffers.addOne(memoryLoad)
        unwrapExpr(memoryLoad.index)
      case _ =>
    }
    buffers
  }

  val regionLattice: PowersetLattice[MemoryRegion] = PowersetLattice()

  /**
    * Lifted memory region lattice, with new bottom element representing "unreachable".
    */
  val liftedLattice: LiftLattice[Set[MemoryRegion], PowersetLattice[MemoryRegion]] = LiftLattice(regionLattice)

  val lattice: MapLattice[CfgNode, LiftedElement[Set[MemoryRegion]], LiftLattice[Set[MemoryRegion], PowersetLattice[MemoryRegion]]] = MapLattice(liftedLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  val first: Set[CfgNode] = cfg.funEntries.toSet

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)

  private val mallocVariable = Register("R0", 64)

  def eval(exp: Expr, env: Set[MemoryRegion], n: CfgCommandNode): Set[MemoryRegion] = {
    Logger.debug(s"evaluating $exp")
    Logger.debug(s"env: $env")
    Logger.debug(s"n: $n")
    exp match {
      case binOp: BinaryExpr =>
        if (binOp.arg1 == stackPointer) {
          evaluateExpression(binOp.arg2, constantProp(n)) match {
            case Some(b: BitVecLiteral) => Set(poolMaster(b, n.parent))
            case None => env
          }
        } else {
          evaluateExpression(binOp, constantProp(n)) match {
            case Some(b: BitVecLiteral) => eval(b, env, n)
            case None => env
          }
        }
      case bitVecLiteral: BitVecLiteral =>
        if (globals.contains(bitVecLiteral.value)) {
          val globalName = globals(bitVecLiteral.value)
          Set(DataRegion(globalName, bitVecLiteral))
        } else if (subroutines.contains(bitVecLiteral.value)) {
          val subroutineName = subroutines(bitVecLiteral.value)
          Set(DataRegion(subroutineName, bitVecLiteral))
        } else if (globalOffsets.contains(bitVecLiteral.value)) {
          val val1 = globalOffsets(bitVecLiteral.value)
          if (subroutines.contains(val1)) {
            val globalName = subroutines(val1)
            Set(DataRegion(globalName, bitVecLiteral))
          } else {
            Set(DataRegion(s"Unknown_$bitVecLiteral", bitVecLiteral))
          }
        } else {
          //throw new Exception(s"Unknown type for $bitVecLiteral")
          // unknown region here
          Set(DataRegion(s"Unknown_$bitVecLiteral", bitVecLiteral))
        }
      case variable: Variable =>
        variable match {
          case _: LocalVar =>
            env
          case reg: Register if reg == stackPointer =>
            env
          case _ =>
            evaluateExpression(variable, constantProp(n)) match {
              case Some(b: BitVecLiteral) =>
                eval(b, env, n)
              case _ =>
                env // we cannot evaluate this to a concrete value, we need VSA for this
            }
        }
      // we cannot evaluate this to a concrete value, we need VSA for this
      case _ =>
        Logger.debug(s"type: ${exp.getClass} $exp\n")
        throw new Exception("Unknown type")
    }
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CfgNode, s: Set[MemoryRegion]): Set[MemoryRegion] = n match {
    case cmd: CfgCommandNode =>
      cmd.data match {
        case directCall: DirectCall =>
          if (directCall.target.name == "malloc") {
            evaluateExpression(mallocVariable, constantProp(n)) match {
              case Some(b: BitVecLiteral) =>
                regionLattice.lub(s, Set(HeapRegion(nextMallocCount(), b)))
              case None => s
            }
          } else {
            s
          }
        case memAssign: MemoryAssign =>
          if (ignoreRegions.contains(memAssign.value)) {
            return s
          }
          val result = eval(memAssign.index, s, cmd)
          regionLattice.lub(s, result)
        case localAssign: Assign =>
          var m = s
          unwrapExpr(localAssign.rhs).foreach {
            case memoryLoad: MemoryLoad =>
              val result = eval(memoryLoad.index, s, cmd)
              m = regionLattice.lub(m, result)
            case _ => m
          }
          m
        case _ => s
      }
    case _ => s // ignore other kinds of nodes
  }

  def transferUnlifted(n: CfgNode, s: Set[MemoryRegion]): Set[MemoryRegion] = localTransfer(n, s)
}

class MemoryRegionAnalysisSolver(
    cfg: ProgramCfg,
    globals: Map[BigInt, String],
    globalOffsets: Map[BigInt, BigInt],
    subroutines: Map[BigInt, String],
    constantProp: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]]
) extends MemoryRegionAnalysis(cfg, globals, globalOffsets, subroutines, constantProp)
    with IntraproceduralForwardDependencies
    with Analysis[Map[CfgNode, LiftedElement[Set[MemoryRegion]]]]
    with WorklistFixpointSolverWithReachability[CfgNode, Set[MemoryRegion], PowersetLattice[MemoryRegion]] {

  override def funsub(n: CfgNode, x: Map[CfgNode, LiftedElement[Set[MemoryRegion]]]): LiftedElement[Set[MemoryRegion]] = {
    n match {
      // function entry nodes are always reachable as this is intraprocedural
      case _: CfgFunctionEntryNode => liftedLattice.lift(regionLattice.bottom)
      // all other nodes are processed with join+transfer
      case _ => super.funsub(n, x)
    }
  }
}
