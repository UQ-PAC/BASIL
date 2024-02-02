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
          case la: LocalAssign =>
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

/** Base class for value analysis with simple (non-lifted) lattice.
 */
trait ConstantPropagationWithSSA(val cfg: ProgramCfg) {
  /** The lattice of abstract states.
   */

  val valuelattice: ConstantPropagationLatticeWithSSA = ConstantPropagationLatticeWithSSA()

  val statelattice: MapLattice[RegisterVariableWrapper, Set[BitVecLiteral], ConstantPropagationLatticeWithSSA] = MapLattice(valuelattice)

  /** Default implementation of eval.
   */
  def eval(exp: Expr, env: Map[RegisterVariableWrapper, Set[BitVecLiteral]]): Set[BitVecLiteral] =
    import valuelattice._
    exp match
      case id: Variable => env(RegisterVariableWrapper(id))
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

      case _ => Set.empty

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CfgNode, s: Map[RegisterVariableWrapper, Set[BitVecLiteral]]): Map[RegisterVariableWrapper, Set[BitVecLiteral]] =
    n match
      case r: CfgCommandNode =>
        r.data match
          // assignments
          case la: LocalAssign =>
            if (s.contains(RegisterVariableWrapper(la.lhs))) {
                println("Contains")
                s + (RegisterVariableWrapper(la.lhs) -> s(RegisterVariableWrapper(la.lhs)).union(eval(la.rhs, s)))
            } else {
              s + (RegisterVariableWrapper(la.lhs) -> eval(la.rhs, s))
            }
          // all others: like no-ops
          case _ => s
      case _ => s

  /** The analysis lattice.
   */
  val lattice: MapLattice[CfgNode, Map[RegisterVariableWrapper, Set[BitVecLiteral]], MapLattice[RegisterVariableWrapper, Set[BitVecLiteral], ConstantPropagationLatticeWithSSA]] = MapLattice(statelattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
   */
  def transfer(n: CfgNode, s: Map[RegisterVariableWrapper, Set[BitVecLiteral]]): Map[RegisterVariableWrapper, Set[BitVecLiteral]] = localTransfer(n, s)
}

class ConstantPropagationSolverWithSSA(cfg: ProgramCfg) extends ConstantPropagationWithSSA(cfg)
  with SimplePushDownWorklistFixpointSolver[CfgNode, Map[RegisterVariableWrapper, Set[BitVecLiteral]], MapLattice[RegisterVariableWrapper, Set[BitVecLiteral], ConstantPropagationLatticeWithSSA]]
  with IntraproceduralForwardDependencies
  with Analysis[Map[CfgNode, Map[RegisterVariableWrapper, Set[BitVecLiteral]]]]

trait MemoryRegionAnalysis(val cfg: ProgramCfg,
                           val globals: Map[BigInt, String],
                           val globalOffsets: Map[BigInt, BigInt],
                           val subroutines: Map[BigInt, String],
                           val constantProp: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]],
                           val ANRResult: Map[CfgNode, Set[Variable]],
                           val RNAResult: Map[CfgNode, Set[Variable]],
                           val RegToResult: Map[CfgNode, Map[RegisterVariableWrapper, FlatElement[Expr]]]) {

  var mallocCount: Int = 0
  var stackCount: Int = 0
  val stackMap: mutable.Map[Procedure, mutable.Map[Expr, StackRegion]] = mutable.Map()

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
  def poolMaster(expr: BitVecLiteral, stackBase: Procedure): StackRegion = {
    val stackPool = stackMap.getOrElseUpdate(stackBase, mutable.HashMap())
    if (stackPool.contains(expr)) {
      stackPool(expr)
    } else {
      val newRegion = StackRegion(nextStackCount(), expr, stackBase)
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

  def resolveGlobalOffset(address: BitVecLiteral): DataRegion = {
    val tableAddress = globalOffsets.getOrElse(address.value, address.value)
    var name = "@ERROR"
    if (globals.contains(tableAddress)) {
      name = globals(tableAddress)
    } else if (subroutines.contains(tableAddress)) {
      name = subroutines(tableAddress)
    }

    DataRegion(name, address)
  }

  def stackDetection(stmt: Statement): Unit = {
    println("Stack detection")
    println(spList)
    stmt match {
      case localAssign: LocalAssign =>
        if (spList.contains(localAssign.rhs)) {
          // add lhs to spList
          spList.addOne(localAssign.lhs)
        } else {
          // remove lhs from spList
          if spList.contains(localAssign.lhs) && localAssign.lhs != stackPointer then // TODO: This is a hack: it should check for stack ptr using the wrapper
            spList.remove(spList.indexOf(localAssign.lhs))
        }
      // TODO: should handle the store case (last case)
      case _ =>
    }
  }

  val regionLattice: PowersetLattice[MemoryRegion] = PowersetLattice()

  /**
    * Lifted memory region lattice, with new bottom element representing "unreachable".
    */
  val liftedLattice: LiftLattice[Set[MemoryRegion], PowersetLattice[MemoryRegion]] = LiftLattice(regionLattice)

  val lattice: MapLattice[CfgNode, LiftedElement[Set[MemoryRegion]], LiftLattice[Set[MemoryRegion], PowersetLattice[MemoryRegion]]] = MapLattice(liftedLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  val first: Set[CfgNode] = cfg.funEntries.toSet

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))
  private val mallocVariable = Register("R0", BitVecType(64))
  private val spList = ListBuffer[Expr](stackPointer)
  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)
  val registerToRegions: mutable.Map[RegisterVariableWrapper, mutable.Set[MemoryRegion]] = mutable.Map()
  val procedureToSharedRegions: mutable.Map[Procedure, mutable.Set[MemoryRegion]] = mutable.Map()

  def eval(exp: Expr, env: Set[MemoryRegion], n: CfgCommandNode): Set[MemoryRegion] = {
    Logger.debug(s"evaluating $exp")
    Logger.debug(s"env: $env")
    Logger.debug(s"n: $n")
    exp match {
      case binOp: BinaryExpr =>
        if (spList.contains(binOp.arg1)) {
          evaluateExpression(binOp.arg2, constantProp(n)) match {
            case Some(b: BitVecLiteral) => Set(poolMaster(b, n.parent.data))
            case None => env
          }
        } else {
          evaluateExpression(binOp, constantProp(n)) match {
            case Some(b: BitVecLiteral) => eval(b, env, n)
            case None => env
          }
        }
      case bitVecLiteral: BitVecLiteral =>
          Set(resolveGlobalOffset(bitVecLiteral))
      case variable: Variable =>
        variable match {
          case _: LocalVar =>
            env
          case reg: Register if spList.contains(reg) =>
            env
          case _ =>
            evaluateExpression(variable, constantProp(n)) match {
              case Some(b: BitVecLiteral) =>
                eval(b, env, n)
              case _ =>
                env // we cannot evaluate this to a concrete value, we need VSA for this
            }
        }
      case memoryLoad: MemoryLoad =>
        eval(memoryLoad.index, env, n)
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
          val ANR = ANRResult(n)
          val RNA = RNAResult(cfg.funEntries.filter(fn => fn.data == directCall.target).head)
          val parameters = RNA.intersect(ANR)
          val ctx = RegToResult(n)
          for (elem <- parameters) {
            if (ctx.contains(RegisterVariableWrapper(elem))) {
              ctx(RegisterVariableWrapper(elem)) match {
                case FlatEl(al) =>
                  val regions = eval(al, s, cmd)
                  //val targetMap = stackMap(directCall.target)
                  //cfg.funEntries.filter(fn => fn.data == directCall.target).head
                  procedureToSharedRegions.getOrElseUpdate(directCall.target, mutable.Set.empty).addAll(regions)
                  registerToRegions.getOrElseUpdate(RegisterVariableWrapper(elem), mutable.Set.empty).addAll(regions)
              }
            }
          }
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
          if (ignoreRegions.contains(memAssign.rhs.value)) {
            return s
          }
          val result = eval(memAssign.rhs.index, s, cmd)
          regionLattice.lub(s, result)
        case localAssign: LocalAssign =>
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
    constantProp: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]],
    ANRResult: Map[CfgNode, Set[Variable]],
    RNAResult: Map[CfgNode, Set[Variable]],
    RegToResult: Map[CfgNode, Map[RegisterVariableWrapper, FlatElement[Expr]]]
) extends MemoryRegionAnalysis(cfg, globals, globalOffsets, subroutines, constantProp, ANRResult, RNAResult, RegToResult)
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
