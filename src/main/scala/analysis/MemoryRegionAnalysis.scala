package analysis

import analysis.solvers.WorklistFixpointSolverWithReachability
import ir.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait MemoryRegionAnalysis(val cfg: ProgramCfg,
                           val globals: Map[BigInt, String],
                           val globalOffsets: Map[BigInt, BigInt],
                           val subroutines: Map[BigInt, String],
                           val constantProp: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]],
                           val ANRResult: Map[CfgNode, Set[Variable]],
                           val RNAResult: Map[CfgNode, Set[Variable]],
                           val regionAccesses: Map[CfgNode, Map[RegisterVariableWrapper, FlatElement[Expr]]]) {

  var mallocCount: Int = 0
  private var stackCount: Int = 0
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
  private def poolMaster(expr: BitVecLiteral, stackBase: Procedure): StackRegion = {
    val stackPool = stackMap.getOrElseUpdate(stackBase, mutable.HashMap())
    if (stackPool.contains(expr)) {
      stackPool(expr)
    } else {
      val newRegion = StackRegion(nextStackCount(), expr, stackBase)
      stackPool += (expr -> newRegion)
      newRegion
    }
  }

  private def stackDetection(stmt: Statement): Unit = {
    Logger.debug("Stack detection")
    Logger.debug(spList)
    stmt match {
      case assign: Assign =>
        if (spList.contains(assign.rhs)) {
          // add lhs to spList
          spList.addOne(assign.lhs)
        } else {
          // remove lhs from spList
          if spList.contains(assign.lhs) && assign.lhs != stackPointer then // TODO: This is a hack: it should check for stack ptr using the wrapper
            spList.remove(spList.indexOf(assign.lhs))
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

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)
  private val mallocVariable = Register("R0", 64)
  private val spList = ListBuffer[Expr](stackPointer)
  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)
  // TODO: this could be used instead of regionAccesses in other analyses to reduce the Expr to region conversion
  private val registerToRegions: mutable.Map[RegisterVariableWrapper, mutable.Set[MemoryRegion]] = mutable.Map()
  val procedureToSharedRegions: mutable.Map[Procedure, mutable.Set[MemoryRegion]] = mutable.Map()

  def reducibleToRegion(binExpr: BinaryExpr, n: CfgCommandNode): Set[MemoryRegion] = {
    var reducedRegions = Set.empty[MemoryRegion]
    binExpr.arg1 match {
      case variable: Variable =>
        val reg = RegisterVariableWrapper(variable)
        val ctx = regionAccesses(n)
        if (ctx.contains(reg)) {
          ctx(reg) match {
            case FlatEl(al) =>
              val regions = al match {
                case memoryLoad: MemoryLoad =>
                  eval(memoryLoad.index, Set.empty, n)
                case _ =>
                  eval(al, Set.empty, n)
              }
              evaluateExpression(binExpr.arg2, constantProp(n)) match {
                case Some(b: BitVecLiteral) =>
                  regions.foreach {
                    case stackRegion: StackRegion =>
                      val nextOffset = BinaryExpr(BVADD, stackRegion.start, b)
                      evaluateExpression(nextOffset, constantProp(n)) match {
                        case Some(b2: BitVecLiteral) =>
                          reducedRegions = reducedRegions + poolMaster(b2, n.parent.data)
                        case None =>
                      }
                    case _ =>
                  }
                case None =>
              }
          }
        }
      case _ =>
    }
    reducedRegions
  }

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
        } else if (reducibleToRegion(binOp, n).nonEmpty) {
          reducibleToRegion(binOp, n)
        } else {
          evaluateExpression(binOp, constantProp(n)) match {
            case Some(b: BitVecLiteral) => eval(b, env, n)
            case None => env
          }
        }
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
      // ignore case where it could be a global region (loaded later in MMM from relf)
      case _: BitVecLiteral =>
        env
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
          val ctx = regionAccesses(n)
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
              case Some(b: BitVecLiteral) => regionLattice.lub(s, Set(HeapRegion(nextMallocCount(), b)))
              case None => s
            }
          } else {
            s
          }
        case memAssign: MemoryAssign =>
          if (ignoreRegions.contains(memAssign.value)) {
            s
          } else {
            val result = eval(memAssign.index, s, cmd)
            regionLattice.lub(s, result)
          }
        case assign: Assign =>
          stackDetection(assign)
          var m = s
          unwrapExpr(assign.rhs).foreach {
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
    regionAccesses: Map[CfgNode, Map[RegisterVariableWrapper, FlatElement[Expr]]]
  ) extends MemoryRegionAnalysis(cfg, globals, globalOffsets, subroutines, constantProp, ANRResult, RNAResult, regionAccesses)
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