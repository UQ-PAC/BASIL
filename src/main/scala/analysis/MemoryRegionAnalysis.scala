package analysis

import analysis.solvers.WorklistFixpointSolverWithReachability
import ir.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait MemoryRegionAnalysis(val program: Program,
                           val globals: Map[BigInt, String],
                           val globalOffsets: Map[BigInt, BigInt],
                           val subroutines: Map[BigInt, String],
                           val constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                           val ANRResult: Map[CFGPosition, Set[Variable]],
                           val RNAResult: Map[CFGPosition, Set[Variable]],
                           val regionAccesses: Map[CFGPosition, Map[RegisterVariableWrapper, FlatElement[Expr]]],
                           reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]) {

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

  val lattice: MapLattice[CFGPosition, LiftedElement[Set[MemoryRegion]], LiftLattice[Set[MemoryRegion], PowersetLattice[MemoryRegion]]] = MapLattice(liftedLattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  val first: Set[CFGPosition] = Set.empty ++ program.procedures

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)
  private val mallocVariable = Register("R0", 64)
  private val spList = ListBuffer[Expr](stackPointer)
  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)
  // TODO: this could be used instead of regionAccesses in other analyses to reduce the Expr to region conversion
  private val registerToRegions: mutable.Map[RegisterVariableWrapper, mutable.Set[MemoryRegion]] = mutable.Map()
  val procedureToSharedRegions: mutable.Map[Procedure, mutable.Set[MemoryRegion]] = mutable.Map()

  def reducibleToRegion(binExpr: BinaryExpr, n: Command): Set[MemoryRegion] = {
    var reducedRegions = Set.empty[MemoryRegion]
    binExpr.arg1 match {
      case variable: Variable =>
        val ctx = getUse(variable, n, reachingDefs)
        for (i <- ctx) {
          val regions = i.rhs match {
            case memoryLoad: MemoryLoad =>
              eval(memoryLoad.index, Set.empty, i)
            case _: BitVecLiteral =>
              Set.empty
            case _ =>
              eval(i.rhs, Set.empty, i)
          }
          evaluateExpression(binExpr.arg2, constantProp(n)) match {
            case Some(b: BitVecLiteral) =>
              regions.foreach {
                case stackRegion: StackRegion =>
                  val nextOffset = BinaryExpr(binExpr.op, stackRegion.start, b)
                  evaluateExpression(nextOffset, constantProp(n)) match {
                    case Some(b2: BitVecLiteral) =>
                      reducedRegions = reducedRegions + poolMaster(b2, IRWalk.procedure(n))
                    case None =>
                  }
                case _ =>
              }
            case None =>
          }
        }
      case _ =>
    }
    reducedRegions
  }

  def eval(exp: Expr, env: Set[MemoryRegion], n: Command): Set[MemoryRegion] = {
    Logger.debug(s"evaluating $exp")
    Logger.debug(s"env: $env")
    Logger.debug(s"n: $n")
    exp match {
      case binOp: BinaryExpr =>
        if (spList.contains(binOp.arg1)) {
          evaluateExpression(binOp.arg2, constantProp(n)) match {
            case Some(b: BitVecLiteral) => Set(poolMaster(b, IRWalk.procedure(n)))
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
            eval(BitVecLiteral(0, 64), env, n)
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
      case b: BitVecLiteral =>
        Set(poolMaster(b, IRWalk.procedure(n)))
      // we cannot evaluate this to a concrete value, we need VSA for this
      case _ =>
        Logger.debug(s"type: ${exp.getClass} $exp\n")
        Set()
        // throw new Exception("Unknown type")
    }
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition, s: Set[MemoryRegion]): Set[MemoryRegion] = n match {
    case cmd: Command =>
      cmd match {
        case directCall: DirectCall =>
          val ANR = ANRResult(cmd)
          val RNA = RNAResult(program.procedures.filter(fn => fn == directCall.target).head)
          val parameters = RNA.intersect(ANR)
          // TODO: Re-enable when ReachingDef has interprocedural option
//          val ctx = regionAccesses(cmd)
//          for (elem <- parameters) {
//            if (ctx.contains(RegisterVariableWrapper(elem, getUse(elem, cmd.data, reachingDefs)))) {
//              ctx(RegisterVariableWrapper(elem, getUse(elem, cmd.data, reachingDefs))) match {
//                case FlatEl(al) =>
//                  val regions = eval(al, s, cmd)
//                  //val targetMap = stackMap(directCall.target)
//                  //cfg.funEntries.filter(fn => fn.data == directCall.target).head
//                  procedureToSharedRegions.getOrElseUpdate(directCall.target, mutable.Set.empty).addAll(regions)
//                  registerToRegions.getOrElseUpdate(RegisterVariableWrapper(elem, getUse(elem, cmd.data, reachingDefs)), mutable.Set.empty).addAll(regions)
//              }
//            }
//          }
          if (directCall.target.name == "malloc") {
            evaluateExpression(mallocVariable, constantProp(n)) match {
              case Some(b: BitVecLiteral) => regionLattice.lub(s, Set(HeapRegion(nextMallocCount(), b, IRWalk.procedure(n))))
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

  def transferUnlifted(n: CFGPosition, s: Set[MemoryRegion]): Set[MemoryRegion] = localTransfer(n, s)
}

class MemoryRegionAnalysisSolver(
    program: Program,
    globals: Map[BigInt, String],
    globalOffsets: Map[BigInt, BigInt],
    subroutines: Map[BigInt, String],
    constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
    ANRResult: Map[CFGPosition, Set[Variable]],
    RNAResult: Map[CFGPosition, Set[Variable]],
    regionAccesses: Map[CFGPosition, Map[RegisterVariableWrapper, FlatElement[Expr]]],
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
  ) extends MemoryRegionAnalysis(program, globals, globalOffsets, subroutines, constantProp, ANRResult, RNAResult, regionAccesses, reachingDefs)
  with IRIntraproceduralForwardDependencies
  with Analysis[Map[CFGPosition, LiftedElement[Set[MemoryRegion]]]]
  with WorklistFixpointSolverWithReachability[CFGPosition, Set[MemoryRegion], PowersetLattice[MemoryRegion]] {

  override def funsub(n: CFGPosition, x: Map[CFGPosition, LiftedElement[Set[MemoryRegion]]]): LiftedElement[Set[MemoryRegion]] = {
    n match {
      // function entry nodes are always reachable as this is intraprocedural
      case _: Procedure => liftedLattice.lift(regionLattice.bottom)
      // all other nodes are processed with join+transfer
      case _ => super.funsub(n, x)
    }
  }
}
