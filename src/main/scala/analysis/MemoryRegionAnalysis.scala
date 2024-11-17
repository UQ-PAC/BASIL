package analysis

import analysis.BitVectorEval.isNegative
import analysis.solvers.SimpleWorklistFixpointSolver
import ir.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait MemoryRegionAnalysis(val program: Program,
                           val domain: Set[CFGPosition],
                           val globals: Map[BigInt, String],
                           val globalOffsets: Map[BigInt, BigInt],
                           val subroutines: Map[BigInt, String],
                           val constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                           val ANRResult: Map[CFGPosition, Set[Variable]],
                           val RNAResult: Map[CFGPosition, Set[Variable]],
                           val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
                           val graResult: Map[CFGPosition, Set[DataRegion]],
                           val mmm: MemoryModelMap) {

  private var mallocCount: BigInt = 0
  private var stackCount: Int = 0
  val stackMap: mutable.Map[Procedure, mutable.Map[BigInt, StackRegion]] = mutable.Map()

  private def nextMallocCount(size: BigInt) = {
    val start = mallocCount
    mallocCount += (size.toDouble/8).ceil.toInt + 1
    (s"malloc_$mallocCount", start)
  }

  private def nextStackCount() = {
    stackCount += 1
    s"stack_$stackCount"
  }

  /**
   * Controls the pool of stack regions. Each pool is unique to a function.
   * If the offset has already been defined in the context of the function, then the same region is returned.
   *
   * @param base   : the offset
   * @param parent : the function entry node
   * @return the stack region corresponding to the offset
   */
  private def poolMaster(base: BigInt, stackBase: Procedure, subAccess: BigInt): StackRegion = {
    assert(subAccess >= 0)
    val stackPool = stackMap.getOrElseUpdate(stackBase, mutable.HashMap())
    val region = if (stackPool.contains(base)) {
      stackPool(base)
    } else {
      val newRegion = StackRegion(nextStackCount(), base, stackBase)
      addReturnStack(stackBase, newRegion)
      stackPool += (base -> newRegion)
      newRegion
    }
    mmm.stackSubAccesses(region) = mmm.stackSubAccesses.getOrElse(region, mutable.Set()) += (subAccess.toDouble/8).ceil.toInt
    region
  }

  private def stackDetection(stmt: Statement): Unit = {
    Logger.debug("Stack detection")
    Logger.debug(spList)
    stmt match {
      case assign: Assign =>
        if (spList.contains(assign.rhs)) {
          // add lhs to spList
          spList.addOne(assign.lhs)
        // TODO: This is a hack: it should check for stack ptr using the wrapper
        } else if (spList.contains(assign.lhs) && assign.lhs != stackPointer) {
          // remove lhs from spList
          spList.remove(spList.indexOf(assign.lhs))
        }
      // TODO: should handle the store case (last case)
      case _ =>
    }
  }

  val regionLattice: PowersetLattice[StackRegion] = PowersetLattice()

  val lattice: MapLattice[CFGPosition, Set[StackRegion], PowersetLattice[StackRegion]] = MapLattice(regionLattice)

  val first: Set[CFGPosition] = Set.empty + program.mainProcedure

  private val stackPointer = Register("R31", 64)
  private val mallocVariable = Register("R0", 64)
  private val spList = ListBuffer[Expr](stackPointer)
  val procedureToSharedRegions: mutable.Map[Procedure, mutable.Set[MemoryRegion]] = mutable.Map()
  var procedureToStackRegions: mutable.Map[Procedure, mutable.Set[StackRegion]] = mutable.Map()
  var procedureToHeapRegions: mutable.Map[DirectCall, HeapRegion] = mutable.Map()
  var mergeRegions: mutable.Set[Set[MemoryRegion]] = mutable.Set()

  def addReturnStack(procedure: Procedure, returnRegion: StackRegion): Unit = {
    procedureToStackRegions.getOrElseUpdate(procedure, mutable.Set.empty).add(returnRegion)
  }

  def addReturnHeap(directCall: DirectCall, returnRegion: HeapRegion): Unit = {
    procedureToHeapRegions.put(directCall, returnRegion)
  }

  def reducibleToRegion(binExpr: BinaryExpr, n: Command, subAccess: BigInt): Set[StackRegion] = {
    val reducedRegions = binExpr.arg1 match {
      case variable: Variable if !spList.contains(variable) =>
        evaluateExpression(binExpr.arg2, constantProp(n)) match {
          case Some(b: BitVecLiteral) =>
            val ctx = getUse(variable, n, reachingDefs).filter(assign => unwrapExpr(assign.rhs).isEmpty)
            for {
              i <- ctx
              stackRegion <- eval(i.rhs, Set.empty, i, subAccess)
            } yield {
              val nextOffset = bitVectorOpToBigIntOp(binExpr.op, stackRegion.start, b.value)
              poolMaster(nextOffset, IRWalk.procedure(n), subAccess)
            }
          case None =>
            Set()
        }
      case _ =>
        eval(binExpr, Set.empty, n, subAccess)
    }
    reducedRegions
  }

  def reducibleVariable(variable: Variable, n: Command, subAccess: BigInt): Set[StackRegion] = {
    val ctx = getDefinition(variable, n, reachingDefs)

    // TODO: nicer way to deal with loops (a variable is being incremented in a loop)
    val regions = ctx.flatMap { i =>
      if (i != n) {
        eval(i.rhs, Set.empty, i, subAccess)
      } else {
        Set()
      }
    }
    regions
  }

  def eval(exp: Expr, env: Set[StackRegion], n: Command, subAccess: BigInt): Set[StackRegion] = {
    if (graResult(n).nonEmpty) {
      Set.empty // skip global memory regions
    } else {
      exp match {
        case binOp: BinaryExpr =>
          if (spList.contains(binOp.arg1)) {
            evaluateExpression(binOp.arg2, constantProp(n)) match {
              case Some(b: BitVecLiteral) =>
                val negB = if isNegative(b) then b.value - BigInt(2).pow(b.size) else b.value
                Set(poolMaster(negB, IRWalk.procedure(n), subAccess))
              case None => Set.empty
            }
          } else if (reducibleToRegion(binOp, n, subAccess).nonEmpty) {
            reducibleToRegion(binOp, n, subAccess)
          } else {
            Set.empty
          }
        case reg: Register if spList.contains(reg) => // TODO: this is a hack because spList is not comprehensive it needs to be a standalone analysis
          if (getDefinition(reg, n, reachingDefs).isEmpty) {
            Set(poolMaster(Long.MaxValue, IRWalk.procedure(n), subAccess))
          } else {
            reducibleVariable(reg, n, subAccess)
          }
        case variable: Variable =>
          evaluateExpression(variable, constantProp(n)) match {
            case Some(b: BitVecLiteral) =>
              eval(b, env, n, subAccess)
            case _ =>
              reducibleVariable(variable, n, subAccess)
          }
        case memoryLoad: MemoryLoad =>
          eval(memoryLoad.index, env, n, memoryLoad.size)
        // ignore case where it could be a global region (loaded later in MMM from relf)
        case _: BitVecLiteral =>
          Set.empty
        // we cannot evaluate this to a concrete value, we need VSA for this
        case _ =>
          Logger.debug(s"type: ${exp.getClass} $exp\n")
          throw new Exception("Unknown type")
      }
    }
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition, s: Set[StackRegion]): Set[StackRegion] = n match {
    case directCall: DirectCall =>
      // TODO: Re-enable when ReachingDef has interprocedural option
      // val ANR = ANRResult(directCall)
      // val RNA = RNAResult(program.procedures.filter(fn => fn == directCall.target).head)
      //      val parameters = RNA.intersect(ANR)
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
          case Some(b: BitVecLiteral) =>
            val negB = if isNegative(b) then b.value - BigInt(2).pow(b.size) else b.value
            val (name, start) = nextMallocCount(negB)
            val newHeapRegion = HeapRegion(name, start, negB, IRWalk.procedure(n))
            addReturnHeap(directCall, newHeapRegion)
            s
          case None =>
            // Assume heap region size is at least 1 TODO: must approximate size of heap
            val negB = 1
            val (name, start) = nextMallocCount(negB)
            val newHeapRegion = HeapRegion(name, start, negB, IRWalk.procedure(n))
            addReturnHeap(directCall, newHeapRegion)
            s
        }
      } else {
        s
      }
    case memAssign: MemoryAssign =>
      eval(memAssign.index, s, memAssign, memAssign.size)
    case assign: Assign =>
      stackDetection(assign)
      val unwrapped = unwrapExpr(assign.rhs)
      if (unwrapped.isDefined) {
        eval(unwrapped.get.index, s, assign, unwrapped.get.size)
      } else {
        Set()
      }
    case _ => s
  }

  def transfer(n: CFGPosition, s: Set[StackRegion]): Set[StackRegion] = localTransfer(n, s)
}

class MemoryRegionAnalysisSolver(
    program: Program,
    domain: Set[CFGPosition],
    globals: Map[BigInt, String],
    globalOffsets: Map[BigInt, BigInt],
    subroutines: Map[BigInt, String],
    constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
    ANRResult: Map[CFGPosition, Set[Variable]],
    RNAResult: Map[CFGPosition, Set[Variable]],
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
    graResult: Map[CFGPosition, Set[DataRegion]],
    mmm: MemoryModelMap
  ) extends MemoryRegionAnalysis(program, domain, globals, globalOffsets, subroutines, constantProp, ANRResult, RNAResult, reachingDefs, graResult, mmm)
  with IRIntraproceduralForwardDependencies
  with Analysis[Map[CFGPosition, Set[StackRegion]]]
  with SimpleWorklistFixpointSolver[CFGPosition, Set[StackRegion], PowersetLattice[StackRegion]]