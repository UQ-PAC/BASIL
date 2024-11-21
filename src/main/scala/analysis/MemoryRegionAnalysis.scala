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
                           val mmm: MemoryModelMap,
                           val vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]) {

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

  private def stackDetection(n: CFGPosition, s: Set[Variable]): Set[Variable] = {
    var spList = s
    if (spList.isEmpty) {
      spList = spList + stackPointer
    }
    n match {
      case assign: Assign =>
        // check if any of rhs variables is a stack pointer
        val sp = unwrapExprToVar(assign.rhs)
        val isSP = sp.isDefined && spList.contains(sp.get)
        if (isSP) {
          // add lhs to spList
          return spList + assign.lhs
        }
        spList - assign.lhs
      // TODO: should handle the store case (last case)
      case _ => spList
    }
  }

  private def isAStackPointer(expr: Expr, spList: Set[Variable]): Boolean = {
    expr.variables.exists(v => spList.contains(v))
  }

  val stackLattice: PowersetLattice[StackRegion] = PowersetLattice()
  val stackPointerLattice: PowersetLattice[Variable] = PowersetLattice()
  val stackTupleLattice: TupleLattice[PowersetLattice[StackRegion], PowersetLattice[Variable], Set[StackRegion], Set[Variable]] = TupleLattice(stackLattice, stackPointerLattice)

  val heapLattice: PowersetLattice[HeapRegion] = PowersetLattice()
  val tupleLattice: TupleLattice[stackTupleLattice.type, heapLattice.type, (Set[StackRegion], Set[Variable]), Set[HeapRegion]] = TupleLattice(stackTupleLattice, heapLattice)

  val lattice: MapLattice[CFGPosition, ((Set[StackRegion], Set[Variable]), Set[HeapRegion]), tupleLattice.type] = MapLattice(tupleLattice)

  val first: Set[CFGPosition] = Set.empty + program.mainProcedure

  private val stackPointer = Register("R31", 64)
  private val mallocVariable = Register("R0", 64)
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

  def reducibleToRegion(binExpr: BinaryExpr, n: Command, subAccess: BigInt, spList: Set[Variable]): Set[StackRegion] = {
    val reducedRegions: Set[StackRegion] = binExpr.arg1 match {
      case variable: Variable if !spList.contains(variable) =>
        evaluateExpression(binExpr.arg2, constantProp(n)) match {
          case Some(b: BitVecLiteral) =>
            val ctx = getUse(variable, n, reachingDefs).filter(assign => unwrapExpr(assign.rhs).isEmpty)
            for {
              i <- ctx
              stackRegion <- eval(i.rhs, Set.empty, spList, i, subAccess)
            } yield {
              val negB = if isNegative(b) then b.value - BigInt(2).pow(b.size) else b.value
              val nextOffset = bitVectorOpToBigIntOp(binExpr.op, stackRegion.start, negB) // TODO: should the operation be minus if the value is negative?
              poolMaster(nextOffset, IRWalk.procedure(n), subAccess)
            }
          case None =>
            Set()
        }
      case _ =>
        eval(binExpr, Set.empty, spList, n, subAccess)
    }
    reducedRegions
  }

  def reducibleVariable(variable: Variable, n: Command, subAccess: BigInt, spList: Set[Variable]): Set[StackRegion] = {
    val ctx = getDefinition(variable, n, reachingDefs)

    // TODO: nicer way to deal with loops (a variable is being incremented in a loop)
    val regions = ctx.flatMap { i =>
      if (i != n) {
        eval(i.rhs, Set.empty, spList, i, subAccess)
      } else {
        Set()
      }
    }
    regions
  }

  def eval(exp: Expr, env: Set[StackRegion], spList: Set[Variable], n: Command, subAccess: BigInt): Set[StackRegion] = {
    if (graResult(n).nonEmpty) {
      Set.empty // skip global memory regions
    } else {
      exp match {
        case binOp: BinaryExpr =>
          if (isAStackPointer(binOp.arg1, spList)) {
            evaluateExpression(binOp.arg2, constantProp(n)) match {
              case Some(b: BitVecLiteral) =>
                val negB = if isNegative(b) then b.value - BigInt(2).pow(b.size) else b.value
                Set(poolMaster(negB, IRWalk.procedure(n), subAccess))
              case None => Set.empty
            }
          } else if (reducibleToRegion(binOp, n, subAccess, spList).nonEmpty) {
            reducibleToRegion(binOp, n, subAccess, spList)
          } else {
            Set.empty
          }
        case reg: Register if spList.contains(reg) => // TODO: this is a hack because spList is not comprehensive it needs to be a standalone analysis
          if (getDefinition(reg, n, reachingDefs).isEmpty) {
            Set(poolMaster(Long.MaxValue, IRWalk.procedure(n), subAccess))
          } else {
            reducibleVariable(reg, n, subAccess, spList)
          }
        case variable: Variable =>
          evaluateExpression(variable, constantProp(n)) match {
            case Some(b: BitVecLiteral) =>
              eval(b, env, spList, n, subAccess)
            case _ =>
              reducibleVariable(variable, n, subAccess, spList)
          }
        case memoryLoad: MemoryLoad =>
          eval(memoryLoad.index, env, spList, n, memoryLoad.size)
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

  def checkForHeap(expr: Expr, n: CFGPosition): Set[HeapRegion] = { // may need to go to definitions (uses instead of n)
    val possibleVar = unwrapExprToVar(expr)
    if (possibleVar.isDefined) {
      val variable = possibleVar.get
      val uses = getUse(variable, n, reachingDefs)
      uses.flatMap(i => getVSAHints(variable, i))
    }
    Set()
  }

  def getVSAHints(variable: Variable, n: CFGPosition): Set[HeapRegion] = {
    val collage: Set[HeapRegion] = vsaResult.get(n) match {
      case Some(Lift(el)) =>
        el.getOrElse(variable, Set()).flatMap {
          case AddressValue(heapRegion2: HeapRegion) => Some(heapRegion2)
          case _ => Set()
        }
      case _ => Set()
    }
    collage
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition, s: ((Set[StackRegion], Set[Variable]), Set[HeapRegion])): ((Set[StackRegion], Set[Variable]), Set[HeapRegion]) =
    val spList = stackDetection(n, s._1._2)
    n match {
    case directCall: DirectCall =>
      if (directCall.target.name == "malloc") {
        evaluateExpression(mallocVariable, constantProp(n)) match {
          case Some(b: BitVecLiteral) =>
            val negB = if isNegative(b) then b.value - BigInt(2).pow(b.size) else b.value
            val (name, start) = nextMallocCount(negB)
            val newHeapRegion = HeapRegion(name, start, negB, IRWalk.procedure(n))
            addReturnHeap(directCall, newHeapRegion)
            ((Set.empty, spList), s._2 + newHeapRegion)
          case None =>
            // Assume heap region size is at least 1 TODO: must approximate size of heap
            val negB = 1
            val (name, start) = nextMallocCount(negB)
            val newHeapRegion = HeapRegion(name, start, negB, IRWalk.procedure(n))
            addReturnHeap(directCall, newHeapRegion)
            ((Set.empty, spList), s._2 + newHeapRegion)
        }
      } else {
        ((Set.empty, spList), s._2)
      }
    case memAssign: MemoryAssign =>
      val isHeap = checkForHeap(memAssign.index, n)
      if (isHeap.nonEmpty) {
          ((Set.empty, spList), s._2 ++ isHeap)
      } else {
        ((eval(memAssign.index, s._1._1, s._1._2, memAssign, memAssign.size), spList), Set.empty)
      }
    case assign: Assign =>
      val unwrapped = unwrapExpr(assign.rhs)
      if (unwrapped.isDefined) {
        ((eval(unwrapped.get.index, s._1._1, s._1._2, assign, unwrapped.get.size), spList), Set.empty)
      } else {
        // this is a constant, but we need to check if it is a data region
        ((Set.empty, spList), s._2)
      }
    case _ => ((Set.empty, spList), s._2)
  }

  def transfer(n: CFGPosition, s: ((Set[StackRegion], Set[Variable]), Set[HeapRegion])): ((Set[StackRegion], Set[Variable]), Set[HeapRegion]) = localTransfer(n, s)
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
    mmm: MemoryModelMap,
    vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]
  ) extends MemoryRegionAnalysis(program, domain, globals, globalOffsets, subroutines, constantProp, ANRResult, RNAResult, reachingDefs, graResult, mmm, vsaResult)
  with IRIntraproceduralForwardDependencies
  with Analysis[Map[CFGPosition, ((Set[StackRegion], Set[Variable]), Set[HeapRegion])]]
  with SimpleWorklistFixpointSolver[CFGPosition, ((Set[StackRegion], Set[Variable]), Set[HeapRegion]), TupleLattice[TupleLattice[PowersetLattice[StackRegion], PowersetLattice[Variable], Set[StackRegion], Set[Variable]], PowersetLattice[HeapRegion], (Set[StackRegion], Set[Variable]), Set[HeapRegion]]]