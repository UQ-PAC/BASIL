package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.*
import ir.eval.BitVectorEval.bv2SignedInt
import util.MRALogger
import util.assertion.*

import scala.collection.mutable

/** Identifies stack and heap regions.
  *
  * This is iterated until results reach a fixpoint. Subsequent runs may refine the definition of global data regions as
  * well.
  *
  * @param program
  * @param domain
  *   reachable parts of the program
  * @param constantProp
  *   constant propagation results
  * @param reachingDefs
  *   maps each CFG node to two maps: variable definitions and variables uses.
  * @param graResult
  *   results from global region analysis.
  * @param mmm
  *   preloaded globals from symbol table.
  * @param vsaResult
  *   extra information from VSA results of previous passes.
  */
trait MemoryRegionAnalysis(
  val program: Program,
  val domain: Set[CFGPosition],
  val constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
  val graResult: Map[CFGPosition, Set[DataRegion]],
  val mmm: MemoryModelMap,
  val vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]
) {

  private var mallocCount: BigInt = 0
  private var stackCount: Int = 0
  val stackMap: mutable.Map[Procedure, mutable.Map[BigInt, StackRegion]] = mutable.Map()

  private def nextMallocCount(size: BigInt) = {
    val start = mallocCount
    mallocCount += (size.toDouble / 8).ceil.toInt + 1
    (s"malloc_$mallocCount", start)
  }

  private def nextStackCount() = {
    stackCount += 1
    s"stack_$stackCount"
  }

  /** Controls the pool of stack regions. Each pool is unique to a function. If the offset has already been defined in
    * the context of the function, then the same region is returned.
    *
    * @param base
    *   : the offset
    * @param parent
    *   : the function entry node
    * @return
    *   the stack region corresponding to the offset
    */
  private def poolMaster(base: BigInt, stackBase: Procedure, subAccess: BigInt): StackRegion = {
    debugAssert(subAccess >= 0)
    val stackPool = stackMap.getOrElseUpdate(stackBase, mutable.HashMap())
    val region = if (stackPool.contains(base)) {
      stackPool(base)
    } else {
      val newRegion = StackRegion(nextStackCount(), base, stackBase)
      addReturnStack(stackBase, newRegion)
      stackPool += (base -> newRegion)
      newRegion
    }
    mmm.stackSubAccesses(region) =
      mmm.stackSubAccesses.getOrElse(region, mutable.Set()) += (subAccess.toDouble / 8).ceil.toInt
    region
  }

  private def stackDetection(n: CFGPosition, s: Set[Variable]): Set[Variable] = {
    val stackPointerVariables = if (s.isEmpty) {
      Set(stackPointer)
    } else {
      s
    }
    n match {
      case assign: LocalAssign =>
        // check if any of rhs variables is a stack pointer
        val sp = unwrapExprToVar(assign.rhs)
        if (sp.isDefined && stackPointerVariables.contains(sp.get)) {
          // add lhs to stackPointerVariables
          stackPointerVariables + assign.lhs
        } else {
          stackPointerVariables - assign.lhs
        }
      case load: MemoryLoad =>
        stackPointerVariables - load.lhs
      // TODO: should handle the store case (last case)
      case _ => stackPointerVariables
    }
  }

  val stackLattice: PowersetLattice[StackRegion] = PowersetLattice()
  val stackPointerLattice: PowersetLattice[Variable] = PowersetLattice()
  val stackTupleLattice
    : TupleLattice[PowersetLattice[StackRegion], Set[StackRegion], Set[Variable]] =
    TupleLattice(stackLattice, stackPointerLattice)

  val heapLattice: PowersetLattice[HeapRegion] = PowersetLattice()
  val tupleLattice
    : TupleLattice[stackTupleLattice.type, heapLattice.type, (Set[StackRegion], Set[Variable]), Set[HeapRegion]] =
    TupleLattice(stackTupleLattice, heapLattice)

  val lattice: MapLattice[CFGPosition, ((Set[StackRegion], Set[Variable]), Set[HeapRegion]), tupleLattice.type] =
    MapLattice(tupleLattice)

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

  def reducibleToRegion(
    binExpr: BinaryExpr,
    n: Command,
    subAccess: BigInt,
    stackPointerVariables: Set[Variable]
  ): Set[StackRegion] = {
    val reducedRegions: Set[StackRegion] = binExpr.arg1 match {
      case variable: Variable if !stackPointerVariables.contains(variable) =>
        evaluateExpression(binExpr.arg2, constantProp(n)) match {
          case Some(b: BitVecLiteral) =>
            val ctx = getUse(variable, n, reachingDefs)
            val stackRegions = ctx.flatMap {
              case l @ SimulAssign(assigns, _) =>
                assigns.flatMap((lhs, rhs) => eval(rhs, stackPointerVariables, l, subAccess)).toSet
              case l: MemoryAssign => eval(l.rhs, stackPointerVariables, l, subAccess)
              case _: MemoryLoad => Set()
              case unhandled: DirectCall =>
                throw Exception(
                  s"Memory Regions Analysis attempted to retrieve stack regions from direct call, unsupported: $unhandled"
                )
            }
            for (stackRegion <- stackRegions) yield {
              val negB = bv2SignedInt(b)
              val nextOffset = bitVectorOpToBigIntOp(
                binExpr.op,
                stackRegion.start,
                negB
              ) // TODO: should the operation be minus if the value is negative?
              poolMaster(nextOffset, IRWalk.procedure(n), subAccess)
            }
          case None =>
            Set()
        }
      case _ =>
        eval(binExpr, stackPointerVariables, n, subAccess)
    }
    reducedRegions
  }

  def reducibleVariable(
    variable: Variable,
    n: Command,
    subAccess: BigInt,
    stackPointerVariables: Set[Variable]
  ): Set[StackRegion] = {
    val ctx = getDefinition(variable, n, reachingDefs)

    // TODO: nicer way to deal with loops (a variable is being incremented in a loop)
    val regions = ctx.flatMap { i =>
      if (i != n) {
        i match {
          case l: MemoryAssign => eval(l.rhs, stackPointerVariables, l, subAccess)
          case l @ SimulAssign(assigns, _) =>
            assigns.flatMap((lhs, rhs) => eval(rhs, stackPointerVariables, l, subAccess)).toSet
          case m: MemoryLoad => eval(m.index, stackPointerVariables, m, m.size)
          case d: DirectCall =>
            throw Exception(s"attempted to reduce variables from direct call, unssupported: $d")
        }
      } else {
        Set()
      }
    }
    regions
  }

  def eval(exp: Expr, stackPointerVariables: Set[Variable], n: Command, subAccess: BigInt): Set[StackRegion] = {
    if (graResult(n).nonEmpty) {
      Set.empty // skip global memory regions
    } else {
      exp match {
        case binOp: BinaryExpr =>
          if (binOp.arg1.variables.exists(v => stackPointerVariables.contains(v))) {
            evaluateExpression(binOp.arg2, constantProp(n)) match {
              case Some(b: BitVecLiteral) =>
                val negB = bv2SignedInt(b)
                Set(poolMaster(negB, IRWalk.procedure(n), subAccess))
              case None => Set.empty
            }
          } else if (reducibleToRegion(binOp, n, subAccess, stackPointerVariables).nonEmpty) {
            reducibleToRegion(binOp, n, subAccess, stackPointerVariables)
          } else {
            Set.empty
          }
        case reg @ Register(_, _) if stackPointerVariables.contains(reg) => // TODO: this is a hack because stackPointerVariables is not comprehensive it needs to be a standalone analysis
          if (getDefinition(reg, n, reachingDefs).isEmpty) {
            Set(poolMaster(Long.MaxValue, IRWalk.procedure(n), subAccess))
          } else {
            reducibleVariable(reg, n, subAccess, stackPointerVariables)
          }
        case variable: Variable =>
          evaluateExpression(variable, constantProp(n)) match {
            case Some(b: BitVecLiteral) =>
              eval(b, stackPointerVariables, n, subAccess)
            case _ =>
              reducibleVariable(variable, n, subAccess, stackPointerVariables)
          }
        // ignore case where it could be a global region (loaded later in MMM from relf)
        case _: BitVecLiteral =>
          Set.empty
        // we cannot evaluate this to a concrete value, we need VSA for this
        case _ =>
          MRALogger.debug(s"type: ${exp.getClass} $exp\n")
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
    } else {
      Set()
    }
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
  def transfer(
    n: CFGPosition,
    s: ((Set[StackRegion], Set[Variable]), Set[HeapRegion])
  ): ((Set[StackRegion], Set[Variable]), Set[HeapRegion]) = {
    val stackPointerVariables = stackDetection(n, s(0)(1))
    n match {
      case directCall: DirectCall =>
        if (directCall.target.procName == "malloc") {
          evaluateExpression(mallocVariable, constantProp(n)) match {
            case Some(b: BitVecLiteral) =>
              val negB = bv2SignedInt(b)
              val (name, start) = nextMallocCount(negB)
              val newHeapRegion = HeapRegion(name, start, negB, IRWalk.procedure(n))
              addReturnHeap(directCall, newHeapRegion)
              ((Set.empty, stackPointerVariables), s(1) + newHeapRegion)
            case None =>
              // Assume heap region size is at least 1 TODO: must approximate size of heap
              val negB = 1
              val (name, start) = nextMallocCount(negB)
              val newHeapRegion = HeapRegion(name, start, negB, IRWalk.procedure(n))
              addReturnHeap(directCall, newHeapRegion)
              ((Set.empty, stackPointerVariables), s(1) + newHeapRegion)
          }
        } else {
          ((Set.empty, stackPointerVariables), s(1))
        }
      case store: MemoryStore =>
        val isHeap = checkForHeap(store.index, n)
        if (isHeap.nonEmpty) {
          ((Set.empty, stackPointerVariables), s(1) ++ isHeap)
        } else {
          ((eval(store.index, stackPointerVariables, store, store.size), stackPointerVariables), Set.empty)
        }
      case _: LocalAssign =>
        // this is a constant, but we need to check if it is a data region
        ((Set.empty, stackPointerVariables), s(1))
      case load: MemoryLoad =>
        val isHeap = checkForHeap(load.index, n)
        if (isHeap.nonEmpty) {
          ((Set.empty, stackPointerVariables), s(1) ++ isHeap)
        } else {
          ((eval(load.index, stackPointerVariables, load, load.size), stackPointerVariables), Set.empty)
        }
      case _ => ((Set.empty, stackPointerVariables), s(1))
    }
  }

}

class MemoryRegionAnalysisSolver(
  program: Program,
  domain: Set[CFGPosition],
  constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
  graResult: Map[CFGPosition, Set[DataRegion]],
  mmm: MemoryModelMap,
  vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]
) extends MemoryRegionAnalysis(program, domain, constantProp, reachingDefs, graResult, mmm, vsaResult)
    with IRIntraproceduralForwardDependencies
    with Analysis[Map[CFGPosition, ((Set[StackRegion], Set[Variable]), Set[HeapRegion])]]
    with SimpleWorklistFixpointSolver[
      CFGPosition,
      ((Set[StackRegion], Set[Variable]), Set[HeapRegion]),
      TupleLattice[
        TupleLattice[PowersetLattice[StackRegion], Set[StackRegion], Set[Variable]],
        PowersetLattice[HeapRegion],
        (Set[StackRegion], Set[Variable]),
        Set[HeapRegion]
      ]
    ]
