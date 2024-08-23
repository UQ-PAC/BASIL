package analysis

import analysis.BitVectorEval.isNegative
import analysis.solvers.WorklistFixpointSolverWithReachability
import ir.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait MemoryRegionAnalysis(val program: Program,
                           val globals: Map[BigInt, String],
                           val globalOffsets: Map[BigInt, BigInt],
                           val subroutines: Map[BigInt, String],
                           val constantProp: Map[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]],
                           val ANRResult: Map[CFGPosition, Set[Variable]],
                           val RNAResult: Map[CFGPosition, Set[Variable]],
                           val regionAccesses: Map[CfgNode, Map[RegisterWrapperPartialEquality, FlatElement[Expr]]],
                           val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
                           val maxDepth: Int,
                           val exactMatch: Boolean) {

  private var mallocCount: Int = 0
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
  private val registerToRegions: mutable.Map[RegisterWrapperPartialEquality, mutable.Set[MemoryRegion]] = mutable.Map()
  val procedureToSharedRegions: mutable.Map[Procedure, mutable.Set[MemoryRegion]] = mutable.Map()
  var depthMap: mutable.Map[CFGPosition, Int] = mutable.Map()

  def reducibleToRegion(binExpr: BinaryExpr, n: Command): Set[MemoryRegion] = {
    var reducedRegions = Set.empty[MemoryRegion]
    if (depthMap.contains(n)) {
      if (depthMap(n) > maxDepth) {
        depthMap += (n -> 0)
        return reducedRegions
      }
    } else {
      depthMap += (n -> 0)
    }
    depthMap(n) += 1
    binExpr.arg1 match {
      case variable: Variable if !spList.contains(variable) =>
        val ctx = getUse(variable, n, reachingDefs)
        for (i <- ctx) {
          val regions = i.rhs match {
            case memoryLoad: MemoryLoad =>
              eval(memoryLoad.index, Set.empty, i)
            case _: BitVecLiteral =>
              Set.empty
            case _ =>
              println(s"OG $n")
              println(s"Unreducible: $i")
              eval(i.rhs, Set.empty, i)
          }
          for (elem <- evaluateExpressionWithSSA(binExpr.arg2, constantProp(n), n, reachingDefs, exactMatch)) {
            elem match {
              case b: BitVecLiteral =>
                regions.foreach {
                  case stackRegion: StackRegion =>
                    val nextOffset = BinaryExpr(binExpr.op, stackRegion.start, b)
                    for (elem <- evaluateExpressionWithSSA(nextOffset, constantProp(n), n, reachingDefs, exactMatch)) {
                      elem match {
                        case b2: BitVecLiteral =>
                          reducedRegions = reducedRegions + poolMaster(b2, IRWalk.procedure(n))
                      }
                    }
                  case _ =>
                }
            }
          }
        }
      case _ =>
        eval(binExpr, Set.empty, n)
    }
    reducedRegions
  }

  def reducibleVariable(variable: Variable, n: Command): Set[MemoryRegion] = {
    var regions = Set.empty[MemoryRegion]
    val ctx = getDefinition(variable, n, reachingDefs)
    for (i <- ctx) {
      i.rhs match {
        case binaryExpr: BinaryExpr =>
          regions = regions ++ reducibleToRegion(binaryExpr, i)
        case _ =>
          //regions = regions ++ eval(i.rhs, Set.empty, i)
      }
    }
    regions
  }

  def eval(exp: Expr, env: Set[MemoryRegion], n: Command): Set[MemoryRegion] = {
    println(s"Asked to evaluate: $exp at ${n.label}")
    val regionsToReturn = mutable.Set[MemoryRegion]()
    exp match {
      case binOp: BinaryExpr =>
        if (spList.contains(binOp.arg1)) {
          for (elem <- evaluateExpressionWithSSA(binOp.arg2, constantProp(n), n, reachingDefs, exactMatch)) {
            elem match {
              case b: BitVecLiteral =>
                if (isNegative(b)) {
                  regionsToReturn.addAll(Set(poolMaster(BitVecLiteral(0, 64), IRWalk.procedure(n))))
                } else {
                  regionsToReturn.addAll(Set(poolMaster(b, IRWalk.procedure(n))))
                }
            }
          }
        } else {
          val reduced = reducibleToRegion(binOp, n)
          if (reduced.nonEmpty) {
            println(s"Reducible: exp $exp")
            regionsToReturn.addAll(reduced)
          } else {
            val elems = evaluateExpressionWithSSA(binOp, constantProp(n), n, reachingDefs, exactMatch)
            for (elem <- elems) {
              elem match {
                case b: BitVecLiteral => regionsToReturn.addAll(eval(b, env, n))
              }
            }
            if (elems.isEmpty) {
              regionsToReturn.addAll(eval(binOp.arg1, env, n) ++ eval(binOp.arg2, env, n))
            }
          }
        }
      case variable: Variable =>
        variable match {
          case _: LocalVar =>
          case reg: Register if spList.contains(reg) =>
            regionsToReturn.addAll(Set(poolMaster(BitVecLiteral(0, 64), IRWalk.procedure(n))))
          case _ =>
            for (elem <- evaluateExpressionWithSSA(variable, constantProp(n), n, reachingDefs, exactMatch)) {
              elem match {
                case b: BitVecLiteral => regionsToReturn.addAll(eval(b, env, n))
                case _ => reducibleVariable(variable, n)
              }
            }
        }
      case memoryLoad: MemoryLoad =>
        regionsToReturn.addAll(eval(memoryLoad.index, env, n))
      // ignore case where it could be a global region (loaded later in MMM from relf)
      case b: BitVecLiteral =>
      case literal: Literal => // ignore literals other than BitVectors
      case extract: Extract =>
        regionsToReturn.addAll(eval(extract.body, env, n))
      case repeat: Repeat =>
        regionsToReturn.addAll(eval(repeat.body, env, n))
      case zeroExtend: ZeroExtend =>
        regionsToReturn.addAll(eval(zeroExtend.body, env, n))
      case signExtend: SignExtend =>
        regionsToReturn.addAll(eval(signExtend.body, env, n))
      case unaryExpr: UnaryExpr =>
        regionsToReturn.addAll(eval(unaryExpr.arg, env, n))
      case memoryStore: MemoryAssign =>
        regionsToReturn.addAll(eval(memoryStore.index, env, n) ++ eval(memoryStore.value, env, n))
      case memory: Memory =>
    }
    regionsToReturn.toSet
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition, s: Set[MemoryRegion]): Set[MemoryRegion] =
    var m = s
    n match {
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
            for (elem <- evaluateExpressionWithSSA(mallocVariable, constantProp(n), n, reachingDefs, exactMatch)) {
              elem match {
                case b: BitVecLiteral => m = regionLattice.lub(m, Set(HeapRegion(nextMallocCount(), b, IRWalk.procedure(n))))
              }
            }
          }
        case memAssign: MemoryAssign =>
          val result = eval(memAssign.index, m, cmd)
          m = regionLattice.lub(m, result)
        case localAssign: Assign =>
          stackDetection(localAssign)
          val result = eval(localAssign.rhs, m, cmd)
          m = regionLattice.lub(m, result)
        case _ =>
      }
    case _ => // ignore other kinds of nodes
  }
    m

  def transferUnlifted(n: CFGPosition, s: Set[MemoryRegion]): Set[MemoryRegion] = localTransfer(n, s)
}

class MemoryRegionAnalysisSolver(
                                  program: Program,
                                  globals: Map[BigInt, String],
                                  globalOffsets: Map[BigInt, BigInt],
                                  subroutines: Map[BigInt, String],
                                  constantProp: Map[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]],
                                  ANRResult: Map[CFGPosition, Set[Variable]],
                                  RNAResult: Map[CFGPosition, Set[Variable]],
                                  regionAccesses: Map[CfgNode, Map[RegisterWrapperPartialEquality, FlatElement[Expr]]],
                                  reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
                                  maxDepth: Int,
                                  exactMatch: Boolean = true
  ) extends MemoryRegionAnalysis(program, globals, globalOffsets, subroutines, constantProp, ANRResult, RNAResult, regionAccesses, reachingDefs, maxDepth, exactMatch)
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

class InterprocMemoryRegionAnalysisSolver(
                                  program: Program,
                                  globals: Map[BigInt, String],
                                  globalOffsets: Map[BigInt, BigInt],
                                  subroutines: Map[BigInt, String],
                                  constantProp: Map[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]],
                                  ANRResult: Map[CFGPosition, Set[Variable]],
                                  RNAResult: Map[CFGPosition, Set[Variable]],
                                  regionAccesses: Map[CfgNode, Map[RegisterWrapperPartialEquality, FlatElement[Expr]]],
                                  reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
                                  maxDepth: Int,
                                  exactMatch: Boolean = false
                                ) extends MemoryRegionAnalysis(program, globals, globalOffsets, subroutines, constantProp, ANRResult, RNAResult, regionAccesses, reachingDefs, maxDepth, exactMatch)
  with IRInterproceduralForwardDependencies
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