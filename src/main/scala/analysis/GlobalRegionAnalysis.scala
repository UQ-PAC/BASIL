package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.*

import scala.collection.mutable

trait GlobalRegionAnalysis(val program: Program,
                           val domain: Set[CFGPosition],
                           val constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                           val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
                           val mmm: MemoryModelMap,
                           val vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]) {

  private var dataCount: Int = 0
  private def nextDataCount() = {
    dataCount += 1
    s"data_$dataCount"
  }

  val regionLattice: PowersetLattice[DataRegion] = PowersetLattice()

  val lattice: MapLattice[CFGPosition, Set[DataRegion], PowersetLattice[DataRegion]] = MapLattice(regionLattice)

  val first: Set[CFGPosition] = Set(program.mainProcedure)

  private val dataMap: mutable.HashMap[BigInt, DataRegion] = mutable.HashMap()

  private def dataPoolMaster(offset: BigInt, size: BigInt): DataRegion = {
    assert(size >= 0)
    if (dataMap.contains(offset)) {
      if (dataMap(offset).size < (size.toDouble / 8).ceil.toInt) {
        dataMap(offset) = DataRegion(dataMap(offset).regionIdentifier, offset, (size.toDouble / 8).ceil.toInt)
        dataMap(offset)
      } else {
        dataMap(offset)
      }
    } else {
      dataMap(offset) = DataRegion(nextDataCount(), offset, (size.toDouble / 8).ceil.toInt)
      dataMap(offset)
    }
  }

  def getDataMap: mutable.HashMap[BigInt, DataRegion] = dataMap

  /**
   * For DataRegions, the actual address used needs to be converted to the relocated address.
   * This is because when regions are found, the relocated address is used and as such match
   * the correct range.
   *
   * @param address: The starting DataRegion
   * @return DataRegion: The relocated data region if any
   */
  def resolveGlobalOffsetSecondLast(address: DataRegion): DataRegion = {
    var tableAddress = address
    // addresses may be layered as in jumptable2 example for which recursive search is required
    var exitLoop = false
    while (mmm.relocatedDataRegion(tableAddress.start).isDefined && mmm.relocatedDataRegion(mmm.relocatedDataRegion(tableAddress.start).get.start).isDefined && !exitLoop) {
      val newAddress = mmm.relocatedDataRegion(tableAddress.start).getOrElse(tableAddress)
      if (newAddress == tableAddress) {
        exitLoop = true
      } else {
        tableAddress = newAddress
      }
    }
    tableAddress
  }

  def tryCoerceIntoData(exp: Expr, n: Command, subAccess: BigInt): Set[DataRegion] = {
    val eval = evaluateExpression(exp, constantProp(n))
    if (eval.isDefined) {
      val index = eval.get.value
      Set(dataPoolMaster(index, subAccess))
    } else {
      exp match {
        case literal: BitVecLiteral => tryCoerceIntoData(literal, n, subAccess)
        case Extract(_, _, body) => tryCoerceIntoData(body, n, subAccess)
        case Repeat(_, body) => tryCoerceIntoData(body, n, subAccess)
        case ZeroExtend(_, body) => tryCoerceIntoData(body, n, subAccess)
        case SignExtend(_, body) => tryCoerceIntoData(body, n, subAccess)
        case UnaryExpr(_, arg) => tryCoerceIntoData(arg, n, subAccess)
        case BinaryExpr(op, arg1, arg2) =>
          val evalArg2 = evaluateExpression(arg2, constantProp(n))
          if (evalArg2.isDefined) {
            tryCoerceIntoData(arg1, n, subAccess) flatMap { i =>
              val newExpr = BinaryExpr(op, BitVecLiteral(i.start, evalArg2.get.size), evalArg2.get)
              tryCoerceIntoData(newExpr, n, subAccess)
            }
          } else {
            Set()
          }
        case _: MemoryLoad => ???
        case _: UninterpretedFunction => Set.empty
        case variable: Variable =>
          val collage: Set[DataRegion] = vsaResult.get(n) match {
            case Some(Lift(el)) =>
              el.getOrElse(variable, Set()).flatMap {
                case addressValue: AddressValue =>
                  el.getOrElse(addressValue.region, Set(addressValue)).flatMap {
                    case AddressValue(dataRegion2: DataRegion) => Some(dataRegion2)
                    case _ => Set()
                  }
                case _ => Set()
              }
            case _ => Set()
          }
          return collage
        case _ => Set()
      }
    }
  }

  /**
   * Check if the data region is defined.
   * Finds full and partial matches
   * Full matches sizes are altered to match the size of the data region
   * Partial matches are not altered
   * Otherwise the data region is returned
   *
   * @param dataRegions Set[DataRegion]
   * @param n CFGPosition
   * @return Set[DataRegion]
   */
  def checkIfDefined(dataRegions: Set[DataRegion], n: CFGPosition): Set[DataRegion] = {
    val converted = dataRegions.map { i =>
      val (f, p) = mmm.findDataObjectWithSize(i.start, i.size)
      val accesses = f.union(p)
      if (accesses.isEmpty) {
        i
      } else if (accesses.size == 1) {
        dataMap(i.start) = DataRegion(i.regionIdentifier, i.start, i.size.max(accesses.head.size))
        dataMap(i.start)
      } else {
        val highestRegion = accesses.maxBy(_.start)
        dataMap(i.start) = DataRegion(i.regionIdentifier, i.start, i.size.max(highestRegion.end - i.start))
        dataMap(i.start)
      }
    }
    converted
  }

  // TODO: might need similar for stack regions
  def findLoadedWithPreDefined(n: CFGPosition, region: DataRegion): Set[DataRegion] = {
    // check if relocated
    val relocated = mmm.relocatedDataRegion(region.start)
    if (relocated.isDefined) {
      return Set(relocated.get)
    }
    Set(region)
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition, s: Set[DataRegion]): Set[DataRegion] = {
    n match {
      case memAssign: MemoryAssign =>
        return checkIfDefined(tryCoerceIntoData(memAssign.index, memAssign, memAssign.size), n)
      case assign: Assign =>
        val unwrapped = unwrapExpr(assign.rhs)
        if (unwrapped.isDefined) {
          var regions: Set[DataRegion] = tryCoerceIntoData(unwrapped.get.index, assign, unwrapped.get.size).flatMap(findLoadedWithPreDefined(n, _))
          // if regions is empty, it means that the load index must be a direct stack address
          if (regions.isEmpty) {
            regions = tryCoerceIntoData(assign.lhs, assign, unwrapped.get.size).flatMap(findLoadedWithPreDefined(n, _))
          }
          return checkIfDefined(regions, n)
        } else {
          // this is a constant but we need to check if it is a data region
          return checkIfDefined(tryCoerceIntoData(assign.rhs, assign, 1), n)
        }
      case _ =>
        Set()
    }
 }

  def transfer(n: CFGPosition, s: Set[DataRegion]): Set[DataRegion] = localTransfer(n, s)
}

class GlobalRegionAnalysisSolver(
    program: Program,
    domain: Set[CFGPosition],
    constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
    mmm: MemoryModelMap,
    vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]
  ) extends GlobalRegionAnalysis(program, domain, constantProp, reachingDefs, mmm, vsaResult)
  with IRIntraproceduralForwardDependencies
  with Analysis[Map[CFGPosition, Set[DataRegion]]]
  with SimpleWorklistFixpointSolver[CFGPosition, Set[DataRegion], PowersetLattice[DataRegion]]