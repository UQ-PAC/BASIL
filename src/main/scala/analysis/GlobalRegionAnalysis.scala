package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.*

import scala.collection.mutable

trait GlobalRegionAnalysis(val program: Program,
                           val domain: Set[CFGPosition],
                           val constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                           val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
                           val mmm: MemoryModelMap,
                           val globalOffsets: Map[BigInt, BigInt],
                           val vsaResult: Option[Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]]) {

  var dataCount: Int = 0
  private def nextDataCount() = {
    dataCount += 1
    s"data_$dataCount"
  }

  val regionLattice: PowersetLattice[DataRegion] = PowersetLattice()

  val lattice: MapLattice[CFGPosition, Set[DataRegion], PowersetLattice[DataRegion]] = MapLattice(regionLattice)

  val first: Set[CFGPosition] = Set.empty + program.mainProcedure

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)
  private val mallocVariable = Register("R0", 64)

  private val dataMap: mutable.HashMap[BigInt, DataRegion] = mutable.HashMap()

  private def dataPoolMaster(offset: BigInt, size: BigInt): Option[DataRegion] = {
    assert(size >= 0)
    if (dataMap.contains(offset)) {
      if (dataMap(offset).size < (size.toDouble / 8).ceil.toInt) {
        dataMap(offset) = DataRegion(dataMap(offset).regionIdentifier, offset, (size.toDouble / 8).ceil.toInt)
        Some(dataMap(offset))
      } else {
        Some(dataMap(offset))
      }
    } else {
      dataMap(offset) = DataRegion(nextDataCount(), offset, (size.toDouble / 8).ceil.toInt)
      Some(dataMap(offset))
    }
  }

  def getDataMap: mutable.HashMap[BigInt, DataRegion] = dataMap

  def resolveGlobalOffsetSecondLast(address: BigInt): BigInt = {
    var tableAddress = address
    // addresses may be layered as in jumptable2 example for which recursive search is required
    var exitLoop = false
    while (globalOffsets.contains(tableAddress) && globalOffsets.contains(globalOffsets(tableAddress)) && !exitLoop) {
      val newAddress = globalOffsets.getOrElse(tableAddress, tableAddress)
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
      val region = dataPoolMaster(eval.get.value, subAccess)
      if (region.isDefined) {
          return Set(region.get)
      }
    }
    exp match
      case literal: BitVecLiteral => tryCoerceIntoData(literal, n, subAccess)
      case Extract(end, start, body) => tryCoerceIntoData(body, n, subAccess)
      case Repeat(repeats, body) => tryCoerceIntoData(body, n, subAccess)
      case ZeroExtend(extension, body) => tryCoerceIntoData(body, n, subAccess)
      case SignExtend(extension, body) => tryCoerceIntoData(body, n, subAccess)
      case UnaryExpr(op, arg) => tryCoerceIntoData(arg, n, subAccess)
      case BinaryExpr(op, arg1, arg2) =>
        val evalArg2 = evaluateExpression(arg2, constantProp(n))
        if (evalArg2.isDefined) {
          val firstArg = tryCoerceIntoData(arg1, n, subAccess)
          var regions = Set.empty[DataRegion]
          for (i <- firstArg) {
//            if (globalOffsets.contains(i.start) && globalOffsets.contains(globalOffsets(i.start))) { // get the first base address
//              val newExpr = BinaryExpr(op, BitVecLiteral(globalOffsets(i.start), evalArg2.get.size), evalArg2.get)
//              regions = regions ++ tryCoerceIntoData(newExpr, n, subAccess)
//            } else {
//              val newExpr = BinaryExpr(op, BitVecLiteral(i.start, evalArg2.get.size), evalArg2.get)
//              regions = regions ++ tryCoerceIntoData(newExpr, n, subAccess)
//            }
            val newExpr = BinaryExpr(op, BitVecLiteral(resolveGlobalOffsetSecondLast(i.start), evalArg2.get.size), evalArg2.get)
            regions = regions ++ tryCoerceIntoData(newExpr, n, subAccess)
          }
          return regions
        }
        Set.empty
      case MemoryLoad(mem, index, endian, size) => ???
      case UninterpretedFunction(name, params, returnType) => Set.empty
      case variable: Variable =>
        val ctx = getUse(variable, n, reachingDefs)
        var collage = Set.empty[DataRegion]
        for (i <- ctx) {
          if (i != n) {
            var tryVisit = Set.empty[DataRegion]
            if (vsaResult.isDefined) {
              vsaResult.get.get(i) match
                case Some(value) => value match
                  case Lift(el) => el.get(i.lhs) match
                    case Some(value) => value.map {
                      case addressValue: AddressValue =>
                        // find what the region contains
                        vsaResult.get.get(i) match
                          case Some(value) => value match
                            case Lift(el) => el.get(addressValue.region) match
                              case Some(value) => value.map {
                                case addressValue: AddressValue =>
                                  addressValue.region match
                                    case region: DataRegion =>
                                      tryVisit = tryVisit + region
                                    case _ =>
                                case literalValue: LiteralValue =>
                              }
                              case None =>
                            case LiftedBottom =>
                            case _ =>
                          case None =>
                      case literalValue: LiteralValue =>
                    }
                    case None =>
                  case LiftedBottom =>
                  case _ =>
                case None =>
            }
            if (tryVisit.isEmpty) {
              tryVisit = localTransfer(i, Set.empty)
            }
            if (tryVisit.nonEmpty) {
              collage = collage ++ tryVisit
            }
          }
        }
        collage
      case _ => Set.empty
  }

  def evalMemLoadToGlobal(index: Expr, size: BigInt, n: Command): Set[DataRegion] = {
    val indexValue = evaluateExpression(index, constantProp(n))
    if (indexValue.isDefined) {
      val indexValueBigInt = indexValue.get.value
      val region = dataPoolMaster(indexValueBigInt, size)
      if (region.isDefined) {
        return Set(region.get)
      }
    }
    tryCoerceIntoData(index, n, size)
  }

//  def mergeRegions(regions: Set[DataRegion]): DataRegion = {
//    if (regions.size == 1) {
//      return regions.head
//    }
//    val start = regions.minBy(_.start).start
//    val end = regions.maxBy(_.end).end
//    val size = end - start
//    val newRegion = DataRegion(nextDataCount(), start, size)
//    regions.foreach(i => dataMap(i.start) = newRegion)
//    newRegion
//  }

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
    var returnSet = Set.empty[DataRegion]
    for (i <- dataRegions) {
      val (f, p) = mmm.findDataObjectWithSize(i.start, i.size)
      val accesses = f.union(p)
      if (accesses.isEmpty) {
        returnSet = returnSet + i
      } else {
          if (accesses.size == 1) {
            dataMap(i.start) = DataRegion(i.regionIdentifier, i.start, i.size.max(accesses.head.size))
            returnSet = returnSet + dataMap(i.start)
          } else if (accesses.size > 1) {
            val highestRegion = accesses.maxBy(_.start)
            dataMap(i.start) = DataRegion(i.regionIdentifier, i.start, i.size.max(highestRegion.end - i.start))
            returnSet = returnSet + dataMap(i.start)
          }
      }
    }
    if (returnSet.size > 1) {
      mmm.addMergeRegions(returnSet.asInstanceOf[Set[MemoryRegion]], nextDataCount())
    }
    returnSet
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition, s: Set[DataRegion]): Set[DataRegion] = {
    n match {
      case cmd: Command =>
        cmd match {
          case memAssign: MemoryAssign =>
            return checkIfDefined(evalMemLoadToGlobal(memAssign.index, memAssign.size, cmd), n)
          case assign: Assign =>
            val unwrapped = unwrapExpr(assign.rhs)
            if (unwrapped.isDefined) {
              return checkIfDefined(evalMemLoadToGlobal(unwrapped.get.index, unwrapped.get.size, cmd), n)
            } else {
              // this is a constant but we need to check if it is a data region
              return checkIfDefined(evalMemLoadToGlobal(assign.rhs, 1, cmd), n)
            }
          case _ =>
        }
      case _ =>
    }
    Set.empty
 }

  def transfer(n: CFGPosition, s: Set[DataRegion]): Set[DataRegion] = localTransfer(n, s)
}

class GlobalRegionAnalysisSolver(
    program: Program,
    domain: Set[CFGPosition],
    constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
    mmm: MemoryModelMap,
    globalOffsets: Map[BigInt, BigInt],
    vsaResult: Option[Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]]
  ) extends GlobalRegionAnalysis(program, domain, constantProp, reachingDefs, mmm, globalOffsets, vsaResult)
  with IRIntraproceduralForwardDependencies
  with Analysis[Map[CFGPosition, Set[DataRegion]]]
  with SimpleWorklistFixpointSolver[CFGPosition, Set[DataRegion], PowersetLattice[DataRegion]]