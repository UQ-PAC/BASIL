package analysis

import analysis.given
import analysis.solvers.SimpleWorklistFixpointSolver
import ir.*
import util.assertion.*

import scala.collection.mutable

/** Identifies global data regions.
  *
  * This is iterated until results reach a fixpoint.
  *
  * @param program
  * @param domain
  *   reachable parts of the program
  * @param constantProp
  *   constant propagation results
  * @param reachingDefs
  *   maps each CFG node to two maps: variable definitions and variables uses.
  * @param mmm
  *   preloaded globals from symbol table.
  * @param vsaResult
  *   extra information from VSA results of previous passes.
  */
trait GlobalRegionAnalysis(
  val program: Program,
  val domain: Set[CFGPosition],
  val constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
  val mmm: MemoryModelMap,
  val vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]
) {

  private var dataCount: Int = 0
  private def nextDataCount() = {
    dataCount += 1
    s"data_$dataCount"
  }

  val regionLattice: PowersetLattice[DataRegion] = PowersetLattice()

  val lattice: MapLattice[CFGPosition, Set[DataRegion]] = MapLattice(regionLattice)

  val first: Set[CFGPosition] = Set(program.mainProcedure)

  private val dataMap: mutable.HashMap[BigInt, DataRegion] = mutable.HashMap()

  private def dataPoolMaster(offset: BigInt, size: BigInt, convertSize: Boolean = true): DataRegion = {
    debugAssert(size >= 0)
    val givenSize: Int = if (convertSize) (size.toDouble / 8).ceil.toInt else size.toInt
    if (dataMap.contains(offset)) {
      if (dataMap(offset).size < givenSize) {
        dataMap(offset) = DataRegion(dataMap(offset).regionIdentifier, offset, givenSize)
        dataMap(offset)
      } else {
        dataMap(offset)
      }
    } else {
      dataMap(offset) = DataRegion(nextDataCount(), offset, givenSize)
      dataMap(offset)
    }
  }

  def getDataMap: mutable.HashMap[BigInt, DataRegion] = dataMap

  def tryCoerceIntoData(exp: Expr, n: Command, subAccess: BigInt, noLoad: Boolean = false): Set[DataRegion] = {
    val eval = evaluateExpression(exp, constantProp(n))
    if (eval.isDefined) {
      val index = eval.get.value
      Set(dataPoolMaster(index, subAccess, convertSize = !noLoad))
    } else {
      exp match {
        case literal: BitVecLiteral => tryCoerceIntoData(literal, n, subAccess, noLoad)
        case Extract(_, _, body) => tryCoerceIntoData(body, n, subAccess, noLoad)
        case Repeat(_, body) => tryCoerceIntoData(body, n, subAccess, noLoad)
        case ZeroExtend(_, body) => tryCoerceIntoData(body, n, subAccess, noLoad)
        case SignExtend(_, body) => tryCoerceIntoData(body, n, subAccess, noLoad)
        case UnaryExpr(_, arg) => tryCoerceIntoData(arg, n, subAccess, noLoad)
        case BinaryExpr(op, arg1, arg2) =>
          val evalArg2 = evaluateExpression(arg2, constantProp(n))
          if (evalArg2.isDefined) {
            tryCoerceIntoData(arg1, n, subAccess, noLoad) flatMap { i =>
              val newExpr = BinaryExpr(op, BitVecLiteral(i.start, evalArg2.get.size), evalArg2.get)
              tryCoerceIntoData(newExpr, n, subAccess, noLoad)
            }
          } else {
            Set()
          }
        case _: FApplyExpr => Set.empty
        case variable: Variable =>
          if (noLoad) {
            Set()
          } else {
            val uses = getUse(variable, n, reachingDefs)
            uses.flatMap(i => getVSAHints(variable, subAccess, i))
          }
        case _ => Set()
      }
    }
  }

  def getVSAHints(variable: Variable, subAccess: BigInt, n: CFGPosition): Set[DataRegion] = {
    val collage: Set[DataRegion] = vsaResult.get(n) match {
      case Some(Lift(el)) =>
        el.getOrElse(variable, Set()).flatMap {
          case AddressValue(dataRegion2: DataRegion) => Some(dataPoolMaster(dataRegion2.start, subAccess))
          case _ => Set()
        }
      case _ => Set()
    }
    collage
  }

  /** Check if the data region is defined. Finds full and partial matches Full matches sizes are altered to match the
    * size of the data region Partial matches are not altered Otherwise the data region is returned
    *
    * @param dataRegions
    *   Set[DataRegion]
    * @param n
    *   CFGPosition
    * @return
    *   Set[DataRegion]
    */
  def checkIfDefined(dataRegions: Set[DataRegion], n: CFGPosition, strict: Boolean = false): Set[DataRegion] = {
    var converted: Set[DataRegion] = Set.empty
    dataRegions.foreach { i =>
      val (f, p) = mmm.findDataObjectWithSize(i.start, i.size)
      val accesses = f.union(p)
      if (accesses.isEmpty) {
        i
      } else if (accesses.size == 1) {
        if (f.contains(accesses.head)) {
          // full access
          if (strict && i.size == 1) {
            dataPoolMaster(i.start, accesses.head.size, false)
          } else {
            dataPoolMaster(i.start, i.size, false)
          }
        } else {
          // partial access (we cannot determine the size)
          if (i.start == accesses.head.start && i.size == 1 && strict) {
            dataPoolMaster(i.start, accesses.head.size, false)
          } else {
            dataPoolMaster(i.start, i.size, false)
          }
        }
        converted = converted ++ Set(dataMap(i.start))
      } else {
        //        val highestRegion = accesses.maxBy(_.start)
        //        dataMap(i.start) = DataRegion(i.regionIdentifier, i.start, i.size.max(highestRegion.end - i.start))
        //        dataMap(i.start)
        dataMap.remove(i.start)
        accesses.foreach(a => dataPoolMaster(a.start, a.size, false))
        converted = converted ++ accesses.map(a => dataMap(a.start))
      }
    }
    converted
  }

  /** Transfer function for state lattice elements.
    */
  def transfer(n: CFGPosition, s: Set[DataRegion]): Set[DataRegion] = {
    n match {
      case store: MemoryStore =>
        checkIfDefined(tryCoerceIntoData(store.index, store, store.size), n)
      case load: MemoryLoad =>
        val regions: Set[DataRegion] = tryCoerceIntoData(load.index, load, load.size)
        checkIfDefined(regions, n)
      case assign: LocalAssign =>
        // this is a constant but we need to check if it is a data region
        // TODO aefault value of 1 here causes problems as 1 is both used as a default and is also
        //  a valid size for 1-byte accesses
        checkIfDefined(tryCoerceIntoData(assign.rhs, assign, 1, noLoad = true), n, strict = true)
      case _ =>
        Set()
    }
  }

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
    with SimpleWorklistFixpointSolver[CFGPosition, Set[DataRegion]]
