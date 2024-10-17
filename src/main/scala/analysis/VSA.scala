package analysis

import ir._
import analysis.solvers._

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable
import util.Logger

/** ValueSets are PowerSet of possible values */
trait Value {
}

case class AddressValue(region: MemoryRegion) extends Value {
  override def toString: String = "Address(" + region + ")"
}

case class LiteralValue(expr: BitVecLiteral) extends Value {
  override def toString: String = "Literal(" + expr + ")"
}

trait ValueSetAnalysis(domain: Set[CFGPosition],
                        program: Program,
                        mmm: MemoryModelMap,
                        constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                        reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]) {

  val powersetLattice: PowersetLattice[Value] = PowersetLattice()

  val mapLattice: MapLattice[Variable | MemoryRegion, Set[Value], PowersetLattice[Value]] = MapLattice(powersetLattice)

  val liftedLattice: LiftLattice[Map[Variable | MemoryRegion, Set[Value]], mapLattice.type] = LiftLattice(mapLattice)

  val lattice: MapLattice[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]], LiftLattice[Map[Variable | MemoryRegion, Set[Value]], mapLattice.type]] = MapLattice(liftedLattice)

  val first: Set[CFGPosition] = Set.empty + program.mainProcedure

  private val mallocVariable = Register("R0", 64)

  def nodeToRegion(n: CFGPosition): Set[MemoryRegion] = {
    var returnRegions = Set.empty[MemoryRegion]
    n match {
      case directCall: DirectCall =>
        returnRegions = returnRegions + mmm.getHeap(directCall).asInstanceOf[MemoryRegion]
      case _ =>
        returnRegions = returnRegions ++ mmm.getStack(n).asInstanceOf[Set[MemoryRegion]] ++ mmm.getData(n).asInstanceOf[Set[MemoryRegion]]
    }
    returnRegions
  }

  def canCoerceIntoDataRegion(bitVecLiteral: BitVecLiteral): Option[DataRegion] = {
    mmm.isDataBase(bitVecLiteral.value)
  }

  /** Default implementation of eval.
    */
  def eval(cmd: Command, s: Map[Variable | MemoryRegion, Set[Value]], n: CFGPosition): Map[Variable | MemoryRegion, Set[Value]] = {
    var m = s
    cmd match
      case directCall: DirectCall if directCall.target.name == "malloc" =>
        val regions = nodeToRegion(n)
        // malloc variable
        m = m + (mallocVariable -> regions.map(r => AddressValue(r)))
        m
      case localAssign: Assign =>
        val regions = nodeToRegion(n)
        if (regions.nonEmpty) {
          m = m + (localAssign.lhs -> regions.map(r => AddressValue(r)))
        } else {
          evaluateExpression(localAssign.rhs, constantProp(n)) match
            case Some(bitVecLiteral: BitVecLiteral) =>
              val possibleData = canCoerceIntoDataRegion(bitVecLiteral)
              if (possibleData.isDefined) {
                m = m + (localAssign.lhs -> Set(AddressValue(possibleData.get)))
              } else {
                m = m + (localAssign.lhs -> Set(LiteralValue(bitVecLiteral)))
              }
            case None =>
              val unwrapValue = unwrapExprToVar(localAssign.rhs)
              unwrapValue match {
                case Some(v: Variable) =>
                  m = m + (localAssign.lhs -> m(v))
                case None =>
                  Logger.debug(s"Too Complex: $localAssign.rhs") // do nothing
              }
        }
        m
      case memAssign: MemoryAssign =>
        val regions = nodeToRegion(n)
        evaluateExpression(memAssign.value, constantProp(n)) match
          case Some(bitVecLiteral: BitVecLiteral) =>
            regions.foreach { r =>
              val possibleData = canCoerceIntoDataRegion(bitVecLiteral)
              if (possibleData.isDefined) {
                m = m + (r -> Set(AddressValue(possibleData.get)))
              } else {
                m = m + (r -> Set(LiteralValue(bitVecLiteral)))
              }
            }
          case None =>
            val unwrapValue = unwrapExprToVar(memAssign.value)
            unwrapValue match {
              case Some(v: Variable) =>
                regions.foreach { r =>
                  m = m + (r -> m(v))
                }
              case None =>
                Logger.debug(s"Too Complex: $memAssign.value") // do nothing
            }
        m
      case _ =>
        m
  }

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CFGPosition, s: Map[Variable | MemoryRegion, Set[Value]]): Map[Variable | MemoryRegion, Set[Value]] =
    if (IRWalk.procedure(n) == n) {
      mmm.pushContext(n.asInstanceOf[Procedure].name)
      s
    } else if (IRWalk.lastInProc(IRWalk.procedure(n)) == n) {
      mmm.popContext()
      s
    } else n match
      case command: Command =>
        eval(command, s, n)
      case _ =>
        s

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transferUnlifted(n: CFGPosition, s: Map[Variable | MemoryRegion, Set[Value]]): Map[Variable | MemoryRegion, Set[Value]] = localTransfer(n, s)
}

class ValueSetAnalysisSolver(
    domain: Set[CFGPosition],
    program: Program,
    mmm: MemoryModelMap,
    constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
) extends ValueSetAnalysis(domain, program, mmm, constantProp, reachingDefs)
    with IRIntraproceduralForwardDependencies
    with Analysis[Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]]
    with WorklistFixpointSolverWithReachability[CFGPosition, Map[Variable | MemoryRegion, Set[Value]], MapLattice[Variable | MemoryRegion, Set[Value], PowersetLattice[Value]]] {

  override def funsub(n: CFGPosition, x: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]): LiftedElement[Map[Variable | MemoryRegion, Set[Value]]] = {
    n match {
      // function entry nodes are always reachable as this is intraprocedural
      case _: Procedure => liftedLattice.lift(mapLattice.bottom)
      // all other nodes are processed with join+transfer
      case _ => super.funsub(n, x)
    }
  }
}
