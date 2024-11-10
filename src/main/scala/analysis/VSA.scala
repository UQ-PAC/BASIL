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

trait ValueSetAnalysis(program: Program,
                       mmm: MemoryModelMap,
                       constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) {

  val powersetLattice: PowersetLattice[Value] = PowersetLattice()

  val mapLattice: MapLattice[Variable | MemoryRegion, Set[Value], PowersetLattice[Value]] = MapLattice(powersetLattice)

  val liftedLattice: LiftLattice[Map[Variable | MemoryRegion, Set[Value]], mapLattice.type] = LiftLattice(mapLattice)

  val lattice: MapLattice[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]], LiftLattice[Map[Variable | MemoryRegion, Set[Value]], mapLattice.type]] = MapLattice(liftedLattice)

  val first: Set[CFGPosition] = Set(program.mainProcedure)

  private val mallocVariable = Register("R0", 64)

  def canCoerceIntoDataRegion(bitVecLiteral: BitVecLiteral, size: Int): Option[DataRegion] = {
    mmm.findDataObject(bitVecLiteral.value)
  }

  def findLoadedWithPreDefined(s: Map[Variable | MemoryRegion, Set[Value]], region: MemoryRegion, n: CFGPosition): Set[MemoryRegion] = {
    // check if relocated
    if (region.isInstanceOf[DataRegion]) {
      val relocated = mmm.relocatedDataRegion(region.start)
      if (relocated.isDefined) {
        return Set(relocated.get)
      }
    }
    // get only the address values
    val vsaDetermined = s(region).collect { case a: AddressValue => a.region }
    if (vsaDetermined.nonEmpty) {
      vsaDetermined
    } else {
      Set(region) // TODO: works for syscall:clang_O2 but it is a memLoad so it should be getting what is loaded?
    }
  }

  /** Default implementation of eval.
    */
  def eval(cmd: Command, s: Map[Variable | MemoryRegion, Set[Value]], n: CFGPosition): Map[Variable | MemoryRegion, Set[Value]] = {
    cmd match
      case directCall: DirectCall if directCall.target.name == "malloc" =>
        val regions = mmm.nodeToRegion(n)
        // malloc variable
        s + (mallocVariable -> regions.map(r => AddressValue(r)))
      case localAssign: Assign =>
        val regions = mmm.nodeToRegion(n).flatMap(r => findLoadedWithPreDefined(s, r, n))
        if (regions.nonEmpty) {
          s + (localAssign.lhs -> regions.map(r => AddressValue(r)))
        } else {
          evaluateExpression(localAssign.rhs, constantProp(n)) match {
            case Some(bitVecLiteral: BitVecLiteral) =>
              val possibleData = canCoerceIntoDataRegion(bitVecLiteral, 1)
                if (possibleData.isDefined) {
                  s + (localAssign.lhs -> Set(AddressValue(possibleData.get)))
                } else {
                  s + (localAssign.lhs -> Set(LiteralValue(bitVecLiteral)))
                }
            case None =>
              val unwrapValue = unwrapExprToVar(localAssign.rhs)
              unwrapValue match {
                case Some(v: Variable) =>
                  s + (localAssign.lhs -> s(v))
                case None =>
                  Logger.debug(s"Too Complex: ${localAssign.rhs}") // do nothing
                  s
              }
          }
        }
      case memAssign: MemoryAssign =>
        val regions = mmm.nodeToRegion(n)
        evaluateExpression(memAssign.value, constantProp(n)) match {
          case Some(bitVecLiteral: BitVecLiteral) =>
            val possibleData = canCoerceIntoDataRegion(bitVecLiteral, memAssign.size)
            if (possibleData.isDefined) {
              s ++ regions.map(r => r -> Set(AddressValue(possibleData.get)))
            } else {
              s ++ regions.map(r => r -> Set(LiteralValue(bitVecLiteral)))
            }
          case None =>
            val unwrapValue = unwrapExprToVar(memAssign.value)
            unwrapValue match {
              case Some(v: Variable) =>
                s ++ regions.map(r => r -> s(v))
              case None =>
                Logger.debug(s"Too Complex: $memAssign.value") // do nothing
                s
            }
        }
      case _ =>
        s
  }

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CFGPosition, s: Map[Variable | MemoryRegion, Set[Value]]): Map[Variable | MemoryRegion, Set[Value]] = {
    n match {
      case p: Procedure =>
        mmm.pushContext(p.name)
        s
      case _: Return =>
        mmm.popContext()
        s
      case command: Command =>
        eval(command, s, n)
      case _ =>
        s
    }
  }

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transferUnlifted(n: CFGPosition, s: Map[Variable | MemoryRegion, Set[Value]]): Map[Variable | MemoryRegion, Set[Value]] = localTransfer(n, s)
}

class ValueSetAnalysisSolver(
    program: Program,
    mmm: MemoryModelMap,
    constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
) extends ValueSetAnalysis(program, mmm, constantProp)
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
