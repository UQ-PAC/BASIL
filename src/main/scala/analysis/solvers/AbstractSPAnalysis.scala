//package analysis.solvers
//
//import ir.*
//import analysis.solvers._
//import analysis.*
//
//import scala.collection.immutable
//import scala.collection.mutable
//
//
//class AbstractSP(val locations: Set[BitVecLiteral], val definitions: Set[LocalAssign]) {
//  override def toString: String = "AbstractSP(" + location + ")"
//
//  def add(that: BitVecLiteral, definer: Set[LocalAssign]): AbstractSP = {
//    val newLocations = locations.map(l => BitVectorEval.smt_bvadd(l, that))
//    AbstractSP(newLocations, definer)
//  }
//
//  def sub(that: BitVecLiteral, definer: Set[LocalAssign]): AbstractSP = {
//    val newLocations = locations.map(l => BitVectorEval.smt_bvsub(l, that))
//    AbstractSP(newLocations, definer)
//  }
//
//  def union(that: AbstractSP): AbstractSP = {
//    AbstractSP(locations ++ that.locations, definitions ++ that.definitions)
//  }
//}
//
//class TopAbstractSP extends AbstractSP(Set.empty, Set.empty) {
//  override def toString: String = "TopAbstractSP"
//}
//
//
///**
// * Tracks the stack pointer abstractly and offers calculations for the stack pointer.
// * Uses
// */
//trait AbstractSPAnalysis(program: Program, constantProp: Map[CFGPosition, Map[RegisterWrapperPartialEquality, Set[BitVecLiteral]]]) {
//
//  val mapLattice: MapLattice[RegisterWrapperPartialEquality, FlatElement[AbstractSP], FlatLattice[AbstractSP]] = MapLattice(AbstractSPLattice())
//
//  val lattice: MapLattice[CFGPosition, Map[RegisterWrapperPartialEquality, FlatElement[AbstractSP]], MapLattice[RegisterWrapperPartialEquality, FlatElement[AbstractSP], FlatLattice[AbstractSP]]] = MapLattice(mapLattice)
//
//  val domain: Set[CFGPosition] = Set.empty ++ program
//
//  private val stackPointer = Register("R31", BitVecType(64))
//
//  /** Default implementation of eval.
//    */
//  def eval(cmd: Command, s: Map[RegisterWrapperPartialEquality, FlatElement[AbstractSP]]): Map[RegisterWrapperPartialEquality, FlatElement[AbstractSP]] = {
//
//  }
//
//  /** Transfer function for state lattice elements.
//    */
//  def localTransfer(n: CFGPosition, s: Map[RegisterWrapperPartialEquality, FlatElement[AbstractSP]]): Map[RegisterWrapperPartialEquality, FlatElement[AbstractSP]] = n match {
//    case r: Command =>
//      r match {
//        // assignments
//        case la: LocalAssign =>
//          if (la.lhs == stackPointer) {
//            val reachingDefs = getDefinition(la.lhs, n, reachingDefs)
//            val rhs = eval(la.rhs, s, n, reachingDefs)
//            val rhsLocations = rhs.locations
//            val rhsDefinitions = rhs.definitions
//            val lhs = AbstractSP(rhsLocations, rhsDefinitions)
//            s + (la.lhs -> FlatEl(lhs))
//          } else {
//            s + (la.lhs -> eval(la.rhs, s))
//          }
//
//          val lhsWrappers = s.collect {
//            case (k, v) if RegisterWrapperPartialEquality(k.variable, k.assigns) == RegisterWrapperPartialEquality(la.lhs, getDefinition(la.lhs, r, reachingDefs)) => (k, v)
//          }
//          if (lhsWrappers.nonEmpty) {
//            s ++ lhsWrappers.map((k, v) => (RegisterWrapperEqualSets(k.variable, k.assigns ++ getDefinition(la.lhs, r, reachingDefs)), v.union(eval(la.rhs, s, r))))
//          } else {
//            s + (RegisterWrapperEqualSets(la.lhs, getDefinition(la.lhs, r, reachingDefs)) -> eval(la.rhs, s, n))
//          }
//        // all others: like no-ops
//        case _ => s
//      }
//    case _ => s
//  }
//
//  /** Transfer function for state lattice elements.
//      */
//  def transfer(n: CFGPosition, s: Map[RegisterWrapperPartialEquality, FlatElement[AbstractSP]]): Map[RegisterWrapperPartialEquality, FlatElement[AbstractSP]] = localTransfer(n, s)
//}
//
//class AbstractSPAnalysisSolver(program: Program, constantProp: Map[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]]) extends AbstractSPAnalysis(program, constantProp)
//    with IRIntraproceduralForwardDependencies
//    with Analysis[Map[RegisterWrapperPartialEquality, FlatElement[AbstractSP]]]
//    with SimpleWorklistFixpointSolver[CFGPosition, Map[RegisterWrapperPartialEquality, FlatElement[AbstractSP]], MapLattice[RegisterWrapperPartialEquality, FlatElement[AbstractSP], FlatLattice[AbstractSP]]] {
//}