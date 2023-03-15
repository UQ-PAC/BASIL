//package analysis
//
//import ir._
//import analysis._
//import analysis.solvers._
//
//class VSA {
//
//}
//
//abstract class ValueSetAnalysis(cfg: Cfg) extends FlowSensitiveAnalysis(true) with ValueAnalysisMisc {
//  val lattice: MapLattice[CfgNode, statelattice.type] = new MapLattice(statelattice)
//  val domain: Set[CfgNode] = cfg.nodes
//
//  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element = {
//    n match {
//      case directCall: DirectCall =>
//        s
//
//      case indirectCall: IndirectCall =>
//        s
//
//      case goTo: GoTo =>
//        s
//
//      case memAssign: MemoryAssign =>
//        s
//
//      case localAssign: LocalAssign =>
//        s
//
//      case _ => s // ignore other kinds of nodes
//    }
//  }
//}
//
////  class ValueSetAnalysisSolver(cfg: IntraproceduralProgramCfg)
////    extends ValueSetAnalysis(cfg)
////      with SimpleWorklistFixpointSolver[CfgNode]
////      with ForwardDependencies
//
//// Define RegionType to be an enumeration of the different kinds of regions
//// that we want to track.  For example, we might want to track regions for
//// local variables, global variables, and heap objects.
//enum RegionType(name: String) {
//  case Local extends RegionType("Local")
//  case Global extends RegionType("Global")
//  case Heap extends RegionType("Heap")
//
//  override def toString: String = name
//}
//
//class MemoryRegion(regType: RegionType, id: Int) {
//  def regionType: RegionType = regType
//  def regionId: Int = id
//
//  override def equals(obj: Any): Boolean =
//    obj match {
//      case that: MemoryRegion =>
//        this.regionType == that.regionType && this.regionId == that.regionId
//      case _ => false
//    }
//
//  override def hashCode: Int =
//    (regionType.hashCode + regionId.hashCode).hashCode()
//
//  override def toString: String =
//    s"MemoryRegion(Type: $regionType, ID: $regionId)"
//}
//
//class ValueSet(tuple: (MemoryRegion, ValueSetLattice.type)) {
//  val regionMap: Map[MemoryRegion, ValueSetLattice.type] = Map[MemoryRegion, ValueSetLattice.type](tuple)
//
//}