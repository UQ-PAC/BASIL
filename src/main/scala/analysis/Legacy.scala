package analysis

import analysis.Interval as _
import analysis.data_structure_analysis.*
import ir.*

import scala.jdk.CollectionConverters.*

case class StaticAnalysisContext(
  intraProcConstProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  interProcConstProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  memoryRegionResult: Map[CFGPosition, ((Set[StackRegion], Set[Variable]), Set[HeapRegion])],
  vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]],
  interLiveVarsResults: Map[CFGPosition, Map[Variable, TwoElement]],
  paramResults: Map[Procedure, Set[Variable]],
  steensgaardResults: Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]],
  mmmResults: MemoryModelMap,
  reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
  regionInjector: Option[RegionInjector],
  symbolicAddresses: Map[CFGPosition, Map[SymbolicAddress, TwoElement]],
  localDSA: Map[Procedure, Graph],
  bottomUpDSA: Map[Procedure, Graph],
  topDownDSA: Map[Procedure, Graph],
  writesToResult: Map[Procedure, Set[GlobalVar]],
  ssaResults: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
)
