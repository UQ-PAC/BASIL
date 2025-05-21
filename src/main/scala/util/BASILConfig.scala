package util

import analysis.data_structure_analysis.DSAPhase

enum ProcRelyVersion {
  case Function, IfCommandContradiction
}

enum PCTrackingOption {
  case None, Keep, Assert
}

case class BoogieGeneratorConfig(
  memoryFunctionType: BoogieMemoryAccessMode = BoogieMemoryAccessMode.SuccessiveStoreSelect,
  coalesceConstantMemory: Boolean = true,
  procedureRely: Option[ProcRelyVersion] = None,
  threadSplit: Boolean = false,
  directTranslation: Boolean = false
)

case class ILLoadingConfig(
  inputFile: String,
  relfFile: String,
  specFile: Option[String] = None,
  dumpIL: Option[String] = None,
  mainProcedureName: String = "main",
  procedureTrimDepth: Int = Int.MaxValue,
  parameterForm: Boolean = false,
  trimEarly: Boolean = false,
  pcTracking: PCTrackingOption = PCTrackingOption.None
)

case class StaticAnalysisConfig(
  dumpILToPath: Option[String] = None,
  analysisResultsPath: Option[String] = None,
  analysisDotPath: Option[String] = None,
  threadSplit: Boolean = false,
  memoryRegions: MemoryRegionsMode = MemoryRegionsMode.Disabled,
  irreducibleLoops: Boolean = true
)


case class DSConfig(
  phase: DSAPhase = DSAPhase.TD,
  splitGlobals: Boolean = false,
  globalAsserts: Boolean = false,
  eqClasses: Boolean = false,
  checks: Boolean = true
)



enum BoogieMemoryAccessMode {
  case SuccessiveStoreSelect, LambdaStoreSelect
}

enum MemoryRegionsMode {
  case DSA, MRA, Disabled
}

case class BASILConfig(
  context: Option[IRContext] = None,
  loading: ILLoadingConfig,
  runInterpret: Boolean = false,
  simplify: Boolean = false,
  validateSimp: Boolean = false,
  dsaConfig: Option[DSConfig] = None,
  memoryTransform: Boolean = false,
  summariseProcedures: Boolean = false,
  generateRelyGuarantees: Boolean = false,
  staticAnalysis: Option[StaticAnalysisConfig] = None,
  boogieTranslation: BoogieGeneratorConfig = BoogieGeneratorConfig(),
  outputPrefix: String
)
