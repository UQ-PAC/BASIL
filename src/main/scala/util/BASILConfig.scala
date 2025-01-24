package util

enum ProcRelyVersion {
  case Function, IfCommandContradiction
}

case class BoogieGeneratorConfig(
  memoryFunctionType: BoogieMemoryAccessMode = BoogieMemoryAccessMode.SuccessiveStoreSelect,
  coalesceConstantMemory: Boolean = true,
  procedureRely: Option[ProcRelyVersion] = None,
  threadSplit: Boolean = false
)

case class ILLoadingConfig(
  inputFile: String,
  relfFile: String,
  specFile: Option[String] = None,
  dumpIL: Option[String] = None,
  mainProcedureName: String = "main",
  procedureTrimDepth: Int = Int.MaxValue
)

case class StaticAnalysisConfig(
  dumpILToPath: Option[String] = None,
  analysisResultsPath: Option[String] = None,
  analysisDotPath: Option[String] = None,
  threadSplit: Boolean = false,
  summariseProcedures: Boolean = false,
  memoryRegions: MemoryRegionsMode = MemoryRegionsMode.Disabled,
  irreducibleLoops: Boolean = true,
  indirectCalls: Boolean = false,
)

enum BoogieMemoryAccessMode {
  case SuccessiveStoreSelect, LambdaStoreSelect
}

enum MemoryRegionsMode {
  case DSA, MRA, Disabled
}

case class BASILConfig(
  loading: ILLoadingConfig,
  runInterpret: Boolean = false,
  staticAnalysis: Option[StaticAnalysisConfig] = None,
  boogieTranslation: BoogieGeneratorConfig = BoogieGeneratorConfig(),
  outputPrefix: String
)
