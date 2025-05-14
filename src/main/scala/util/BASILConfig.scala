package util

enum ProcRelyVersion {
  case Function, IfCommandContradiction
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
  trimEarly: Boolean = false
)

case class StaticAnalysisConfig(
  dumpILToPath: Option[String] = None,
  analysisResultsPath: Option[String] = None,
  analysisDotPath: Option[String] = None,
  threadSplit: Boolean = false,
  memoryRegions: MemoryRegionsMode = MemoryRegionsMode.Disabled,
  irreducibleLoops: Boolean = true
)

enum DSAConfig {
  case Prereq, Standard, Checks
}

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
  tvSimp: Boolean = false,
  dsaConfig: Option[DSAConfig] = None,
  summariseProcedures: Boolean = false,
  generateRelyGuarantees: Boolean = false,
  memoryTransform: Boolean = false,
  staticAnalysis: Option[StaticAnalysisConfig] = None,
  boogieTranslation: BoogieGeneratorConfig = BoogieGeneratorConfig(),
  outputPrefix: String
)
