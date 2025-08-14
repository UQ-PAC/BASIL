package util

import ir.IRContext
import ir.FrontendMode

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
  relfFile: Option[String] = None,
  specFile: Option[String] = None,
  dumpIL: Option[String] = None,
  mainProcedureName: String = "main",
  procedureTrimDepth: Int = Int.MaxValue,
  parameterForm: Boolean = false,
  trimEarly: Boolean = false,
  gtirbLiftOffline: Boolean = false,
  pcTracking: PCTrackingOption = PCTrackingOption.None
) {
  lazy val frontendMode =
    if inputFile.endsWith(".gts") then {
      FrontendMode.Gtirb
    } else if inputFile.endsWith(".gtirb") then {
      FrontendMode.Gtirb
    } else if inputFile.endsWith(".adt") then {
      FrontendMode.Bap
    } else if (inputFile.endsWith(".il")) {
      FrontendMode.Basil
    } else {
      throw Exception(s"input file name ${inputFile} must be an .adt, .gts or .gtirb file")
    }
}

case class StaticAnalysisConfig(
  dumpILToPath: Option[String] = None,
  analysisResultsPath: Option[String] = None,
  analysisDotPath: Option[String] = None,
  threadSplit: Boolean = false,
  memoryRegions: MemoryRegionsMode = MemoryRegionsMode.Disabled,
  irreducibleLoops: Boolean = true
)

enum DSAPhase {
  case Pre, Local, BU, TD
}

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
  tvSimp: Boolean = false,
  dsaConfig: Option[DSConfig] = None,
  summariseProcedures: Boolean = false,
  generateLoopInvariants: Boolean = false,
  generateRelyGuarantees: Boolean = false,
  memoryTransform: Boolean = false,
  assertCalleeSaved: Boolean = false,
  staticAnalysis: Option[StaticAnalysisConfig] = None,
  boogieTranslation: BoogieGeneratorConfig = BoogieGeneratorConfig(),
  outputPrefix: String
)
