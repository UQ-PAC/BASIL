package util

/**
 * We use 'quirks' to manage the operation mode of the tool e.g. for a-b testing or compiler options.
 */


case class ILLoadingConfig(adtFile: String, relfFile: String, specFile: Option[String], dumpIL: Boolean)
case class BoogieGeneratorConfig(memoryFunctionType: BoogieMemoryAccessMode = BoogieMemoryAccessMode.SuccessiveStoreSelect)
case class StaticAnalysisConfig(dumpILEveryPhase: Boolean = false)
enum BoogieMemoryAccessMode:
  case SuccessiveStoreSelect, LambdaStoreSelect

case class BASILConfig(
                   loading: ILLoadingConfig,
                   runInterpret: Boolean = false,
                   staticAnalysis: Option[StaticAnalysisConfig] = None,
                   boogieTranslation: BoogieGeneratorConfig = BoogieGeneratorConfig(),
                   outputPrefix: String
                 )

