package util


enum ProcRelyVersion:
  case Function, IfCommandContradiction
case class BoogieGeneratorConfig(memoryFunctionType: BoogieMemoryAccessMode = BoogieMemoryAccessMode.SuccessiveStoreSelect, coalesceConstantMemory: Boolean = true,
                                 procedureRely: Option[ProcRelyVersion] = None)
case class ILLoadingConfig(inputFile: String, relfFile: String, specFile: Option[String] = None, dumpIL: Option[String] = None, mainProcedureName: String = "main", procedureTrimDepth: Int = Int.MaxValue)
case class StaticAnalysisConfig(dumpILToPath: Option[String] = None, analysisResultsPath: Option[String] = None, analysisDotPath: Option[String] = None)
enum BoogieMemoryAccessMode:
  case SuccessiveStoreSelect, LambdaStoreSelect

case class BASILConfig(loading: ILLoadingConfig,
                       runInterpret: Boolean = false,
                       staticAnalysis: Option[StaticAnalysisConfig] = None,
                       boogieTranslation: BoogieGeneratorConfig = BoogieGeneratorConfig(),
                       outputPrefix: String,
                      )

