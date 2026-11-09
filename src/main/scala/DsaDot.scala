import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

object DsaDot {
  def main(args: Array[String]): Unit = {
    val path = args(1)
    val relf = args(2)

    val result = RunUtils.loadAndTranslate(
      BASILConfig(
        loading =
          ILLoadingConfig(inputFile = path, relfFile = Some(relf), specFile = None, dumpIL = None),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out"
      )
    )
    val program = result.ir.program

    println(result.analysis.get.topDownDSA(program.mainProcedure).toDot)
    ()
  }
}
