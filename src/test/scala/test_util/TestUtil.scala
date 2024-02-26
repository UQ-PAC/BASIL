package test_util

import ir.{Block, Procedure, Program}
import util.{BASILConfig, BasilResult, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

import java.io.File

trait TestUtil {
  val examplePath = "./examples/"
  val correctPath = "./src/test/correct/"
  val correctPrograms: Array[String] = getSubdirectories(correctPath)
  val incorrectPath = "./src/test/incorrect/"
  val incorrectPrograms: Array[String] = getSubdirectories(incorrectPath)
  extension (p: Program)
    def procs: Map[String, Procedure] = p.collect {
      case b: Procedure => b.name -> b
    }.toMap

    def blocks: Map[String, Block] = p.collect {
      case b: Block => b.label -> b
    }.toMap

  def getSubdirectories(directoryName: String): Array[String] = {
    File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }


  def runExample(name: String, path: String = correctPath, variation: String = "gcc/"): BasilResult = {
    RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          adtFile = path + s"/$name/$variation$name.adt",
          relfFile = path + s"/$name/$variation$name.relf",
          specFile = None,
          dumpIL = None,
          mainProcedureName = "main",
        ),
        runInterpret = false,
        staticAnalysis = Some(StaticAnalysisConfig(None)),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
  }
}
