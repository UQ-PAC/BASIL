package test_util

import ir.{Block, Procedure, Program}
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

import java.io.File

trait TestUtil {
  val resultsFile = "_results"
  val resultParser: String => String = identity[String]
  val testPath = "./src/test/analysis/"
  val dumpPath = testPath + "dump/"
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

  def runTest(path: String, name: String, variation: String = "", example: Boolean = false): Unit = {
    var expected = ""
    var actual = ""

    val dumpFolder = File(dumpPath)
    if (!dumpFolder.exists) {
      dumpFolder.mkdir()
    }

    RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          adtFile = path + s"/$name" + (if !example then s"/$variation" else "") + s"/$name.adt",
          relfFile = path + s"/$name" + (if !example then s"/$variation" else "") + s"/$name.relf",
          specFile = None,
          dumpIL = None,
          mainProcedureName = "main",
        ),
        runInterpret = false,
        staticAnalysis = Some(StaticAnalysisConfig(analysisResultsPath = Some(dumpPath))),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )


    if example then
      try
        val expectedFile = scala.io.Source.fromFile(testPath + s"$name")
        expected = expectedFile.mkString
        expectedFile.close()
        expected = resultParser(expected)

        val actualFile = scala.io.Source.fromFile(dumpPath + resultsFile)
        actual = actualFile.mkString
        actualFile.close()
        actual = resultParser(actual)

      catch
        case e: Exception => throw e //new Exception(s"$path/$name Test Crashed", e)

      assert(actual == expected)
  }

}
