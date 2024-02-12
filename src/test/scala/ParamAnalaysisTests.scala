import analysis.ParamAnalysis
import ir.{Block, Procedure, Program}
import org.scalatest.funsuite.AnyFunSuite
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

import java.io.File

class ParamAnalaysisTests extends AnyFunSuite{

  extension (p: Program)
    def procs: Map[String, Procedure] = p.collect {
      case b: Procedure => b.name -> b
    }.toMap

    def blocks: Map[String, Block] = p.collect {
      case b: Block => b.label -> b
    }.toMap

  val testPath = "./src/test/analysis/params/"
  val dumpPath = testPath + "dump/"
  val examplePath = "./examples/"
  val correctPath = "./src/test/correct/"
  val correctPrograms: Array[String] = getSubdirectories(correctPath)
  val incorrectPath = "./src/test/incorrect/"
  val incorrectPrograms: Array[String] = getSubdirectories(incorrectPath)

  // get all variations of each program
  for (p <- correctPrograms) {
    val path = correctPath + p
    val variations = getSubdirectories(path)
//    variations.foreach(t =>
//      test("correct/" + p + "/" + t) {
//        runTest(correctPath, p, t)
//      }
//    )
  }

  for (p <- incorrectPrograms) {
    val path = incorrectPath + p
    val variations = getSubdirectories(path)
//    variations.foreach(t =>
////      test("incorrect/" + p + "/" + t) {
////        runTest(incorrectPath, p, t)
////      }
//    )
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
        expected = ParamAnalysis.parseAnalysisResults(expected)


        val actualFile = scala.io.Source.fromFile(dumpPath + "paramAnalysisResults.txt")
        actual = actualFile.mkString
        actualFile.close()
        actual = ParamAnalysis.parseAnalysisResults(actual)


      catch
        case e: Exception => throw e //new Exception(s"$path/$name Test Crashed", e)

      assert(actual == expected)
  }


  test("basic_array_write") {
    runTest(examplePath, "basic_arrays_write", example = true)
  }

  test("function") {
    runTest(examplePath, "function", example = true)
  }

  test("basic_function_call_caller") {
    runTest(examplePath, "basic_function_call_caller", example = true)
  }

  test("function1") {
    runTest(examplePath, "function1", example = true)
  }

  test("ifbranches") {
    runTest(examplePath, "ifbranches", example = true)
  }


  def getSubdirectories(directoryName: String): Array[String] = {
    File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }
}
