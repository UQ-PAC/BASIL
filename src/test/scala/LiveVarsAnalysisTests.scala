import analysis.Top
import ir.{BitVecType, Register, Variable}
import org.scalatest.funsuite.AnyFunSuite
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

import java.io.{BufferedWriter, File, FileWriter}

class LiveVarsAnalysisTests extends AnyFunSuite {

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))

  private val initialized: Set[Variable] = Set(stackPointer, linkRegister, framePointer)

  val testPath = "./src/test/"
  val correctPath = "./src/test/correct"
  val correctPrograms: Array[String] = getSubdirectories(correctPath)
  val incorrectPath = "./src/test/incorrect"
  val incorrectPrograms: Array[String] = getSubdirectories(incorrectPath)

  // get all variations of each program
  for (p <- correctPrograms) {
    val path = correctPath + "/" + p
    val variations = getSubdirectories(path)
    variations.foreach(t =>
      test("correct/" + p + "/" + t) {
        runTest(correctPath, p, t)
      }
    )
  }

  for (p <- incorrectPrograms) {
    val path = incorrectPath + "/" + p
    val variations = getSubdirectories(path)
    variations.foreach(t =>
      test("incorrect/" + p + "/" + t) {
        runTest(incorrectPath, p, t)
      }
    )
  }

  def runTest(path: String, name: String, variation: String): Unit = {
    RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          adtFile = path + s"/$name" + s"/$variation" + s"/$name.adt",
          relfFile = path + s"/$name" + s"/$variation" + s"/$name.relf",
          specFile = None,
          dumpIL = None,
          mainProcedureName = "main",
        ),
        runInterpret = false,
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )

    RunUtils.liveVarAnalysisResults(RunUtils.programIR.mainProcedure).foreach{
      case (variable, Top) if initialized.contains(variable) =>
      case _ =>
//        println(RunUtils.liveVarAnalysisResults(RunUtils.programIR.mainProcedure))
//        assert(false)
    }

  }


  def getSubdirectories(directoryName: String): Array[String] = {
    File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }
}
