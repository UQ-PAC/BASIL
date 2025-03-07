package test_util
import java.io.{BufferedWriter, File, FileWriter}

import ir.{Block, Procedure, Program}
import util.{BASILConfig, BASILResult, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

import org.scalatest.{Retries, Failed, Exceptional, Pending, Canceled, Succeeded}
import org.scalatest.funsuite.AnyFunSuite

import java.io.File

trait TestUtil {
  val correctPath = "./src/test/correct/"
  val correctPrograms: Array[String] = getSubdirectories(correctPath)
  val incorrectPath = "./src/test/incorrect/"
  val incorrectPrograms: Array[String] = getSubdirectories(incorrectPath)
  extension (p: Program) {
    def procs: Map[String, Procedure] = p.collect { case b: Procedure =>
      b.name -> b
    }.toMap

    def blocks: Map[String, Block] = p.collect { case b: Block =>
      b.label -> b
    }.toMap
  }

  def getSubdirectories(directoryName: String): Array[String] = {
    File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }

  def runExample(name: String, path: String = correctPath, variation: String = "gcc/"): BASILResult = {
    RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = path + s"/$name/$variation$name.adt",
          relfFile = path + s"/$name/$variation$name.relf",
          specFile = None,
          dumpIL = None
        ),
        staticAnalysis = Some(StaticAnalysisConfig(None)),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out"
      )
    )
  }
}

/** @param directoryName
  *   of the parent directory
  * @return
  *   the names all subdirectories of the given parent directory
  */
def getSubdirectories(directoryName: String): Array[String] = {
  Option(File(directoryName).listFiles(_.isDirectory)) match {
    case None => throw java.io.IOException(s"failed to read directory '$directoryName'")
    case Some(subdirs) => subdirs.map(_.getName)
  }
}

def log(text: String, path: String): Unit = {
  val writer = BufferedWriter(FileWriter(path, false))
  writer.write(text)
  writer.flush()
  writer.close()
}

/**
 * A mixin for TestSuite (including AnyFunSuite) which allows
 * for customisation of expected test outcomes, based on the string
 * name of a test case. This is most useful in conjunction with
 * dynamically-generated test cases. For manually-written tests, it is
 * usually easier and clearer to make the modifications directly in
 * the test case.
 *
 * Users should implement the customiseTestsByName method (see doc
 * comment below).
 *
 */
trait TestCustomisation extends AnyFunSuite with Retries {

  enum Mode(val reason: Option[String]):
    case Normal extends Mode(None)
    case Retry(s: String) extends Mode(Some(s))
    case ExpectFailure(s: String) extends Mode(Some(s))
    case Disabled(s: String) extends Mode(Some(s))

    /**
     * Simple chaining of customisation modes. The current
     * mode is returned if it is abnormal, otherwise the second (given)
     * mode is returned.
     */
    def orElse(other: => Mode): Mode = this match {
      case Normal => other
      case _ => this
    }

  /**
   * This method is called with a test case name and it should return the behaviour
   * of that test case (use Mode.Normal for unremarkable tests).
   */
  def customiseTestsByName(name: String): Mode

  override def withFixture(test: NoArgTest) = {

    val mode = customiseTestsByName(test.name)

    def invokeTest() = super.withFixture(test)

    if (mode != Mode.Normal) {
      info(s"NOTE: Test case is customised with: \"$mode\"\n")
    }

    mode match {
      case Mode.Normal => invokeTest()
      case Mode.Retry(s) => withRetry { invokeTest() }
      case Mode.ExpectFailure(s) => {
        val res = invokeTest()
        res match {
          case _ => fail(s"Expected failure, but no exception/assertion was thrown")
          case Exceptional(_) | Failed(_) => Pending
          case Canceled(_) | Pending => res
        }
      }
      case Mode.Disabled(s) => cancel(s"Test has been explicitly disabled")
    }
  }
}
