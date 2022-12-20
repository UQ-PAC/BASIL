import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.sys.process._

/**
  * Add more tests by simply adding them to the programs directory.
  * Refer to the existing tests for the expected directory structure and file-name patterns.
  */
class SystemTests extends AnyFunSuite {
  // get test-program directories
  val correctProgramsDir = "./src/test/correct_programs"
  val correctPrograms = getSubdirectories(correctProgramsDir)
  val incorrectProgramsDir = "./src/test/incorrect_programs"
  val incorrectPrograms = getSubdirectories(incorrectProgramsDir)
  // create a test for each test-program directory
  correctPrograms.foreach(t => test(t) {
    test(correctProgramsDir, t, true)
  })
  incorrectPrograms.foreach(t => test(t) {
    test(incorrectProgramsDir, t, false)
  })

  def test(programsDir: String, test: String, expectedCorrect: Boolean) = {
    // expected pathnames given the standardised structure of test directories
    val stdPath = "%s/%s/%s".format(programsDir, test, test)
    // the tool's output boogie file
    val actualOutPath = stdPath + "_actual_out.bpl"
    // the expected output boogie file
    val expectedOutPath = stdPath + "_expected_out.bpl"
    // run the tool and write the output boogie file to the test directory
    main(stdPath + ".adt", stdPath + ".relf", stdPath + ".spec", actualOutPath)
    // check that the success/failure of the verification matches expectation
    // creates an xml file which may take a while to display
    // in future, we may want to automatically analyse this file
    val xmlArg = "/xml:" + stdPath + "_boogie_result.xml"
    // verify the output boogie file
    // executes "boogie /printVerifiedProceduresCount:0 /xml:<xml path>, <actual out path>"
    val boogieResult = Seq("boogie", "/printVerifiedProceduresCount:0", xmlArg, actualOutPath).!!
    // for now, verification is checked by string-matching the success message
    val verified = boogieResult.strip().equals("Boogie program verifier finished with 0 errors")
    val failureMsg = if expectedCorrect then "Expected verification success, but got failure."
        else "Expected verification failure, but got success."
    if (verified != expectedCorrect) fail(failureMsg)
    // finally check that the actual output boogie file matches the expected output boogie file
    if (!Source.fromFile(actualOutPath).getLines.mkString.equals(
      Source.fromFile(expectedOutPath).getLines().mkString)) {
      info("Warning: Boogie file differs from expected")
    }
  }

  /**
    * @param directoryName of the parent directory
    * @return the names all subdirectories of the given parent directory
    */
  def getSubdirectories(directoryName: String): Array[String] = {
    (new File(directoryName))
      .listFiles
      .filter(_.isDirectory)
      .map(_.getName)
  }
}
