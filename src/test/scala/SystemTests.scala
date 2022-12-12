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
  // directory containing all test programs
  val programsDir = "./src/test/programs"
  // all subdirectories of programsDir
  val programs = getTests(programsDir)
  // create a test for each test-program directory
  programs.foreach(t => test(t) {
    // expected pathnames given the standardised structure of test directories
    val stdPath = "%s/%s/%s".format(programsDir, t, t)
    val actualOutPath = stdPath + "_actual_out.bpl"
    val expectedOutPath = stdPath + "_expected_out.bpl"
    // run the tool and write the output to the standard location within the test directory
    main(stdPath + ".adt", stdPath + ".relf", stdPath + ".spec", actualOutPath)
    // check that the success/failure of the verification matches expectation
    // creates an xml file which may take a while to display
    // in future, we may want to automatically analyse this file
    val xmlArg = "/xml:" + stdPath + "_boogie_result.xml"
    // executes "boogie /printVerifiedProceduresCount:0 /xml:<xml path>, <actual out path>"
    val boogieResult = Seq("boogie", "/printVerifiedProceduresCount:0", xmlArg, actualOutPath).!!
    /* For now, we only test for verification success. In future, we want to test for verification
    failure as well. Since a program can be verified iff RG conditions can be found for it,
    verification failure can be tested by checking for their absence. Until RG-generation is
    integrated, this can be mocked with 'null' or 'default' RG conditions. Expected verification
    results may be expressed in the .spec file, or the test programs directory can be split into
    success and failure cases. */
    val verified = boogieResult.strip().equals("Boogie program verifier finished with 0 errors")
    assert(verified);
    // check that the boogie output matches expectation
    if (!Source.fromFile(actualOutPath).getLines.mkString.equals(
      Source.fromFile(expectedOutPath).getLines().mkString)) {
      info("Warning: Boogie file differs from expected")
    }
  })

  /**
    * @param directoryName of the parent directory
    * @return the names all subdirectories of the given parent directory
    */
  def getTests(directoryName: String): Array[String] = {
    (new File(directoryName))
      .listFiles
      .filter(_.isDirectory)
      .map(_.getName)
  }
}
