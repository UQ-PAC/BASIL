import org.scalatest.funsuite.AnyFunSuite
import java.io.File
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

/**
  * Add more tests by simply adding them to the programs directory.
  * Refer to the existing tests for the expected directory structure and file-name patterns.
  */
class SystemTests extends AnyFunSuite {
  val programsDir = "./src/test/programs"
  val programs = getTests(programsDir)
  programs.foreach(t => test(t) {
    val stdPath = "%s/%s/%s".format(programsDir, t, t)
    val actualOutPath = stdPath + "_actual_out.bpl"
    val expectedOutPath = stdPath + "_expected_out.bpl"
    main(stdPath + ".adt", stdPath + ".relf", stdPath + ".spec", actualOutPath)
    // check success/failure of verification matches expectation
      // todo: run boogie?
    // check that boogie output matches expected
    if(!Source.fromFile(actualOutPath).getLines.mkString.equals(
      Source.fromFile(expectedOutPath).getLines().mkString)) {
      fail("boogie file differs from expected")
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
