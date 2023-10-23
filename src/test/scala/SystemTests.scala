import org.scalatest.funsuite.AnyFunSuite
import util.Logger

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.sys.process.*

/** Add more tests by simply adding them to the programs directory. Refer to the existing tests for the expected
  * directory structure and file-name patterns.
  */
class SystemTests extends AnyFunSuite {
  val correctPath = "./src/test/correct"
  val correctPrograms: Array[String] = getSubdirectories(correctPath)
  val incorrectPath = "./src/test/incorrect"
  val incorrectPrograms: Array[String] = getSubdirectories(incorrectPath)

  // get all variations of each program
  for (p <- correctPrograms) {
    val path = correctPath + "/" + p
    val variations = getSubdirectories(path)
    variations.foreach(t =>
      test(p + "/" + t) {
        runTest(correctPath, p, t, true)
      }
    )
  }

  for (p <- incorrectPrograms) {
    val path = incorrectPath + "/" + p
    val variations = getSubdirectories(path)
    variations.foreach(t =>
      test(p + "/" + t) {
        runTest(incorrectPath, p, t, false)
      }
    )
  }

  def runTest(path: String, name: String, variation: String, shouldVerify: Boolean): Unit = {
    val directoryPath = path + "/" + name + "/"
    val variationPath = directoryPath + variation + "/" + name
    val specPath = directoryPath + name + ".spec"
    val outPath = variationPath + ".bpl"
    val ilPath = variationPath
    val ADTPath = variationPath + ".adt"
    val RELFPath = variationPath + ".relf"
    Logger.info(outPath)
    if (File(specPath).exists) {
      Main.main(Array("--adt", ADTPath, "--relf", RELFPath, "--spec", specPath, "--output", outPath, "--dump-il", ilPath))
    } else {
      Main.main(Array("--adt", ADTPath, "--relf", RELFPath, "--output", outPath, "--dump-il", ilPath))
    }
    Logger.info(outPath + " done")
    val boogieResult = Seq("boogie", "/timeLimit:10", "/printVerifiedProceduresCount:0", "/useArrayAxioms", outPath).!!
    val resultPath = variationPath + "_result.txt"
    log(boogieResult, resultPath)
    val verified = boogieResult.strip().equals("Boogie program verifier finished with 0 errors")
    val failureMsg =
      if shouldVerify then "Expected verification success, but got failure."
      else "Expected verification failure, but got success."

    val expectedOutPath = variationPath + ".expected"
    if (File(expectedOutPath).exists) {
      if (!compareFiles(expectedOutPath, outPath)) {
        info("Warning: Boogie file differs from expected")
      }
    } else {
      info("Note: this test has not previously succeeded")
    }
    if (verified != shouldVerify) fail(failureMsg)
  }

  def compareFiles(path1: String, path2: String): Boolean = {
    val source1 = Source.fromFile(path1)
    val source2 = Source.fromFile(path2)
    val lines1 = source1.getLines
    val lines2 = source2.getLines
    while (lines1.hasNext && lines2.hasNext) {
      val line1 = lines1.next()
      val line2 = lines2.next()
      if (line1 != line2) {
        source1.close
        source2.close
        return false
      }
    }
    if (lines1.hasNext || lines2.hasNext) {
      source1.close
      source2.close
      return false
    }

    source1.close
    source2.close
    true
  }

  /** @param directoryName
    *   of the parent directory
    * @return
    *   the names all subdirectories of the given parent directory
    */
  def getSubdirectories(directoryName: String): Array[String] = {
    File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }

  def log(text: String, path: String): Unit = {
    val writer = BufferedWriter(FileWriter(path, false))
    writer.write(text)
    writer.flush()
    writer.close()
  }
}
