import org.scalatest.Inside.inside
import org.scalatest.*
import org.scalatest.funsuite.*
import util.RunUtils

import java.io.{File, OutputStream, PrintStream, PrintWriter}
class MemoryRegionAnalysisMiscTest extends AnyFunSuite with OneInstancePerTest {

  //C:\workdir\bil-to-boogie-translator\examples
  private val examplesPath = System.getProperty("user.dir") + "/examples/";
  private val expectedPath = System.getProperty("user.dir") + "/src/test/scala/dotExpected/";
  private val tempPath = System.getProperty("user.dir") + "/src/test/scala/dump/";
  def runMain(name: String, dump: Boolean = false): Unit = {
    var expected = ""
    var actual = ""
    var output: Option[Map[analysis.CfgNode, ?]] = null
    RunUtils.generateVCsAdt(examplesPath + s"${name}/${name}.adt", examplesPath + s"${name}/${name}.relf", None, true)
    try {
      output = RunUtils.memoryRegionAnalysisResults
      val outFile = new File(tempPath + s"${name}")
      val pw = new PrintWriter(outFile, "UTF-8")
      output.get.foreach {
        case (k, v) =>
          pw.write(s"${k} -> ${v}")
          pw.write("\n")
      }
      pw.close()
      val actualFile = scala.io.Source.fromFile(tempPath + s"${name}")
      actual = actualFile.mkString
      val expectedFile = scala.io.Source.fromFile(expectedPath + s"${name}")
      expected = expectedFile.mkString
      expectedFile.close()
    } catch {
      case e: Exception =>
        if (dump) {
          val outFile = new File(expectedPath + s"${name}")
          val pw = new PrintWriter(outFile, "UTF-8")
          output.get.foreach {
            case (k, v) =>
              pw.write(s"${k} -> ${v}")
              pw.write("\n")
          }
          pw.close()
          assert(true)
          return
        }
        throw new Exception("TEST NOT SUPPORTED: Expected file not found " + expectedPath + s"${name}")
    }
    assert(actual.split('\n').sorted.mkString("Array(", ", ", ")") == expected.split('\n').sorted.mkString("Array(", ", ", ")"))
  }

//  def compareDots(name: String): Unit = {
//    val exampleFile = scala.io.Source.fromFile(examplesPath.replace("examples/", "test.dot"))
//    val expectedFile = scala.io.Source.fromFile(expectedPath + s"${name}.dot")
//    val output = exampleFile.mkString
//    print(output)
//    val expected = expectedFile.mkString
//    exampleFile.close()
//    expectedFile.close()
//
//    assert(output == expected)
//  }

  test("ifglobalTest") {
    runMain("ifglobal");
  }

  test("ifLocalTest") {
    runMain("iflocal");
  }

  test("secretWriteTest") {
    runMain("secret_write");
  }

  test("basicArraysReadTest") {
    runMain("basic_arrays_read");
  }

  test("basicArraysWriteTest") {
    runMain("basic_arrays_write");
  }

  test("basicFreeTest") {
    runMain("basicfree");
  }

  // used to generate the expected files (DO NOT RUN)
//  test("generate") {
//    runMain("ifglobal", true);
//  }
}
