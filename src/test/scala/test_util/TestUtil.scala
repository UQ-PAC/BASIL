package test_util

import ir.{Block, Procedure, Program}
import java.io.File

object TestUtil {
  val testPath = "./src/test/analysis/livevars/"
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
}
