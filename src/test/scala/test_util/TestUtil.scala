package test_util
import java.io.{BufferedWriter, File, FileWriter}

import ir.{Block, Procedure, Program}
import util.{BASILConfig, BASILResult, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

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
