
import ir.*
import ir.eval._
import java.io.{BufferedWriter, File, FileWriter}
import ir.Endian.LittleEndian
import org.scalatest.*
import org.scalatest.funsuite.*
import specification.*
import util.{BASILConfig, IRLoading, ILLoadingConfig, IRContext, RunUtils, StaticAnalysis, StaticAnalysisConfig, StaticAnalysisContext, BASILResult, Logger, LogLevel, IRTransform}
import ir.eval.{interpretTrace, interpret, ExecEffect, Stopped}
import test_util.*


import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import ir.dsl.*
import util.RunUtils.loadAndTranslate

import scala.collection.mutable

class DifferentialAnalysis extends AnyFunSuite {

  Logger.setLevel(LogLevel.ERROR)

  def diffTest(initial: IRContext, transformed: IRContext) = {
    val (initialRes,traceInit) = interpretTrace(initial)
    val (result,traceRes) = interpretTrace(transformed)


    def filterEvents(trace: List[ExecEffect]) = {
      trace.collect {
        case e @ ExecEffect.Call(_, _, _) => e
        case e @ ExecEffect.StoreMem("mem", _) => e
        case e @ ExecEffect.LoadMem("mem", _) => e
      }
    }

    Logger.info(traceInit.t.map(_.toString.take(80)).mkString("\n"))
    assert(initialRes.nextCmd == Stopped())
    assert(result.nextCmd == Stopped())
    assert(Set.empty == initialRes.memoryState.getMem("mem").toSet.diff(result.memoryState.getMem("mem").toSet))
    assert(traceInit.t.nonEmpty)
    assert(traceRes.t.nonEmpty)
    assert(filterEvents(traceInit.t).mkString("\n") == filterEvents(traceRes.t).mkString("\n"))
  }

  def testProgram(testName: String, examplePath: String, suffix: String =".adt") = {

    val loading = ILLoadingConfig(inputFile = examplePath + testName + suffix,
      relfFile = examplePath + testName + ".relf",
      dumpIL = None,
    )

    var ictx = IRLoading.load(loading)
    ictx = IRTransform.doCleanup(ictx)

    var comparectx = IRLoading.load(loading)
    comparectx = IRTransform.doCleanup(ictx)
    val analysisres = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), comparectx)

    diffTest(ictx, comparectx)
  }

  test("indirect_call_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testProgram(testName, examplePath)
  }


  test("jumptable2_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testProgram(testName, examplePath)
  }


  test("jumptable_example") {
    val testName = "jumptable"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testProgram(testName, examplePath)
  }

  test("functionpointer_example") {
    val testName = "functionpointer"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testProgram(testName, examplePath)
  }



  test("function_got_example") {
    val testName = "function_got"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testProgram(testName, examplePath)
  }


  def runSystemTests(): Unit = {

    val path = System.getProperty("user.dir") + s"/src/test/correct/"
    val programs: Array[String] = getSubdirectories(path)

    // get all variations of each program
    for (p <- programs) {
      val programPath = path + "/" + p
      val variations = getSubdirectories(programPath)
      println(variations.mkString("\n"))
      variations.foreach(variation => {
        test("analysis_differential:" + p + "/" + variation + ":BAP") {
          testProgram(p, path + "/" + p + "/" + variation + "/", suffix=".adt")
        }
        test("analysis_differential:" +  p + "/" + variation + ":GTIRB") {
          testProgram(p, path + "/" + p + "/" + variation + "/", suffix=".gts")
        }
      }
      )
    }
  }


  runSystemTests()
}
