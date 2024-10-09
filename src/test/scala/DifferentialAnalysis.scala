
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

class DifferentialTest extends AnyFunSuite {

  Logger.setLevel(LogLevel.WARN)

  def diffTest(initial: IRContext, transformed: IRContext) = {

    val instructionLimit = 1000000 

    def interp(p: IRContext) : (InterpreterState, Trace) = {
      val interpreter = LayerInterpreter(tracingInterpreter(NormalInterpreter), EffectsRLimit(instructionLimit))
      val initialState = InterpFuns.initProgState(NormalInterpreter)(p, InterpreterState())
      //Logger.setLevel(LogLevel.DEBUG)
      val r = BASILInterpreter(interpreter).run((initialState, Trace(List())), 0)._1
      //Logger.setLevel(LogLevel.WARN)
      r
    }

    val (initialRes,traceInit) = interp(initial)
    val (result,traceRes) = interp(transformed)

    def filterEvents(trace: List[ExecEffect]) = {
      trace.collect {
        case e @ ExecEffect.Call(_, _, _) => e
        case e @ ExecEffect.StoreMem("mem", _) => e
        case e @ ExecEffect.LoadMem("mem", _) => e
      }
    }

    Logger.info(traceInit.t.map(_.toString.take(80)).mkString("\n"))
    val initstdout = initialRes.memoryState.getMem("stdout").toList.sortBy(_._1.value).map(_._2.value.toChar).mkString("")
    val comparstdout  = result.memoryState.getMem("stdout").toList.sortBy(_._1.value).map(_._2.value.toChar).mkString("")
    info("STDOUT: \"" + initstdout + "\"")
    // Logger.info(initialRes.memoryState.getMem("stderr").toList.sortBy(_._1.value).map(_._2).mkString(""))
    assert(initstdout == comparstdout)
    assert(initialRes.nextCmd == Stopped())
    assert(result.nextCmd == Stopped())
    assert(Set.empty == initialRes.memoryState.getMem("mem").toSet.diff(result.memoryState.getMem("mem").toSet))
    assert(traceInit.t.nonEmpty)
    assert(traceRes.t.nonEmpty)
    assert(filterEvents(traceInit.t).mkString("\n") == filterEvents(traceRes.t).mkString("\n"))
  }

  def testProgram(testName: String, examplePath: String, suffix: String =".adt", staticAnalysisConfig : StaticAnalysisConfig = StaticAnalysisConfig(None, None, None), simplify: Boolean = true) = {

    val loading = ILLoadingConfig(inputFile = examplePath + testName + suffix,
      relfFile = examplePath + testName + ".relf",
      dumpIL = None,
    )

    var ictx = IRLoading.load(loading)
    ictx = IRTransform.doCleanup(ictx)

    var comparectx = IRLoading.load(loading)
    comparectx = IRTransform.doCleanup(ictx)
    val analysisres = RunUtils.staticAnalysis(staticAnalysisConfig, comparectx)

    if (simplify) {
      RunUtils.doSimplify(ictx, Some(staticAnalysisConfig))
    }


    diffTest(ictx, comparectx)
  }
}


class DifferentialTestAnalysis extends DifferentialTest {

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
      variations.foreach(variation => {
        test("analysis_differential:" + p + "/" + variation + ":BAP") {
          testProgram(p, path + "/" + p + "/" + variation + "/", suffix=".adt")
        }
        //test("analysis_differential:" +  p + "/" + variation + ":GTIRB") {
        //  testProgram(p, path + "/" + p + "/" + variation + "/", suffix=".gts")
        //}
      }
      )
    }
  }


  runSystemTests()
}

class DifferentialTestSimplification extends DifferentialTest {

  def runSystemTests(): Unit = {

    val path = System.getProperty("user.dir") + s"/src/test/correct/"
    val programs: Array[String] = getSubdirectories(path)

    // get all variations of each program
    for (p <- programs) {
      val programPath = path + "/" + p
      val variations = getSubdirectories(programPath)
      variations.foreach(variation => {
        test("analysis_differential:" + p + "/" + variation + ":BAP") {
          testProgram(p, path + "/" + p + "/" + variation + "/", suffix=".adt", simplify=true)
        }
        //test("analysis_differential:" +  p + "/" + variation + ":GTIRB") {
        //  testProgram(p, path + "/" + p + "/" + variation + "/", suffix=".gts")
        //}
      }
      )
    }
  }
  runSystemTests()
}


