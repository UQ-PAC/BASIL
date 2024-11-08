
import ir.*
import java.io.{BufferedWriter, File, FileWriter}
import ir.Endian.LittleEndian
import org.scalatest.*
import org.scalatest.funsuite.*
import specification.*
import util.{BASILConfig, IRLoading, ILLoadingConfig, IRContext, RunUtils, StaticAnalysis, StaticAnalysisConfig, StaticAnalysisContext, BASILResult, Logger, LogLevel, IRTransform}
import ir.eval.*
import test_util.BASILTest.getSubdirectories

import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import ir.dsl.*
import util.RunUtils.loadAndTranslate

import scala.collection.mutable

class DifferentialAnalysis extends AnyFunSuite {

  Logger.setLevel(LogLevel.WARN)

  def diffTest(initial: IRContext, transformed: IRContext): Unit = {

    val instructionLimit = 1000000 

    def interp(p: IRContext) : (InterpreterState, Trace) = {
      val interpreter = LayerInterpreter(tracingInterpreter(NormalInterpreter), EffectsRLimit(instructionLimit))
      val initialState = InterpFuns.initProgState(NormalInterpreter)(p, InterpreterState())
      //Logger.setLevel(LogLevel.DEBUG)
      val (r, _) = BASILInterpreter(interpreter).run((initialState, Trace(List())), 0)
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
    val initstdout = initialRes.memoryState.getMem("stdout")
    val comparstdout = result.memoryState.getMem("stdout")
    val text = initstdout.toList.sortBy(_._1.value).map(_._2.value.toChar).mkString("")
    info("STDOUT: \"" + text + "\"")
    assert(initstdout == comparstdout)
    assert(initialRes.nextCmd == Stopped())
    assert(result.nextCmd == Stopped())
    assert(traceInit.t.nonEmpty)
    assert(traceRes.t.nonEmpty)
    assert(filterEvents(traceInit.t).mkString("\n") == filterEvents(traceRes.t).mkString("\n"))
  }

  def testProgram(name: String, variation: String, path: String): Unit = {
    val variationPath = path + name + "/" + variation + "/" + name
    val loading = ILLoadingConfig(
      inputFile = variationPath + ".adt",
      relfFile = variationPath + ".relf",
      dumpIL = None,
    )

    var ictx = IRLoading.load(loading)
    ictx = IRTransform.doCleanup(ictx)

    var comparectx = IRLoading.load(loading)
    comparectx = IRTransform.doCleanup(ictx)
    val analysisres = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), comparectx)

    diffTest(ictx, comparectx)
  }

  test("indirect_calls/indirect_call/gcc_pic:BAP") {
    testProgram("indirect_call", "gcc_pic", "./src/test/indirect_calls/")
  }

  test("indirect_calls/jumptable2/gcc_pic:BAP") {
    testProgram("jumptable2", "gcc_pic", "./src/test/indirect_calls/")
  }

  test("indirect_calls/jumptable/gcc:BAP") {
    testProgram("jumptable", "gcc", "./src/test/indirect_calls/")
  }

  test("functionpointer/gcc_pic:BAP") {
    testProgram("functionpointer", "gcc_pic", "./src/test/indirect_calls/")
  }

  def runTests(): Unit = {
    val path = System.getProperty("user.dir") + s"/src/test/correct/"
    val programs = getSubdirectories(path)

    // get all variations of each program
    for (p <- programs) {
      val programPath = path + "/" + p
      val variations = getSubdirectories(programPath)
      variations.foreach { t =>
        val variationPath = programPath + "/" + t + "/" + p
        val inputPath = variationPath + ".adt"
        if (File(inputPath).exists) {
          test("correct" + "/" + p + "/" + t + ":BAP") {
            testProgram(p, t, path)
          }
        }
      }
    }
  }

  runTests()
}
