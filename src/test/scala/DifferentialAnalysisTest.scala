import ir.*
import ir.eval._
import java.io.{BufferedWriter, File, FileWriter}
import ir.Endian.LittleEndian
import org.scalatest.*
import org.scalatest.funsuite.*
import specification.*
import util.{
  BASILConfig,
  IRLoading,
  ILLoadingConfig,
  IRContext,
  RunUtils,
  StaticAnalysis,
  StaticAnalysisConfig,
  StaticAnalysisContext,
  BASILResult,
  Logger,
  LogLevel,
  IRTransform
}
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

    def interp(p: IRContext): (InterpreterState, Trace) = {
      val interpreter = LayerInterpreter(tracingInterpreter(NormalInterpreter), EffectsRLimit(instructionLimit))
      val initialState = ((InterpFuns.initProgState(NormalInterpreter)(p, InterpreterState()), Trace.empty), 0)
      // Logger.setLevel(LogLevel.DEBUG)

      val main = p.program.mainProcedure
      val r = InterpFuns
        .callProcedure(interpreter)(main, InterpFuns.mainDefaultFunctionArguments(main))
        .f(initialState)
      r._1._1
    }

    val (initialRes, traceInit) = interp(initial)
    val (result, traceRes) = interp(transformed)

    def filterEvents(trace: Iterable[ExecEffect]) = {
      trace.collect {
        case e @ ExecEffect.Call(_, _, _) => e
        case e @ ExecEffect.StoreMem("mem", _) => e
        case e @ ExecEffect.LoadMem("mem", _) => e
      }
    }

    Logger.info(traceInit.t.map(_.toString.take(80)).mkString("\n"))
    val initstdout =
      initialRes.memoryState.getMem("stdout").toList.sortBy(_._1.value).map(_._2.value.toChar).mkString("")
    val comparstdout = result.memoryState.getMem("stdout").toList.sortBy(_._1.value).map(_._2.value.toChar).mkString("")
    if (initstdout.nonEmpty) {
      info("STDOUT: \"" + initstdout + "\"")
    }
    // Logger.info(initialRes.memoryState.getMem("stderr").toList.sortBy(_._1.value).map(_._2).mkString(""))
    assert(initstdout == comparstdout)
    assert(normalTermination(initialRes.nextCmd), initialRes.nextCmd)
    assert(normalTermination(result.nextCmd), initialRes.nextCmd)
    assert(Set.empty == initialRes.memoryState.getMem("mem").toSet.diff(result.memoryState.getMem("mem").toSet))
    assert(traceInit.t.nonEmpty)
    assert(traceRes.t.nonEmpty)
    assert(filterEvents(traceInit.t).mkString("\n") == filterEvents(traceRes.t).mkString("\n"))
  }

  def testProgram(
    testName: String,
    examplePath: String,
    suffix: String = ".adt",
    staticAnalysisConfig: StaticAnalysisConfig = StaticAnalysisConfig(None, None, None),
    simplify: Boolean = false
  ) = {

    val loading = ILLoadingConfig(
      inputFile = examplePath + testName + suffix,
      relfFile = examplePath + testName + ".relf",
      dumpIL = None
    )

    var ictx = IRLoading.load(loading)
    ictx = IRTransform.doCleanup(ictx)

    var comparectx = IRLoading.load(loading)
    comparectx = IRTransform.doCleanup(comparectx)

    ir.transforms.clearParams(ictx.program)

    ir.transforms.clearParams(comparectx.program)

    val analysisres = RunUtils.staticAnalysis(staticAnalysisConfig, comparectx)

    if (simplify) {
      ictx = ir.transforms.liftProcedureCallAbstraction(ictx)
      comparectx = ir.transforms.liftProcedureCallAbstraction(comparectx)
      RunUtils.doSimplify(ictx, Some(staticAnalysisConfig))
    }

    diffTest(ictx, comparectx)
  }
}

class DifferentialAnalysisTest extends DifferentialTest {

  def runSystemTests(): Unit = {

    val path = System.getProperty("user.dir") + s"/src/test/correct/"
    val programs: Array[String] = getSubdirectories(path)

    // get all variations of each program
    for (p <- programs) {
      val programPath = path + "/" + p
      val variations = getSubdirectories(programPath)
      variations.foreach(variation => {
        val bapPath = path + "/" + p + "/" + variation + "/" + p + ".adt"
        val gtirbPath = path + "/" + p + "/" + variation + "/" + p + ".gts"

        if (File(bapPath).exists) {
          test("analysis_differential:" + p + "/" + variation + ":BAP") {
            testProgram(p, path + "/" + p + "/" + variation + "/", suffix = ".adt")
          }
        }
        if (File(gtirbPath).exists) {
          test("analysis_differential:" + p + "/" + variation + ":GTIRB") {
            testProgram(p, path + "/" + p + "/" + variation + "/", suffix = ".gts")
          }
        }

      })
    }
  }

  runSystemTests()
}

class DifferentialAnalysisTestSimplification extends DifferentialTest {

  def runSystemTests(): Unit = {

    val path = System.getProperty("user.dir") + s"/src/test/correct/"
    val programs: Array[String] = getSubdirectories(path)

    // get all variations of each program
    for (p <- programs) {
      val programPath = path + "/" + p
      val variations = getSubdirectories(programPath)
      variations.foreach(variation => {

        val bapPath = path + "/" + p + "/" + variation + "/" + p + ".adt"
        val gtirbPath = path + "/" + p + "/" + variation + "/" + p + ".gts"
        if (File(bapPath).exists) {
          test("analysis_differential:" + p + "/" + variation + ":BAP") {
            testProgram(p, path + "/" + p + "/" + variation + "/", suffix = ".adt")
          }
        }
        if (File(gtirbPath).exists) {
          test("analysis_differential:" + p + "/" + variation + ":GTIRB") {
            testProgram(p, path + "/" + p + "/" + variation + "/", suffix = ".gts")
          }
        }

      })
    }
  }
  runSystemTests()
}
