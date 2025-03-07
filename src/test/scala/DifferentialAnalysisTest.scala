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

trait DifferentialTest extends TestCustomisation {
  this: AnyFunSuite =>

  override def customiseTestsByName(name: String) = name match {
    case "analysis_differential:floatingpoint/clang:GTIRB" | "analysis_differential:floatingpoint/gcc:GTIRB" =>
      Mode.ExpectFailure("needs FP_Mul")

    case "analysis_differential:function1/gcc_O2:BAP" | "analysis_differential:function1/gcc_O2:GTIRB" |
        "analysis_differential:malloc_with_local/gcc_O2:BAP" | "analysis_differential:malloc_with_local/gcc_O2:GTIRB" |
        "analysis_differential:malloc_with_local3/gcc_O2:BAP" |
        "analysis_differential:malloc_with_local3/gcc_O2:GTIRB" =>
      Mode.ExpectFailure("needs printf_chk")

    case "analysis_differential:syscall/clang:BAP" | "analysis_differential:syscall/clang:GTIRB" |
        "analysis_differential:syscall/clang_O2:GTIRB" | "analysis_differential:syscall/gcc:BAP" |
        "analysis_differential:syscall/gcc:GTIRB" =>
      Mode.ExpectFailure("needs fork")

    case "analysis_differential:syscall/gcc_O2:BAP" => Mode.ExpectFailure("traceInit empty")
    case "analysis_differential:syscall/gcc_O2:GTIRB" => Mode.ExpectFailure("needs fork")

    case _ => Mode.Normal
  }

  Logger.setLevel(LogLevel.WARN)

  def diffTest(initial: IRContext, transformed: IRContext) = {

    val instructionLimit = 1000000

    def interp(p: IRContext): (InterpreterState, Trace) = {
      val interpreter = LayerInterpreter(tracingInterpreter(NormalInterpreter), EffectsRLimit(instructionLimit))
      val initialState = InterpFuns.initProgState(NormalInterpreter)(p, InterpreterState())
      // Logger.setLevel(LogLevel.DEBUG)
      val r = BASILInterpreter(interpreter).run((initialState, Trace(List())), 0)._1
      // Logger.setLevel(LogLevel.WARN)
      r
    }

    val (initialRes, traceInit) = interp(initial)
    val (result, traceRes) = interp(transformed)

    def filterEvents(trace: List[ExecEffect]) = {
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

@test_util.tags.AnalysisSystemTest
class DifferentialAnalysisTest extends AnyFunSuite with DifferentialTest {

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

@test_util.tags.AnalysisSystemTest
class DifferentialAnalysisTestSimplification extends AnyFunSuite with DifferentialTest {

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
