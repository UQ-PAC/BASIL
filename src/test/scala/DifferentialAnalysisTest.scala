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

abstract class DifferentialTest extends AnyFunSuite, TestCustomisation {

  override def customiseTestsByName(name: String) = name match {
    case "analysis_differential:floatingpoint/clang:GTIRB" | "analysis_differential:floatingpoint/gcc:GTIRB" =>
      Mode.NotImplemented("needs FP_Mul")

    case "analysis_differential:syscall/clang:BAP" | "analysis_differential:syscall/clang:GTIRB" |
        "analysis_differential:syscall/clang_O2:GTIRB" | "analysis_differential:syscall/gcc:BAP" |
        "analysis_differential:syscall/gcc:GTIRB" =>
      Mode.NotImplemented("needs fork")

    case "analysis_differential:syscall/gcc_O2:BAP" => Mode.TempFailure("traceInit empty")
    case "analysis_differential:syscall/gcc_O2:GTIRB" => Mode.NotImplemented("needs fork")

    case _ => Mode.Normal
  }

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
        // Ignore calls to allow inlining etc
        // case e @ ExecEffect.Call(tgt, _, _) => ExecEffect.Call(tgt, ErrorStop(Errored("placeholder")), ErrorStop(Errored("placeholder")))
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
    assert(normalTermination(initialRes.nextCmd), "Non-normal termination for point of truth: " + initialRes.nextCmd)
    assert(normalTermination(result.nextCmd), "Non-normal termination for transformed prog:" + initialRes.nextCmd)
    assert(Set.empty == initialRes.memoryState.getMem("mem").toSet.diff(result.memoryState.getMem("mem").toSet))
    assert(traceInit.t.nonEmpty)
    assert(traceRes.t.nonEmpty)
    val trace1 = filterEvents(traceInit.t).mkString("\n")
    val trace2 = filterEvents(traceRes.t).mkString("\n")
    assert(trace1 == trace2)
  }

  def testProgram(
    testName: String,
    examplePath: String,
    suffix: String = ".adt",
    staticAnalysisConfig: Option[StaticAnalysisConfig] = Some(StaticAnalysisConfig(None, None, None)),
    simplify: Boolean = false
  ) = {

    val loading = ILLoadingConfig(
      inputFile = examplePath + testName + suffix,
      relfFile = examplePath + testName + ".relf",
      dumpIL = None,
      trimEarly = true /* no instances of indirectcalls in these examples */
    )

    var ictx = IRLoading.load(loading)
    ictx = IRTransform.doCleanup(ictx)

    var comparectx = IRLoading.load(loading)
    comparectx = IRTransform.doCleanup(comparectx)

    ir.transforms.clearParams(ictx.program)

    ir.transforms.clearParams(comparectx.program)

    for (analysis <- staticAnalysisConfig) {
      RunUtils.staticAnalysis(analysis, comparectx)
    }

    if (simplify) {
      ictx = ir.transforms.liftProcedureCallAbstraction(ictx)
      comparectx = ir.transforms.liftProcedureCallAbstraction(comparectx)
      RunUtils.doSimplify(ictx, staticAnalysisConfig)
    }

    diffTest(ictx, comparectx)
  }
}


/**
 * Disable analysis differential test because it makes no 
 * IR transforms, these examples contain no indirect calls.
 */
@test_util.tags.DisabledTest
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

@test_util.tags.AnalysisSystemTest
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
            testProgram(
              p,
              path + "/" + p + "/" + variation + "/",
              suffix = ".adt",
              simplify = true,
              staticAnalysisConfig = None
            )
          }
        }
        if (File(gtirbPath).exists) {
          test("analysis_differential:" + p + "/" + variation + ":GTIRB") {
            testProgram(
              p,
              path + "/" + p + "/" + variation + "/",
              suffix = ".gts",
              simplify = true,
              staticAnalysisConfig = None
            )
          }
        }

      })
    }
  }
  runSystemTests()
}
