
import ir.*
import ir.eval._
import java.io.{BufferedWriter, File, FileWriter}
import ir.Endian.LittleEndian
import org.scalatest.*
import org.scalatest.funsuite.*
import specification.*
import util.{BASILConfig, IRLoading, ILLoadingConfig, IRContext, RunUtils, StaticAnalysis, StaticAnalysisConfig, StaticAnalysisContext, BASILResult, Logger, LogLevel}
import ir.eval.{interpretTrace, interpret, ExecEffect, Stopped}


import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import ir.dsl.*
import util.RunUtils.loadAndTranslate

import scala.collection.mutable

class DifferentialIndirectCall extends AnyFunSuite {

  Logger.setLevel(LogLevel.WARN)

  def diffTest(initial: IRContext, transformed: IRContext) = {
    val (initialRes,traceInit) = interpretTrace(initial)
    val (result,traceRes) = interpretTrace(transformed)


    def filterEvents(trace: List[ExecEffect]) = {
      trace.collect {
        case e @ ExecEffect.StoreMem("mem", _) => e
        case e @ ExecEffect.LoadMem("mem", _) => e
      }
    }
    println(traceInit.t.mkString("\n"))

    // println(traceInit.t.mkString("\n    "))
    assert(initialRes.nextCmd == Stopped())
    assert(result.nextCmd == Stopped())
    // assert(initialRes.memoryState.diff(result.memoryState) == Map.empty)
    assert(Set.empty == initialRes.memoryState.getMem("mem").toSet.diff(result.memoryState.getMem("mem").toSet))
    assert(filterEvents(traceInit.t) == filterEvents(traceRes.t))
  }

  def testProgram(testName: String, examplePath: String) = {
    val basilConfig = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = None,
      ),
      outputPrefix = "basil-test",
      staticAnalysis = Some(StaticAnalysisConfig(None, None, None)),
    )

    val basilConfigNoAnalysis = BASILConfig(
      loading = ILLoadingConfig(inputFile = examplePath + testName + ".adt",
        relfFile = examplePath + testName + ".relf",
        dumpIL = None,
      ),
      outputPrefix = "basil-test",
      staticAnalysis = None,
    )


    val program = loadAndTranslate(basilConfigNoAnalysis).ir
    val compare = loadAndTranslate(basilConfig).ir
    diffTest(program, compare)

  }

  test("indirect_call_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testProgram(testName, examplePath)
  }

  test("indirect_call_gcc_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    testProgram(testName, examplePath)
  }

  test("indirect_call_clang_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    testProgram(testName, examplePath)
  }

  test("jumptable2_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testProgram(testName, examplePath)
  }

  test("jumptable2_gcc_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    testProgram(testName, examplePath)
  }

  test("jumptable2_clang_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
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

  test("functionpointer_gcc_example") {
    val testName = "functionpointer"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    testProgram(testName, examplePath)
  }

  test("functionpointer_clang_example") {
    val testName = "functionpointer"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    testProgram(testName, examplePath)
  }


  test("function_got_example") {
    val testName = "function_got"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testProgram(testName, examplePath)
  }
}
