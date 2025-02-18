import ir.*
import ir.eval.*
import analysis.*
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
import ir.dsl

import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import ir.dsl.*
import util.RunUtils.loadAndTranslate

import scala.collection.mutable

class ConstPropInterpreterValidate extends AnyFunSuite {

  Logger.setLevel(LogLevel.ERROR)

  def testInterpretConstProp(testName: String, examplePath: String) = {
    val loading = ILLoadingConfig(
      inputFile = examplePath + testName + ".adt",
      relfFile = examplePath + testName + ".relf",
      dumpIL = None
    )

    var ictx = IRLoading.load(loading)
    ictx = IRTransform.doCleanup(ictx)
    val analysisres = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), ictx)

    val breaks: List[BreakPoint] = analysisres.intraProcConstProp.collect {
      // convert analysis result to a list of breakpoints, each which evaluates an expression describing
      // the invariant inferred by the analysis (the assignment of registers) at a corresponding program point

      case (command: Command, v) => {
        val expectedPredicates: List[(String, Expr)] = v.toList.map(r => {
          val (variable, value) = r
          val assertion = value match {
            case Top => TrueLiteral
            case Bottom => FalseLiteral /* unreachable */
            case FlatEl(value) => BinaryExpr(BVEQ, variable, value)
          }
          (variable.name, assertion)
        })
        BreakPoint(
          location = BreakPointLoc.CMD(command),
          BreakPointAction(saveState = false, evalExprs = expectedPredicates)
        )
      }
    }.toList

    assert(breaks.nonEmpty)

    // run the interpreter evaluating the analysis result at each command with a breakpoint
    val interpretResult = interpretWithBreakPoints(ictx, breaks.toList, NormalInterpreter, InterpreterState())
    val breakres: List[(BreakPoint, _, List[(String, Expr, Expr)])] = interpretResult._2
    assert(interpretResult._1.nextCmd == Stopped())
    assert(breakres.nonEmpty)

    // assert all the collected breakpoint watches have evaluated to true
    for (b <- breakres) {
      val (_, _, evaluatedexprs) = b
      evaluatedexprs.forall(c => {
        val (n, before, evaled) = c
        evaled == TrueLiteral
      })
    }
  }

  test("indirect_call_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testInterpretConstProp(testName, examplePath)
  }

  test("indirect_call_gcc_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    testInterpretConstProp(testName, examplePath)
  }

  test("indirect_call_clang_example") {
    val testName = "indirect_call"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    testInterpretConstProp(testName, examplePath)
  }

  test("jumptable2_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testInterpretConstProp(testName, examplePath)
  }

  test("jumptable2_gcc_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    testInterpretConstProp(testName, examplePath)
  }

  test("jumptable2_clang_example") {
    val testName = "jumptable2"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    testInterpretConstProp(testName, examplePath)
  }

  test("functionpointer_example") {
    val testName = "functionpointer"
    val examplePath = System.getProperty("user.dir") + s"/examples/$testName/"
    testInterpretConstProp(testName, examplePath)
  }

  test("functionpointer_gcc_example") {
    val testName = "functionpointer"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    testInterpretConstProp(testName, examplePath)
  }

  test("functionpointer_clang_example") {
    val testName = "functionpointer"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    testInterpretConstProp(testName, examplePath)
  }

  test("secret_write_clang") {
    val testName = "secret_write"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    testInterpretConstProp(testName, examplePath)
  }

  test("secret_write_gcc") {
    val testName = "secret_write"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
    testInterpretConstProp(testName, examplePath)
  }
}
