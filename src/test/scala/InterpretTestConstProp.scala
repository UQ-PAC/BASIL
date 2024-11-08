import ir.*
import ir.eval.*
import analysis.*
import java.io.{BufferedWriter, File, FileWriter}
import ir.Endian.LittleEndian
import org.scalatest.*
import org.scalatest.funsuite.*
import specification.*
import util.{BASILConfig, IRLoading, ILLoadingConfig, IRContext, RunUtils, StaticAnalysis, StaticAnalysisConfig, StaticAnalysisContext, BASILResult, Logger, LogLevel, IRTransform}
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

  def testInterpretConstProp(name: String, variation: String, path: String): Unit = {
    val variationPath = path + name + "/" + variation + "/" + name
    val loading = ILLoadingConfig(
      inputFile = variationPath + ".adt",
      relfFile = variationPath + ".relf",
      dumpIL = None,
    )

    var ictx = IRLoading.load(loading)
    ictx = IRTransform.doCleanup(ictx)
    val analysisres = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), ictx)

    val breaks: List[BreakPoint] = analysisres.constPropResult.collect {
      // convert analysis result to a list of breakpoints, each which evaluates an expression describing 
      // the invariant inferred by the analysis (the assignment of registers) at a corresponding program point

      case (command: Command, v) =>
        val expectedPredicates: List[(String, Expr)] = v.toList.map { r =>
          val (variable, value) = r
          val assertion = value match {
            case Top => TrueLiteral
            case Bottom => FalseLiteral /* unreachable */
            case FlatEl(value) => BinaryExpr(BVEQ, variable, value)
          }
          (variable.name, assertion)
        }
        BreakPoint(
          location = BreakPointLoc.CMD(command),
          BreakPointAction(saveState = false, evalExprs = expectedPredicates)
        )
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
      evaluatedexprs.forall { c =>
        val (n, before, evaled) = c
        evaled == TrueLiteral
      }
    }
  }

  test("indirect_call/gcc_pic:BAP") {
    testInterpretConstProp("indirect_call", "gcc_pic", "./src/test/indirect_calls/")
  }

  test("indirect_call/gcc:BAP") {
    testInterpretConstProp("indirect_call", "gcc", "./src/test/indirect_calls/")
  }

  test("indirect_call/clang:BAP") {
    testInterpretConstProp("indirect_call", "clang", "./src/test/indirect_calls/")
  }

  test("jumptable2/gcc_pic:BAP") {
    testInterpretConstProp("jumptable2", "gcc_pic", "./src/test/indirect_calls/")
  }

  test("jumptable2/gcc:BAP") {
    testInterpretConstProp("jumptable2", "gcc", "./src/test/indirect_calls/")
  }

  test("jumptable2/clang:BAP") {
    testInterpretConstProp("jumptable2", "clang", "./src/test/indirect_calls/")
  }

  test("functionpointer/gcc_pic:BAP") {
    testInterpretConstProp("functionpointer", "gcc_pic", "./src/test/indirect_calls/")
  }

  test("functionpointer/gcc:BAP") {
    testInterpretConstProp("functionpointer", "gcc", "./src/test/indirect_calls/")
  }

  test("functionpointer/clang:BAP") {
    testInterpretConstProp("functionpointer", "clang", "./src/test/indirect_calls/")
  }

  test("secret_write/clang:BAP") {
    testInterpretConstProp("secret_write", "clang", "./src/test/correct/")
  }

  test("secret_write/gcc:BAP") {
    testInterpretConstProp("secret_write", "gcc", "./src/test/correct/")
  }
}
