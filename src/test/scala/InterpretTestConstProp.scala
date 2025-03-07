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

@test_util.tags.StandardSystemTest
class ConstPropInterpreterValidate extends AnyFunSuite with TestValueDomainWithInterpreter[FlatElement[BitVecLiteral]] {

  Logger.setLevel(LogLevel.ERROR)

  def valueInAbstractValue(absval: FlatElement[BitVecLiteral], concrete: Expr) = {
    absval match {
      case Top => TrueLiteral
      case Bottom => TrueLiteral /* deliberately don't check */
      case FlatEl(value) => BinaryExpr(BVEQ, value, concrete)
    }
  }

  def testInterpretConstProp(testName: String, examplePath: String) = {
    val loading = ILLoadingConfig(
      inputFile = examplePath + testName + ".adt",
      relfFile = examplePath + testName + ".relf",
      dumpIL = None
    )

    var ictx = IRLoading.load(loading)
    ictx = IRTransform.doCleanup(ictx)
    ir.transforms.clearParams(ictx.program)
    val analyses = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), ictx)

    val analysisres = analyses.intraProcConstProp.collect { case (block: Block, v) =>
      block -> v
    }.toMap

    val result = runTestInterpreter(ictx, analysisres)
    assertCorrectResult(result)

  }

  test("function1/clang") {
    val testName = "function1"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/clang/"
    testInterpretConstProp(testName, examplePath)
  }
  test("function1/gcc") {
    val testName = "function1"
    val examplePath = System.getProperty("user.dir") + s"/src/test/correct/$testName/gcc/"
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
