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

class SimplifyCsmith extends AnyFunSuite {

  def diffTest(initial: IRContext, transformed: IRContext) = {

    val instructionLimit = 1000000

    def interp(p: IRContext, breakpoint: List[BreakPoint]): (
      (InterpreterState, Trace),
      List[(BreakPoint, Option[(InterpreterState, Trace)], List[(String, Expr, Expr)])]
    ) = {

      val tracing = LayerInterpreter(NormalInterpreter, tracingInterpreter(NormalInterpreter))
      val breaking = LayerInterpreter(tracing, RememberBreakpoints(tracing, breakpoint.toList))
      val rlimit = LayerInterpreter(breaking, EffectsRLimit(instructionLimit))
      val initialState = InterpFuns.initProgState(NormalInterpreter)(p, InterpreterState())

      val i = (((initialState, Trace(List())), List()), 0)

      val r = InterpFuns.interpretProg(rlimit)(p, i)
      r._1
    }

    def filterEvents(trace: List[ExecEffect]) = {
      trace.collect {
        // case e @ ExecEffect.Call(_, _, _)      => e
        case e @ ExecEffect.StoreMem("mem", _) => e
        case e @ ExecEffect.LoadMem("mem", _)  => e
      }
    }

    val commandInitial = initial.program
      .collect {
        case c: DirectCall if c.target.procName == "platform_main_end" => (c, Register("R0", 64))
      }
      .toList
      .headOption

    val commandSimplified = transformed.program
      .collect {
        case c: DirectCall if c.target.procName == "platform_main_end" =>
          (c, c.actualParams.find(p => p._1.varName == "R0_in").get._2)
      }
      .toList
      .headOption

    val breakInitial = commandInitial.map(o =>
      BreakPoint(
        location = BreakPointLoc.CMD(o._1),
        BreakPointAction(saveState = false, log = false, evalExprs = List(("crc", o._2)))
      )
    )
    val breakSimplified = commandSimplified.map(o =>
      BreakPoint(
        location = BreakPointLoc.CMD(o._1),
        BreakPointAction(saveState = false, log = false, evalExprs = List(("crc", o._2)))
      )
    )

    println("interpret original")
    val ((initialRes, traceInit), breakResultInitial) = interp(initial, breakInitial.toList)
    println("interpret simplified")
    val ((result, traceRes), breakResultSimplified) = interp(transformed, breakSimplified.toList)

    val initstdout =
      initialRes.memoryState.getMem("stdout").toList.sortBy(_._1.value).map(_._2.value.toChar).mkString("")
    val comparstdout = result.memoryState.getMem("stdout").toList.sortBy(_._1.value).map(_._2.value.toChar).mkString("")
    println("STDOUT: \"" + initstdout + "\"")

    assert(initstdout == comparstdout)
    assert(initialRes.nextCmd == Stopped()) // correct exit
    assert(result.nextCmd == Stopped()) // correct exit
    assert(Set.empty == initialRes.memoryState.getMem("mem").toSet.diff(result.memoryState.getMem("mem").toSet))
    assert(traceInit.t.nonEmpty)
    assert(traceRes.t.nonEmpty)
    assert(filterEvents(traceInit.t) == filterEvents(traceRes.t))

    val icrc = breakResultInitial.headOption
      .flatMap(_._3.headOption)
      .map(_._3 match {
        case b: BitVecLiteral => "checksum = %X".format(b.value)
        case _                => ???
      })
    val simpcrc = breakResultSimplified.headOption
      .flatMap(_._3.headOption)
      .map(_._3 match {
        case b: BitVecLiteral => "checksum = %X".format(b.value)
        case _                => ???
      })

    info("orig CRC: " + icrc.toString)
    info("simp CRC: " + simpcrc.toString)

    for {
      icrc <- icrc
      simpcrc <- simpcrc
    } yield (assert(icrc == simpcrc))

  }

  def testProgram(testName: String, examplePath: String, suffix: String = ".adt") = {
    println(testName)
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

    ir.transforms.liftIndirectCall(comparectx.program)
    comparectx = ir.transforms.liftProcedureCallAbstraction(comparectx)
    RunUtils.doSimplify(comparectx, None)

    diffTest(ictx, comparectx)
  }

  def doTest() = {
    for (name <- test_util.getSubdirectories("src/test/csmith")) {
      for (build <- test_util.getSubdirectories("src/test/csmith/" + name)) {
        test(s"$name:$build:BAP") {
          testProgram(name, s"src/test/csmith/$name/$build/", ".adt")
        }

        test(s"$name:$build:GTIRB") {
          testProgram(name, s"src/test/csmith/$name/$build/", ".gts")
        }
      }
    }
  }

  Logger.setLevel(LogLevel.ERROR)
  doTest()

}
