package util

import analysis.data_structure_analysis.*
import analysis.{AnalysisManager, Interval as _, *}
import boogie.*
import ir.*
import ir.dsl.IRToDSL
import ir.dsl.given
import ir.eval.*
import ir.transforms.*
import server.IREpoch
import translating.*
import translating.PrettyPrinter.*
import util.LogLevel.INFO
import util.{DebugDumpIRLogger, Logger}

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

import cilvisitor.*

/** This file contains the main program execution. See RunUtils.loadAndTranslate for the high-level process.
  */

/** Results of the main program execution.
  */
case class BASILResult(
  ir: IRContext,
  analysis: Option[StaticAnalysisContext],
  dsa: Option[DSAContext],
  boogie: ArrayBuffer[BProgram]
)

object RunUtils {

  def addEpochSnapshot(
    name: String,
    beforeState: Program,
    afterState: Program,
    collectedEpochs: Option[ArrayBuffer[IREpoch]]
  ): Unit = {
    collectedEpochs.foreach { buffer =>
      buffer += IREpoch(name, beforeState, afterState)
    }
  }

  def run(q: BASILConfig, collectedEpochsOpt: Option[ArrayBuffer[IREpoch]] = None): BASILResult = {
    val result = loadAndTranslate(q, collectedEpochs = collectedEpochsOpt)
    Logger.info("Writing output")
    writeOutput(result)
    result
  }

  def writeOutput(result: BASILResult): Unit = {
    Logger.debug("[!] Writing file...")
    for (boogie <- result.boogie) {
      val wr = BufferedWriter(FileWriter(boogie.filename))
      boogie.writeToString(wr)
      wr.close()
    }
  }

  def loadAndTranslate(
    conf: BASILConfig,
    postLoad: IRContext => Unit = s => (),
    collectedEpochs: Option[ArrayBuffer[IREpoch]] = None
  ): BASILResult = {
    Logger.info("[!] Loading Program")
    val q = conf
    var ctx = q.context.getOrElse(IRLoading.load(q.loading))
    postLoad(ctx) // allows extracting information from the original loaded program

    assert(ir.invariant.checkTypeCorrect(ctx.program))
    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))

    val analysisManager = AnalysisManager(ctx.program)

    if conf.simplify then doCleanupWithSimplify(ctx, analysisManager)
    else doCleanupWithoutSimplify(ctx, analysisManager)

    assert(ir.invariant.programDiamondForm(ctx.program))

    transforms.inlinePLTLaunchpad(ctx, analysisManager)

    assert(ir.invariant.programDiamondForm(ctx.program))

    if q.loading.trimEarly then
      getStripUnreachableFunctionsTransform(q.loading.procedureTrimDepth)(ctx, analysisManager)
    // todo: since refactoring, there is some extra code that is run here
    // see StripUnreachableFunctions.getStripUnreachableFunctionsTransform

    assert(ir.invariant.programDiamondForm(ctx.program))
    ctx.program.procedures.foreach(transforms.RemoveUnreachableBlocks.apply)
    Logger.info(s"[!] Removed unreachable blocks")

    if (q.loading.parameterForm && !q.simplify) {

      val beforeClearParamsProg = IRToDSL.convertProgram(ctx.program).resolve
      ir.transforms.clearParams(ctx.program)
      val afterClearParamsProg = IRToDSL.convertProgram(ctx.program).resolve
      addEpochSnapshot("clear_params_before_lift", beforeClearParamsProg, afterClearParamsProg, collectedEpochs)

      val beforeLiftProcCallAbstrProg = IRToDSL.convertProgram(ctx.program).resolve
      ctx = ir.transforms.liftProcedureCallAbstraction(ctx)
      val afterLiftProcCallAbstrProg = IRToDSL.convertProgram(ctx.program).resolve
      addEpochSnapshot(
        "lift_procedure_call_abstraction",
        beforeLiftProcCallAbstrProg,
        afterLiftProcCallAbstrProg,
        collectedEpochs
      )

      if (conf.assertCalleeSaved) {
        transforms.CalleePreservedParam.transform(ctx.program)
      }
    } else {
      val beforeClearParamsProg = IRToDSL.convertProgram(ctx.program).resolve
      ir.transforms.clearParams(ctx.program)
      val afterClearParamsProg = IRToDSL.convertProgram(ctx.program).resolve
      addEpochSnapshot("clear_params_only", beforeClearParamsProg, afterClearParamsProg, collectedEpochs)

      assert(invariant.correctCalls(ctx.program))
    }
    assert(invariant.correctCalls(ctx.program))
    assert(ir.invariant.checkTypeCorrect(ctx.program))

    assert(ir.invariant.programDiamondForm(ctx.program))
    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    assert(invariant.correctCalls(ctx.program))

    val beforeStaticAnalysisProg = IRToDSL.convertProgram(ctx.program).resolve
    q.loading.dumpIL.foreach(s => DebugDumpIRLogger.writeToFile(File(s"$s-before-analysis.il"), pp_prog(ctx.program)))
    val analysis = q.staticAnalysis.map { conf =>
      AnalysisPipelineMRA.runToFixpoint(conf, ctx)
    }
    q.loading.dumpIL.foreach(s => DebugDumpIRLogger.writeToFile(File(s"$s-after-analysis.il"), pp_prog(ctx.program)))
    val afterStaticAnalysisProg = IRToDSL.convertProgram(ctx.program).resolve
    addEpochSnapshot("static_analysis", beforeStaticAnalysisProg, afterStaticAnalysisProg, collectedEpochs)

    assert(ir.invariant.programDiamondForm(ctx.program))
    ir.eval.SimplifyValidation.validate = conf.validateSimp
    if (conf.simplify) {

      val beforeClearParamsProg = IRToDSL.convertProgram(ctx.program).resolve
      ir.transforms.clearParams(ctx.program)
      val afterClearParamsProg = IRToDSL.convertProgram(ctx.program).resolve
      addEpochSnapshot("clear_params_and_simplify", beforeClearParamsProg, afterClearParamsProg, collectedEpochs)

      val beforeLiftIndirectCallProg = IRToDSL.convertProgram(ctx.program).resolve
      ir.transforms.liftIndirectCall(ctx.program)
      val afterLiftIndirectCallProg = IRToDSL.convertProgram(ctx.program).resolve
      addEpochSnapshot("lift_indirect_calls", beforeLiftIndirectCallProg, afterLiftIndirectCallProg, collectedEpochs)

      transforms.liftSVCompNonDetEarlyIR(ctx.program)

      DebugDumpIRLogger.writeToFile(File("il-after-indirectcalllift.il"), pp_prog(ctx.program))

      val beforeLiftProcCallAbstrProg = IRToDSL.convertProgram(ctx.program).resolve
      ctx = ir.transforms.liftProcedureCallAbstraction(ctx)
      val afterLiftProcCallAbstrProg = IRToDSL.convertProgram(ctx.program).resolve
      addEpochSnapshot(
        "lift_procedure_call_abstraction_simp",
        beforeLiftProcCallAbstrProg,
        afterLiftProcCallAbstrProg,
        collectedEpochs
      )

      DebugDumpIRLogger.writeToFile(File("il-after-proccalls.il"), pp_prog(ctx.program))

      if (conf.assertCalleeSaved) {
        transforms.CalleePreservedParam.transform(ctx.program)
      }

      assert(ir.invariant.programDiamondForm(ctx.program))
      doSimplify(ctx, conf.staticAnalysis, collectedEpochs)
    }

    assert(ir.invariant.programDiamondForm(ctx.program))
    if (DebugDumpIRLogger.getLevel().id < LogLevel.OFF.id) {
      val dir = File("./graphs/")
      if (!dir.exists()) then dir.mkdirs()
      for (p <- ctx.program.procedures) {
        DebugDumpIRLogger.writeToFile(File(s"graphs/blockgraph-${p.name}-dot-simp.dot"), dotBlockGraph(p))
      }
    }

    assert(ir.invariant.programDiamondForm(ctx.program))
    var dsaContext: Option[DSAContext] = None
    if (conf.dsaConfig.isDefined) {
      updateWithCallSCC(ctx.program)
      val dsaResults = IntervalDSA(ctx, conf.dsaConfig.get).dsa()
      dsaContext = Some(dsaResults)

      if q.memoryTransform && conf.dsaConfig.get.phase == DSAPhase.TD then // need more than prereq
        val memTransferTimer = PerformanceTimer("Mem Transfer Timer", INFO)
        visit_prog(MemoryTransform(dsaResults.topDown, dsaResults.globals), ctx.program)
        memTransferTimer.checkPoint("Performed Memory Transform")
    }

    if (conf.transformIrreducibleLoops) {
      StaticAnalysisLogger.info("[!] Transforming Irreducible Loops")
      IrreducibleLoops.transform_all_and_update(ctx.program)
    }

    if q.summariseProcedures then
      getGenerateProcedureSummariesTransform(q.loading.parameterForm || q.simplify)(ctx, analysisManager)

    if (conf.generateLoopInvariants) {
      StaticAnalysisLogger.info("[!] Generating Loop Invariants")
      FullLoopInvariantGenerator(ctx.program).addInvariants()
    }

    if (q.runInterpret) {
      Logger.info("Start interpret")

      val ((fs, trace), value) =
        InterpFuns.interpretEvalProg(tracingInterpreter(NormalInterpreter))(ctx, (InterpreterState(), Trace.empty))

      val stdout = fs.memoryState.getMem("stdout").toList.sortBy(_._1.value).map(_._2.value.toChar).mkString("")

      Logger.info(s"Interpreter stdout:\n${stdout}")
      value match {
        case Right(r) => Logger.info(s"Interpreter returned: ${r.map(v => v._1.name + " -> " + v._2).mkString(", ")}")
        case _ => ()
      }

      q.loading.dumpIL.foreach(f => {
        val tf = f"${f}-interpret-trace.txt"
        writeToFile(trace.t.mkString("\n"), tf)
        val sf = f"${f}-stdout.txt"
        writeToFile(stdout, sf)
        Logger.info(s"Finished interpret: trace written to $tf")
      })

      val stopState = fs.nextCmd
      if (!normalTermination(stopState)) {
        Logger.error(s"Interpreter exited with $stopState")
      } else {
        Logger.info("Interpreter stopped normally.")
      }
    }

    getPrepareForTranslationTransform(q, Set("free"))(ctx, analysisManager)

    if conf.generateRelyGuarantees then
      getGenerateRgConditionsTransform(ctx.program.procedures.toList.filter(_.returnBlock != None))(
        ctx,
        analysisManager
      )

    q.loading.dumpIL.foreach(s => {
      val timer = PerformanceTimer("Dump IL")
      writeToFile(pp_irctx(ctx), s"$s-output.il")
      timer.checkPoint(".il written")
      val a = ctx.program.toScalaLines
      timer.checkPoint("ToScalaLines done")
      writeToFile(a.mkString, s"$s-output.scala")
      timer.checkPoint("ToScalaLines written")
    })
    Logger.info("[!] Translating to Boogie")

    val regionInjector = analysis.flatMap(a => a.regionInjector)
    assert(ir.invariant.checkTypeCorrect(ctx.program))

    val boogiePrograms = if (q.boogieTranslation.directTranslation) {
      Logger.info("Disabling WPIF VCs")
      ArrayBuffer(translating.BoogieTranslator.translateProg(ctx.program, Some(ctx.specification), q.outputPrefix))
    } else if (q.boogieTranslation.threadSplit && ctx.program.threads.nonEmpty) {
      val outPrograms = ArrayBuffer[BProgram]()
      for (thread <- ctx.program.threads) {
        val fileName = q.outputPrefix.stripSuffix(".bpl") + "_" + thread.entry.name + ".bpl"
        val boogieTranslator =
          IRToBoogie(ctx.program, ctx.specification, Some(thread), fileName, regionInjector, q.boogieTranslation)
        outPrograms.addOne(boogieTranslator.translate)
      }
      outPrograms
    } else {
      val boogieTranslator =
        IRToBoogie(ctx.program, ctx.specification, None, q.outputPrefix, regionInjector, q.boogieTranslation)
      ArrayBuffer(boogieTranslator.translate)
    }
    assert(invariant.singleCallBlockEnd(ctx.program))

    BASILResult(ctx, analysis, dsaContext, boogiePrograms)
  }

}

def readFromFile(fileName: String): Iterable[String] = {
  Files.readAllLines(Paths.get(fileName)).asScala
}

def writeToFile(content: String, fileName: String): Unit = {
  Logger.debug(s"Writing $fileName (${content.size} bytes)")
  val outFile = File(fileName)
  val pw = PrintWriter(outFile, "UTF-8")
  pw.write(content)
  pw.close()
}
