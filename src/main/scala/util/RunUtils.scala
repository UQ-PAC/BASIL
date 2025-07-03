package util

import analysis.data_structure_analysis.*
import analysis.{AnalysisManager, Interval as _, *}
import boogie.*
import ir.*
import ir.dsl.given
import ir.eval.*
import ir.transforms.*
import specification.*
import translating.*
import translating.PrettyPrinter.*
import util.DSAConfig.Prereq
import util.LogLevel.INFO
import util.{DebugDumpIRLogger, Logger}

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

import cilvisitor.*

/** This file contains the main program execution. See RunUtils.loadAndTranslate for the high-level process.
  */

/** Stores the results of the static analyses.
  */
/** Results of the main program execution.
  */
case class BASILResult(
  ir: IRContext,
  analysis: Option[StaticAnalysisContext],
  dsa: Option[DSAContext],
  boogie: ArrayBuffer[BProgram]
)

/** Tools for loading the IR program into an IRContext.
  */

object RunUtils {

  def run(q: BASILConfig): BASILResult = {
    val result = loadAndTranslate(q)
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

  def loadAndTranslate(conf: BASILConfig, postLoad: IRContext => Unit = s => ()): BASILResult = {
    Logger.info("[!] Loading Program")
    val q = conf
    var ctx = q.context.getOrElse(IRLoading.load(q.loading))
    postLoad(ctx) // allows extracting information from the original loaded program

    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))

    val analysisManager = AnalysisManager(ctx.program)
    // these transforms depend on basil config parameters and thus need to be constructed here
    val prepareForTranslation = getPrepareForTranslationTransform(q, Set("free"))
    val genProcSummaries = getGenerateProcedureSummariesTransform(q.loading.parameterForm || q.simplify)
    val genRgConditions = getGenerateRgConditionsTransform(ctx.program.procedures.toList.filter(_.returnBlock != None))
    val stripUnreachableFunctions = getStripUnreachableFunctionsTransform(q.loading.procedureTrimDepth)

    if conf.simplify then doCleanupWithSimplify(ctx, analysisManager)
    else doCleanupWithoutSimplify(ctx, analysisManager)

    assert(ir.invariant.programDiamondForm(ctx.program))

    transforms.inlinePLTLaunchpad(ctx, analysisManager)

    assert(ir.invariant.programDiamondForm(ctx.program))

    if q.loading.trimEarly then stripUnreachableFunctions(ctx, analysisManager)
    // todo: since refactoring, there is some extra code that is run here
    // see StripUnreachableFunctions.getStripUnreachableFunctionsTransform

    assert(ir.invariant.programDiamondForm(ctx.program))
    ctx.program.procedures.foreach(transforms.RemoveUnreachableBlocks.apply)
    Logger.info(s"[!] Removed unreachable blocks")

    if (q.loading.parameterForm && !q.simplify) {
      ir.transforms.clearParams(ctx.program)
      ctx = ir.transforms.liftProcedureCallAbstraction(ctx)
      if (conf.assertCalleeSaved) {
        transforms.CalleePreservedParam.transform(ctx.program)
      }
    } else {
      ir.transforms.clearParams(ctx.program)
      assert(invariant.correctCalls(ctx.program))
    }
    assert(invariant.correctCalls(ctx.program))
    ir.invariant.checkTypeCorrect(ctx.program)

    assert(ir.invariant.programDiamondForm(ctx.program))
    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    assert(invariant.correctCalls(ctx.program))

    q.loading.dumpIL.foreach(s => DebugDumpIRLogger.writeToFile(File(s"$s-before-analysis.il"), pp_prog(ctx.program)))
    val analysis = q.staticAnalysis.map { conf =>
      staticAnalysis(conf, ctx)
    }
    q.loading.dumpIL.foreach(s => DebugDumpIRLogger.writeToFile(File(s"$s-after-analysis.il"), pp_prog(ctx.program)))

    assert(ir.invariant.programDiamondForm(ctx.program))
    ir.eval.SimplifyValidation.validate = conf.validateSimp
    if (conf.simplify) {

      ir.transforms.clearParams(ctx.program)

      ir.transforms.liftIndirectCall(ctx.program)
      transforms.liftSVCompNonDetEarlyIR(ctx.program)

      DebugDumpIRLogger.writeToFile(File("il-after-indirectcalllift.il"), pp_prog(ctx.program))
      ctx = ir.transforms.liftProcedureCallAbstraction(ctx)
      DebugDumpIRLogger.writeToFile(File("il-after-proccalls.il"), pp_prog(ctx.program))

      if (conf.assertCalleeSaved) {
        transforms.CalleePreservedParam.transform(ctx.program)
      }

      assert(ir.invariant.programDiamondForm(ctx.program))
      doSimplify(ctx, conf.staticAnalysis)
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
      val dsaResults = IntervalDSA(ctx).dsa(conf.dsaConfig.get)
      dsaContext = Some(dsaResults)

      if q.memoryTransform && conf.dsaConfig.get != Prereq then // need more than prereq
        val memTransferTimer = PerformanceTimer("Mem Transfer Timer", INFO)
        visit_prog(MemoryTransform(dsaResults.topDown, dsaResults.globals), ctx.program)
        memTransferTimer.checkPoint("Performed Memory Transform")
    }

    if q.summariseProcedures then genProcSummaries(ctx, analysisManager)

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

    prepareForTranslation(ctx, analysisManager)

    if conf.generateRelyGuarantees then genRgConditions(ctx, analysisManager)

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
      ArrayBuffer(translating.BoogieTranslator.translateProg(ctx.program, q.outputPrefix))
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

  /** Use static analysis to resolve indirect calls and replace them in the IR until fixed point.
    */
  def staticAnalysis(config: StaticAnalysisConfig, ctx: IRContext): StaticAnalysisContext = {
    var iteration = 1
    var modified: Boolean = true
    val analysisResult = mutable.ArrayBuffer[StaticAnalysisContext]()
    while (modified) {
      Logger.debug("[!] Running Static Analysis")
      val result = analysis.AnalysisPipelineMRA.analyse(ctx, config, iteration, analysisResult.lastOption)
      val previousResult = analysisResult.lastOption
      analysisResult.append(result)
      StaticAnalysisLogger.info("[!] Replacing Indirect Calls")

      /*
      modified = transforms.SteensgaardIndirectCallResolution(
        ctx.program,
        result.steensgaardResults,
        result.reachingDefs
      ).resolveIndirectCalls()
       */

      if (
        config.memoryRegions == MemoryRegionsMode.MRA && (previousResult.isEmpty || result.vsaResult != previousResult.get.vsaResult)
      ) {
        modified = true
      } else {
        modified =
          transforms.VSAIndirectCallResolution(ctx.program, result.vsaResult, result.mmmResults).resolveIndirectCalls()
      }

      if (modified) {
        iteration += 1
        StaticAnalysisLogger.info(s"[!] Analysing again (iter $iteration)")
      }
    }

    // should later move this to be inside while (modified) loop and have splitting threads cause further iterations

    if (config.threadSplit) {
      transforms.splitThreads(ctx.program, analysisResult.last.steensgaardResults, analysisResult.last.ssaResults)
    }

    val reachingDefs = ReachingDefsAnalysis(ctx.program, analysisResult.last.writesToResult).analyze()
    config.analysisDotPath.foreach { s =>
      AnalysisResultDotLogger.writeToFile(File(s"${s}_ct.dot"), toDot(ctx.program))
    }

    StaticAnalysisLogger.info("[!] Running Symbolic Access Analysis")
    val symResults: Map[CFGPosition, Map[SymbolicAddress, TwoElement]] =
      SymbolicAddressAnalysis(ctx.program, analysisResult.last.interProcConstProp).analyze()
    config.analysisDotPath.foreach { s =>
      val labels = symResults.map { (k, v) => k -> v.toString }
      AnalysisResultDotLogger.writeToFile(File(s"${s}_saa.dot"), toDot(ctx.program, labels))
    }

    StaticAnalysisLogger.info("[!] Running DSA Analysis")

    writeToFile(pp_prog(ctx.program), "testo1.il")
    val symbolTableEntries: Set[SymbolTableEntry] = ctx.globals ++ ctx.funcEntries
    val dsa = DataStructureAnalysis(
      ctx.program,
      symResults,
      analysisResult.last.interProcConstProp,
      symbolTableEntries,
      ctx.globalOffsets,
      ctx.externalFunctions,
      reachingDefs,
      analysisResult.last.writesToResult,
      analysisResult.last.paramResults
    )
    dsa.analyze()

    config.analysisDotPath.foreach { s =>
      dsa.topDown(ctx.program.mainProcedure).toDot
      DebugDumpIRLogger.writeToFile(File(s"${s}_main_dsg.dot"), dsa.topDown(ctx.program.mainProcedure).toDot)
    }

    Logger.debug("[!] Injecting regions")
    val regionInjector = if (config.memoryRegions == MemoryRegionsMode.MRA) {
      val injector = RegionInjectorMRA(ctx.program, analysisResult.last.mmmResults)
      injector.injectRegions()
      Some(injector)
    } else if (config.memoryRegions == MemoryRegionsMode.DSA) {
      val injector = RegionInjectorDSA(ctx.program, dsa.topDown)
      injector.injectRegions()
      Some(injector)
    } else {
      None
    }

    assert(invariant.singleCallBlockEnd(ctx.program))
    StaticAnalysisLogger.info(s"[!] Finished indirect call resolution after $iteration iterations")
    analysisResult.last.copy(
      symbolicAddresses = symResults,
      localDSA = dsa.local.toMap,
      bottomUpDSA = dsa.bottomUp.toMap,
      topDownDSA = dsa.topDown.toMap,
      regionInjector = regionInjector
    )
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
