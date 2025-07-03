package util

import Parsers.*
import analysis.data_structure_analysis.*
import analysis.{AnalysisManager, Interval as _, *}
import bap.*
import boogie.*
import com.grammatech.gtirb.proto.IR.IR
import gtirb.*
import ir.*
import ir.dsl.given
import ir.eval.*
import ir.transforms.*
import org.antlr.v4.runtime.{BailErrorStrategy, CharStreams, CommonTokenStream}
import specification.*
import translating.*
import translating.PrettyPrinter.*
import util.DSAConfig.Prereq
import util.LogLevel.INFO
import util.{DebugDumpIRLogger, Logger}

import java.io.{BufferedWriter, File, FileInputStream, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

import cilvisitor.*

/** This file contains the main program execution. See RunUtils.loadAndTranslate for the high-level process.
  */

/** Stores the IR Program loaded from the binary and ELF tables, which is modified during analysis and program
  * transformation.
  */
case class IRContext(
  symbols: List[ELFSymbol],
  externalFunctions: Set[ExternalFunction],
  globals: Set[SpecGlobal],
  funcEntries: Set[FuncEntry],
  globalOffsets: Map[BigInt, BigInt],
  specification: Specification,
  program: Program // internally mutable
)

enum FrontendMode {
  case Bap
  case Gtirb
  case Basil
}

/** Stores the results of the static analyses.
  */
case class StaticAnalysisContext(
  intraProcConstProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  interProcConstProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  memoryRegionResult: Map[CFGPosition, ((Set[StackRegion], Set[Variable]), Set[HeapRegion])],
  vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]],
  interLiveVarsResults: Map[CFGPosition, Map[Variable, TwoElement]],
  paramResults: Map[Procedure, Set[Variable]],
  steensgaardResults: Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]],
  mmmResults: MemoryModelMap,
  reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
  regionInjector: Option[RegionInjector],
  symbolicAddresses: Map[CFGPosition, Map[SymbolicAddress, TwoElement]],
  localDSA: Map[Procedure, Graph],
  bottomUpDSA: Map[Procedure, Graph],
  topDownDSA: Map[Procedure, Graph],
  writesToResult: Map[Procedure, Set[GlobalVar]],
  ssaResults: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
)

case class DSAContext(
  sva: Map[Procedure, SymValues[DSInterval]],
  constraints: Map[Procedure, Set[Constraint]],
  local: Map[Procedure, IntervalGraph],
  bottomUp: Map[Procedure, IntervalGraph],
  topDown: Map[Procedure, IntervalGraph],
  globals: Map[IntervalNode, IntervalNode]
)

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
object IRLoading {

  /** Create a context from just an IR program.
    */
  def load(p: Program): IRContext = {
    IRContext(
      List.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      Map.empty,
      IRLoading.loadSpecification(None, p, Set.empty),
      p
    )
  }

  /** Load a program from files using the provided configuration.
    */
  def load(q: ILLoadingConfig): IRContext = {

    val mode = if q.inputFile.endsWith(".gts") then {
      FrontendMode.Gtirb
    } else if q.inputFile.endsWith(".adt") then {
      FrontendMode.Bap
    } else if (q.inputFile.endsWith(".il")) {
      FrontendMode.Basil
    } else {
      throw Exception(s"input file name ${q.inputFile} must be an .adt or .gts file")
    }

    val (mainAddress, makeContext) = q.relfFile match {
      case Some(relf) => {
        // TODO: this tuple is large, should be a case class
        val (symbols, externalFunctions, globals, funcEntries, globalOffsets, mainAddress) =
          IRLoading.loadReadELF(relf, q)

        def continuation(ctx: IRContext) =
          val specification = IRLoading.loadSpecification(q.specFile, ctx.program, globals)
          IRContext(symbols, externalFunctions, globals, funcEntries, globalOffsets, specification, ctx.program)

        (Some(mainAddress), continuation)
      }
      case None if mode == FrontendMode.Gtirb => {
        Logger.warn("RELF not provided, recommended for GTIRB input")
        (None, (x: IRContext) => x)
      }
      case None => {
        (None, (x: IRContext) => x)
      }
    }

    val program: IRContext = (mode, mainAddress) match {
      case (FrontendMode.Gtirb, _) => IRLoading.load(loadGTIRB(q.inputFile, mainAddress, Some(q.mainProcedureName)))
      case (FrontendMode.Basil, _) => ir.parsing.ParseBasilIL.loadILFile(q.inputFile)
      case (FrontendMode.Bap, None) => throw Exception("relf is required when using BAP input")
      case (FrontendMode.Bap, Some(mainAddress)) => {
        val bapProgram = loadBAP(q.inputFile)
        IRLoading.load(BAPToIR(bapProgram, mainAddress).translate)
      }
    }

    val ctx = makeContext(program)
    mode match {
      case FrontendMode.Basil => {
        ctx.program.procedures.foreach(_.updateBlockSuffix())
        Logger.info("[!] Disabling PC tracking transforms due to IL input")
      }
      case _ => {
        ir.transforms.PCTracking.applyPCTracking(q.pcTracking, ctx.program)
        ctx.program.procedures.foreach(_.normaliseBlockNames())
      }
    }
    ctx
  }

  def loadBAP(fileName: String): BAPProgram = {
    val ADTLexer = BAP_ADTLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(ADTLexer)
    val parser = BAP_ADTParser(tokens)

    parser.setBuildParseTree(true)

    BAPLoader().visitProject(parser.project())
  }

  def loadGTIRB(fileName: String, mainAddress: Option[BigInt], mainName: Option[String] = None): Program = {
    val fIn = FileInputStream(fileName)
    val ir = IR.parseFrom(fIn)
    val mods = ir.modules
    val cfg = ir.cfg.get

    val semanticsJson = mods.map(_.auxData("ast").data.toStringUtf8)

    val semantics = semanticsJson.map(upickle.default.read[Map[String, List[InsnSemantics]]](_))

    val parserMap: Map[String, List[InsnSemantics]] = semantics.flatten.toMap

    val GTIRBConverter = GTIRBToIR(mods, parserMap, cfg, mainAddress, mainName)
    GTIRBConverter.createIR()
  }

  def loadReadELF(
    fileName: String,
    config: ILLoadingConfig
  ): (List[ELFSymbol], Set[ExternalFunction], Set[SpecGlobal], Set[FuncEntry], Map[BigInt, BigInt], BigInt) = {
    val lexer = ReadELFLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(lexer)
    val parser = ReadELFParser(tokens)
    parser.setErrorHandler(BailErrorStrategy())
    parser.setBuildParseTree(true)
    ReadELFLoader.visitSyms(parser.syms(), config)
  }

  def emptySpecification(globals: Set[SpecGlobal]) =
    Specification(Set(), globals, Map(), List(), List(), List(), Set())

  def loadSpecification(filename: Option[String], program: Program, globals: Set[SpecGlobal]): Specification = {
    filename match {
      case Some(s) =>
        val specLexer = SpecificationsLexer(CharStreams.fromFileName(s))
        val specTokens = CommonTokenStream(specLexer)
        val specParser = SpecificationsParser(specTokens)
        specParser.setBuildParseTree(true)
        val specLoader = SpecificationLoader(globals, program)
        specLoader.visitSpecification(specParser.specification())
      case None => emptySpecification(globals)
    }
  }
}

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

  def doSimplify(ctx: IRContext, config: Option[StaticAnalysisConfig]): Unit = {
    // writeToFile(dotBlockGraph(program, program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-before-simp.dot")
    Logger.info("[!] Running Simplify")
    val timer = PerformanceTimer("Simplify")
    val program = ctx.program

    val foundLoops = LoopDetector.identify_loops(program)
    val newLoops = foundLoops.reducibleTransformIR()
    newLoops.updateIrWithLoops()

    for (p <- program.procedures) {
      p.normaliseBlockNames()
    }

    ctx.program.sortProceduresRPO()

    transforms.liftSVComp(ctx.program)

    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-simp.il"), pp_prog(program))
      }
    }

    transforms.applyRPO(program)

    // example of printing a simple analysis

    transforms.removeEmptyBlocks(program)
    transforms.coalesceBlocks(program)
    transforms.removeEmptyBlocks(program)

    // transforms.coalesceBlocksCrossBranchDependency(program)
    config.foreach {
      _.analysisDotPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_blockgraph-before-dsa.dot"), dotBlockGraph(program.mainProcedure))
      }
    }

    Logger.info("[!] Simplify :: DynamicSingleAssignment")
    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-dsa.il"), pp_prog(program))
      }
    }

    transforms.OnePassDSA().applyTransform(program)

    // fixme: this used to be a plain function but now we have to supply an analysis manager!
    transforms.inlinePLTLaunchpad(ctx, AnalysisManager(ctx.program))

    transforms.removeEmptyBlocks(program)

    config.foreach {
      _.analysisDotPath.foreach { s =>
        AnalysisResultDotLogger.writeToFile(
          File(s"${s}_blockgraph-after-dsa.dot"),
          dotBlockGraph(
            program,
            (program.collect { case b: Block =>
              b -> pp_block(b)
            }).toMap
          )
        )
      }
    }
    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-after-dsa.il"), pp_prog(program))
      }
    }

    if (ir.eval.SimplifyValidation.validate) {
      Logger.info("DSA no uninitialised")
      assert(invariant.allVariablesAssignedIndex(program))
      // Logger.info("Live vars difftest")
      // val tipLiveVars : Map[CFGPosition, Set[Variable]] = analysis.IntraLiveVarsAnalysis(program).analyze()
      // assert(program.procedures.forall(transforms.difftestLiveVars(_, tipLiveVars)))

      Logger.info("DSA Check")
      val x = program.procedures.forall(transforms.rdDSAProperty)
      assert(x)
      Logger.info("DSA Check passed")
      assert(invariant.singleCallBlockEnd(program))
      assert(invariant.cfgCorrect(program))
      assert(invariant.blocksUniqueToEachProcedure(program))
    }

    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-copyprop.il"), pp_prog(program))
      }
    }

    // brute force run the analysis twice because it cleans up more stuff
    // assert(program.procedures.forall(transforms.rdDSAProperty))
    config.foreach {
      _.analysisDotPath.foreach { s =>
        AnalysisResultDotLogger.writeToFile(
          File(s"${s}_blockgraph-before-copyprop.dot"),
          dotBlockGraph(program.mainProcedure)
        )
      }
    }
    Logger.info("Copyprop Start")
    transforms.copyPropParamFixedPoint(program, ctx.globalOffsets)

    transforms.fixupGuards(program)
    transforms.removeDuplicateGuard(program)
    config.foreach {
      _.analysisDotPath.foreach { s =>
        AnalysisResultDotLogger.writeToFile(
          File(s"${s}_blockgraph-after-simp.dot"),
          dotBlockGraph(program.mainProcedure)
        )
      }
    }

    transforms.liftLinuxAssertFail(ctx)

    // assert(program.procedures.forall(transforms.rdDSAProperty))

    assert(invariant.blockUniqueLabels(program))
    Logger.info(s"CopyProp ${timer.checkPoint("Simplify")} ms ")

    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-after-copyprop.il"), pp_prog(program))
      }
    }

    // val x = program.procedures.forall(transforms.rdDSAProperty)
    // assert(x)
    if (ir.eval.SimplifyValidation.validate) {
      Logger.info("DSA Check (after transform)")
      val x = program.procedures.forall(transforms.rdDSAProperty)
      assert(x)
      Logger.info("DSA Check succeeded")
    }
    // run this after cond recovery because sign bit calculations often need high bits
    // which go away in high level conss
    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-after-slices.il"), pp_prog(program))
      }
    }

    // re-apply dsa
    // transforms.OnePassDSA().applyTransform(program)

    if (ir.eval.SimplifyValidation.validate) {
      Logger.info("[!] Simplify :: Writing simplification validation")
      val w = BufferedWriter(FileWriter("rewrites.smt2"))
      ir.eval.SimplifyValidation.makeValidation(w)
      w.close()
    }

    Logger.info("[!] Simplify :: finished")
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
