package util

import Parsers.*
import analysis.data_structure_analysis.*
import analysis.{Interval as _, *}
import bap.*
import boogie.*
import com.grammatech.gtirb.proto.IR.IR
import gtirb.{GTIRBReadELF, GTIRBResolver}
import ir.*
import ir.dsl.given
import ir.eval.*
import ir.transforms.MemoryTransform
import org.antlr.v4.runtime.{BailErrorStrategy, CharStreams, CommonTokenStream}
import specification.*
import translating.*
import translating.PrettyPrinter.*
import util.LogLevel.INFO
import util.{DebugDumpIRLogger, Logger}

import java.io.{BufferedWriter, File, FileInputStream, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import cilvisitor.*
import API.IREpoch
import ir.dsl.IRToDSL
import scala.collection.mutable.ArrayBuffer

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
  sva: Map[Procedure, SymValues[OSet]],
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

    val mode = q.frontendMode
    if (q.inputFile.endsWith(".gtirb") && !q.gtirbLiftOffline) {
      throw IllegalArgumentException(".gtirb input requires --lifter")
    }

    val (mainAddress, makeContext) = q.relfFile match {
      case Some(relf) => {

        // allow loading elf from inputFile if using GTIRB mode.
        val relfData = if (relf == q.inputFile && mode == FrontendMode.Gtirb) {
          Logger.info("[!] Using ELF data from GTIRB: " + q.inputFile)
          IRLoading.loadGTIRBReadELF(q)
        } else {
          Logger.info("[!] Using ELF data from relf: " + relf)
          IRLoading.loadReadELF(relf, q)
        }

        val ReadELFData(symbols, externalFunctions, globals, funcEntries, globalOffsets, mainAddress) = relfData

        def continuation(ctx: IRContext) =
          val specification = IRLoading.loadSpecification(q.specFile, ctx.program, globals)
          IRContext(symbols, externalFunctions, globals, funcEntries, globalOffsets, specification, ctx.program)

        (Some(mainAddress), continuation)
      }
      case None if mode == FrontendMode.Gtirb => {
        Logger.warn(
          "RELF input not provided, this is not recommended! To provide a RELF input, specify --relf or --gts-relf."
        )
        (None, (x: IRContext) => x)
      }
      case None => {
        (None, (x: IRContext) => x)
      }
    }

    val program: IRContext = (mode, mainAddress) match {
      case (FrontendMode.Gtirb, _) =>
        IRLoading.load(loadGTIRB(q.inputFile, mainAddress, q.gtirbLiftOffline, Some(q.mainProcedureName)))
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
        ir.transforms.clearParams(ctx.program)
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

  def skipGTIRBMagic(fileName: String): FileInputStream = {
    val fIn = FileInputStream(fileName)
    (0 to 7).map(_ => fIn.read()).toList match {
      case List('G', 'T', 'I', 'R', 'B', _, _, _) => fIn
      case _ => {
        fIn.close()
        FileInputStream(fileName)
      }
    }
  }

  def loadGTIRB(
    fileName: String,
    mainAddress: Option[BigInt],
    gtirbLiftOffline: Boolean,
    mainName: Option[String] = None
  ): Program = {
    val fIn = skipGTIRBMagic(fileName)
    val ir = IR.parseFrom(fIn)
    val mods = ir.modules
    val cfg = ir.cfg.get

    val lifter =
      if (gtirbLiftOffline) then OfflineLifterInsnLoader(mods)
      else {
        ParserMapInsnLoader(mods)
      }

    val GTIRBConverter = GTIRBToIR(mods, lifter, cfg, mainAddress, mainName)
    GTIRBConverter.createIR()
  }

  /** Loads ELF data from the GTIRB input file. */
  def loadGTIRBReadELF(config: ILLoadingConfig): ReadELFData = {
    val ir = IR.parseFrom(FileInputStream(config.inputFile))
    if (ir.modules.length != 1) {
      Logger.warn(s"GTIRB file ${config.inputFile} unexpectedly has ${ir.modules.length} modules")
    }

    val gtirb = GTIRBResolver(ir.modules.head)
    val gtirbRelfLoader = GTIRBReadELF(gtirb)
    gtirbRelfLoader.getReadELFData(config.mainProcedureName)
  }

  /**
   * Loads ELF data from *both* .relf and .gts (if using GTIRB input). If both
   * sources load successfully, compares them and warns on any differences.
   */
  def loadReadELFWithGTIRB(fileName: String, config: ILLoadingConfig): (ReadELFData, Option[ReadELFData]) = {
    val lexer = ReadELFLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(lexer)
    val parser = ReadELFParser(tokens)
    parser.setErrorHandler(BailErrorStrategy())
    parser.setBuildParseTree(true)

    val relf = ReadELFLoader.visitSyms(parser.syms(), config)

    val gtirbRelf = if (config.inputFile.endsWith(".gts") || config.inputFile.endsWith(".gtirb")) {
      val gtirbRelf = loadGTIRBReadELF(config)
      GTIRBReadELF.checkReadELFCompatibility(gtirbRelf, relf)
      Some(gtirbRelf)
    } else {
      None
    }

    (relf, gtirbRelf)
  }

  /**
   * Loads ELF data from .relf.
   */
  def loadReadELF(fileName: String, config: ILLoadingConfig) =
    loadReadELFWithGTIRB(fileName, config)._1

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

/** Methods related to transforming the IR `Program` in-place.
  *
  * These operate over the IRContext, and possibly use static analysis results.
  */
object IRTransform {
  val boogieReserved: Set[String] = Set("free")

  /** Initial cleanup before analysis.
    */
  def doCleanup(ctx: IRContext, doSimplify: Boolean = false): IRContext = {
    Logger.info("[!] Removing external function calls")
    // Remove external function references (e.g. @printf)
    val externalNames = ctx.externalFunctions.map(e => e.name)
    val externalNamesLibRemoved = mutable.Set[String]()
    externalNamesLibRemoved.addAll(externalNames)

    for (e <- externalNames) {
      if (e.contains('@')) {
        externalNamesLibRemoved.add(e.split('@')(0))
      }
    }

    ctx.program.procedures.foreach(ir.transforms.makeProcEntryNonLoop)

    // useful for ReplaceReturns
    // (pushes single block with `Unreachable` into its predecessor)
    while (transforms.coalesceBlocks(ctx.program)) {}

    transforms.applyRPO(ctx.program)
    val nonReturning = transforms.findDefinitelyExits(ctx.program)
    ctx.program.mainProcedure.foreach {
      case d: DirectCall if nonReturning.nonreturning.contains(d.target) => d.parent.replaceJump(Return())
      case _ =>
    }

    transforms.establishProcedureDiamondForm(ctx.program, doSimplify)

    ir.transforms.removeBodyOfExternal(externalNamesLibRemoved.toSet)(ctx.program)
    for (p <- ctx.program.procedures) {
      p.isExternal = Some(
        ctx.externalFunctions.exists(e => e.name == p.procName || p.address.contains(e.offset)) || p.isExternal
          .getOrElse(false)
      )
    }

    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    assert(invariant.procEntryNoIncoming(ctx.program))
    ctx
  }

  /** Cull unneccessary information that does not need to be included in the translation, and infer stack regions, and
    * add in modifies from the spec.
    */
  def prepareForTranslation(config: BASILConfig, ctx: IRContext): Unit = {
    if (config.staticAnalysis.isEmpty || (config.staticAnalysis.get.memoryRegions == MemoryRegionsMode.Disabled)) {
      ctx.program.determineRelevantMemory(ctx.globalOffsets)
    }

    Logger.info("[!] Stripping unreachable")
    val before = ctx.program.procedures.size
    transforms.stripUnreachableFunctions(ctx.program, config.loading.procedureTrimDepth)
    Logger.info(
      s"[!] Removed ${before - ctx.program.procedures.size} functions (${ctx.program.procedures.size} remaining)"
    )
    val dupProcNames = ctx.program.procedures.groupBy(_.name).filter((_, p) => p.size > 1).toList.flatMap(_(1))
    assert(dupProcNames.isEmpty)

    ctx.program.procedures.foreach(p =>
      p.blocks.foreach(b => {
        b.jump match {
          case GoTo(targs, _) if targs.isEmpty =>
            Logger.warn(s"block ${b.label} in subroutine ${p.name} has no outgoing edges")
          case _ => ()
        }
      })
    )

    if (
      !config.memoryTransform && (config.staticAnalysis.isEmpty || (config.staticAnalysis.get.memoryRegions == MemoryRegionsMode.Disabled))
    ) {
      visit_prog(ir.transforms.StackSubstituter(), ctx.program)
    }

    val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
    ctx.program.setModifies(specModifies)

    visit_prog(ir.transforms.BoogieReservedRenamer(boogieReserved), ctx.program)

    assert(invariant.singleCallBlockEnd(ctx.program))

    // check all blocks with an atomic section exist within the same procedure
    val visited = mutable.Set[Block]()
    for (p <- ctx.program.procedures) {
      for (b <- p.blocks) {
        if (!visited.contains(b)) {
          if (b.atomicSection.isDefined) {
            b.atomicSection.get.getBlocks.foreach { a => assert(a.parent == p) }
            visited.addAll(b.atomicSection.get.getBlocks)
          }
          visited.addOne(b)
        }
      }
    }
  }

  def generateProcedureSummaries(ctx: IRContext, IRProgram: Program, simplified: Boolean = false): Boolean = {
    var modified = false
    // Need to know modifies clauses to generate summaries, but this is probably out of place
    val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
    ctx.program.setModifies(specModifies)

    val summaryGenerator = SummaryGenerator(IRProgram, simplified)
    IRProgram.procedures
      .filter { p =>
        p != IRProgram.mainProcedure
      }
      .foreach { procedure =>
        {
          val req = summaryGenerator.generateRequires(procedure)
          modified = modified | procedure.requires != req
          procedure.requires = req

          val ens = summaryGenerator.generateEnsures(procedure)
          modified = modified | procedure.ensures != ens
          procedure.ensures = ens
        }
      }

    modified
  }

  def generateLoopInvariants(IRProgram: Program) = {
    FullLoopInvariantGenerator(IRProgram).addInvariants()
  }

  def generateRelyGuaranteeConditions(threads: List[Procedure]): Unit = {
    /* Todo: For the moment we are printing these to stdout, but in future we'd
    like to add them to the IR. */
    type StateLatticeElement = LatticeMap[Variable, analysis.Interval]
    type InterferenceLatticeElement = Map[Variable, StateLatticeElement]
    val stateLattice = IntervalLatticeExtension()
    val stateTransfer = SignedIntervalDomain().transfer
    val intDom = ConditionalWritesDomain[StateLatticeElement](stateLattice, stateTransfer)
    val relyGuarantees =
      RelyGuaranteeGenerator[InterferenceLatticeElement, StateLatticeElement](intDom).generate(threads)
    for ((p, (rely, guar)) <- relyGuarantees) {
      StaticAnalysisLogger.info("--- " + p.procName + " " + "-" * 50 + "\n")
      StaticAnalysisLogger.info("Rely:")
      StaticAnalysisLogger.info(intDom.toString(rely) + "\n")
      StaticAnalysisLogger.info("Guarantee:")
      StaticAnalysisLogger.info(intDom.toString(guar) + "\n")
    }
  }
}

/** Methods relating to program static analysis.
  */
object StaticAnalysis {

  def reducibleLoops(IRProgram: Program) = {
    StaticAnalysisLogger.debug("reducible loops")
    val foundLoops = LoopDetector.identify_loops(IRProgram)
    foundLoops.irreducibleLoops.foreach(l => StaticAnalysisLogger.debug(s"Irreducible loop found: ${l.name}"))

    val newLoops = foundLoops.reducibleTransformIR().identifiedLoops
    newLoops.foreach(l => StaticAnalysisLogger.debug(s"Loop found: ${l.name}"))

    foundLoops.updateIrWithLoops()
  }

  /** Run all static analysis passes on the provided IRProgram.
    */
  def analyse(
    ictx: IRContext,
    config: StaticAnalysisConfig,
    iteration: Int,
    previousResults: Option[StaticAnalysisContext] = None
  ): StaticAnalysisContext = {
    var ctx = ictx
    val IRProgram: Program = ctx.program
    val externalFunctions: Set[ExternalFunction] = ctx.externalFunctions
    val globals: Set[SpecGlobal] = ctx.globals
    val globalOffsets: Map[BigInt, BigInt] = ctx.globalOffsets

    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    assert(invariant.correctCalls(ctx.program))

    val subroutines = IRProgram.procedures
      .filter(p => p.address.isDefined)
      .map(p => p.address.get -> p.name)
      .toMap
    val globalAddresses = globals.map(s => s.address -> s.name).toMap
    val globalSizes = globals.map(s => s.name -> s.size).toMap
    val externalAddresses = externalFunctions.map(e => e.offset -> e.name).toMap
    StaticAnalysisLogger.debug("Globals:")
    StaticAnalysisLogger.debug(globalAddresses)
    StaticAnalysisLogger.debug("Global Offsets: ")
    StaticAnalysisLogger.debug(globalOffsets)
    StaticAnalysisLogger.debug("Global Sizes: ")
    StaticAnalysisLogger.debug(globalSizes)
    StaticAnalysisLogger.debug("External: ")
    StaticAnalysisLogger.debug(externalAddresses)
    StaticAnalysisLogger.debug("Subroutine Addresses:")
    StaticAnalysisLogger.debug(subroutines)

    // reducible loops
    if (config.irreducibleLoops) {
      reducibleLoops(IRProgram)

      config.analysisDotPath.foreach { s =>
        AnalysisResultDotLogger.writeToFile(
          File(s"${s}_graph-after-loop-reduce-$iteration.dot"),
          dotBlockGraph(IRProgram, IRProgram.map(b => b -> b.toString).toMap)
        )
        AnalysisResultDotLogger.writeToFile(
          File(s"${s}_blockgraph-after-loop-reduce-$iteration.dot"),
          dotBlockGraph(IRProgram, IRProgram.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap)
        )
      }
    }

    val mergedSubroutines = subroutines ++ externalAddresses

    val domain = computeDomain(IntraProcIRCursor, IRProgram.procedures)
    val interDomain = computeDomain(InterProcIRCursor, IRProgram.procedures)

    StaticAnalysisLogger.debug("[!] Running ANR")
    val ANRSolver = ANRAnalysisSolver(IRProgram)
    val ANRResult = ANRSolver.analyze()

    StaticAnalysisLogger.debug("[!] Running RNA")
    val RNASolver = RNAAnalysisSolver(IRProgram)
    val RNAResult = RNASolver.analyze()

    StaticAnalysisLogger.debug("[!] Running Inter-procedural Constant Propagation")
    val interProcConstProp = InterProcConstantPropagation(IRProgram)
    val interProcConstPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] =
      interProcConstProp.analyze()

    config.analysisResultsPath.foreach { s =>
      DebugDumpIRLogger.writeToFile(
        File(s"${s}OGconstprop$iteration.txt"),
        printAnalysisResults(IRProgram, interProcConstPropResult)
      )
    }

    val intraProcConstProp = IntraProcConstantPropagation(IRProgram)
    val intraProcConstPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] =
      intraProcConstProp.analyze()

    config.analysisResultsPath.foreach { s =>
      DebugDumpIRLogger.writeToFile(
        File(s"${s}_new_ir_constprop$iteration.txt"),
        printAnalysisResults(IRProgram, intraProcConstPropResult)
      )
    }

    config.analysisDotPath.foreach { f =>
      val dumpdomain = computeDomain[CFGPosition, CFGPosition](InterProcIRCursor, IRProgram.procedures)
      AnalysisResultDotLogger.writeToFile(
        File(s"${f}_new_ir_intercfg$iteration.dot"),
        toDot(dumpdomain.toSet, InterProcIRCursor, Map.empty, Set())
      )
    }

    val reachingDefinitionsAnalysisSolver = InterprocReachingDefinitionsAnalysisSolver(IRProgram)
    val reachingDefinitionsAnalysisResults = reachingDefinitionsAnalysisSolver.analyze()

    config.analysisDotPath.foreach { s =>
      AnalysisResultDotLogger.writeToFile(
        File(s"${s}_reachingDefinitions$iteration.dot"),
        toDot(
          IRProgram,
          IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> reachingDefinitionsAnalysisResults(b).toString).toMap,
          true
        )
      )
    }

    Logger.debug("[!] Running Writes To")
    val writesTo = WriteToAnalysis(ctx.program).analyze()

    Logger.debug("[!] Running commondef variable renaming (Intra SSA)")
    val SSAResults = getCommonDefinitionVariableRenaming(IRProgram, writesTo)

    config.analysisDotPath.foreach(s => {
      writeToFile(
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> SSAResults(b).toString).toMap, true),
        s"${s}_SSA$iteration.dot"
      )
    })

    val mmm = MemoryModelMap(globalOffsets, mergedSubroutines, globalAddresses, globalSizes)
    mmm.preLoadGlobals()

    val previousVSAResults = if (previousResults.isDefined) {
      previousResults.get.vsaResult
    } else {
      Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]()
    }

    StaticAnalysisLogger.debug("[!] Running GRA")
    val graSolver = GlobalRegionAnalysisSolver(
      IRProgram,
      domain.toSet,
      interProcConstPropResult,
      reachingDefinitionsAnalysisResults,
      mmm,
      previousVSAResults
    )
    val graResult = graSolver.analyze()

    StaticAnalysisLogger.debug("[!] Running MRA")
    val mraSolver = MemoryRegionAnalysisSolver(
      IRProgram,
      domain.toSet,
      interProcConstPropResult,
      reachingDefinitionsAnalysisResults,
      graResult,
      mmm,
      previousVSAResults
    )
    val mraResult = mraSolver.analyze()

    config.analysisDotPath.foreach { s =>
      AnalysisResultDotLogger.writeToFile(File(s"${s}_callgraph$iteration.dot"), dotCallGraph(IRProgram))
      AnalysisResultDotLogger.writeToFile(
        File(s"${s}_blockgraph$iteration.dot"),
        dotBlockGraph(IRProgram, IRProgram.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap)
      )

      AnalysisResultDotLogger.writeToFile(
        File(s"${s}_new_ir_constprop$iteration.dot"),
        toDot(
          IRProgram,
          IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> intraProcConstPropResult(b).toString).toMap
        )
      )

      writeToFile(
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> ANRResult(b).toString).toMap),
        s"${s}_ANR$iteration.dot"
      )

      writeToFile(
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> RNAResult(b).toString).toMap),
        s"${s}_RNA$iteration.dot"
      )

      writeToFile(
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> mraResult(b).toString).toMap),
        s"${s}_MRA$iteration.dot"
      )

      AnalysisResultDotLogger.writeToFile(
        File(s"${s}_GRA$iteration.dot"),
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> graResult(b).toString).toMap)
      )
    }

    StaticAnalysisLogger.debug("[!] Running MMM")
    mmm.convertMemoryRegions(
      mraSolver.procedureToStackRegions,
      mraSolver.procedureToHeapRegions,
      mraResult,
      mraSolver.procedureToSharedRegions,
      graSolver.getDataMap,
      graResult
    )
    mmm.logRegions()

    Logger.debug("[!] Running VSA")
    val vsaSolver = ValueSetAnalysisSolver(IRProgram, mmm)
    val vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]] = vsaSolver.analyze()

    mmm.postLoadVSARelations(vsaResult, ANRResult, RNAResult)

    config.analysisDotPath.foreach { s =>
      AnalysisResultDotLogger.writeToFile(
        File(s"${s}_VSA$iteration.dot"),
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> vsaResult(b).toString).toMap)
      )
    }

    Logger.debug("[!] Running Steensgaard")
    val steensgaardSolver = InterprocSteensgaardAnalysis(interDomain.toSet, mmm, SSAResults)
    steensgaardSolver.analyze()
    val steensgaardResults = steensgaardSolver.pointsTo()

    mmm.setCallSiteSummaries(steensgaardSolver.callSiteSummary)

    val paramResults: Map[Procedure, Set[Variable]] = ParamAnalysis(IRProgram).analyze()
    val interLiveVarsResults: Map[CFGPosition, Map[Variable, TwoElement]] = InterLiveVarsAnalysis(IRProgram).analyze()

    StaticAnalysisContext(
      intraProcConstProp = intraProcConstPropResult,
      interProcConstProp = interProcConstPropResult,
      memoryRegionResult = mraResult,
      vsaResult = vsaResult,
      interLiveVarsResults = interLiveVarsResults,
      paramResults = paramResults,
      steensgaardResults = steensgaardResults,
      mmmResults = mmm,
      symbolicAddresses = Map.empty,
      reachingDefs = reachingDefinitionsAnalysisResults,
      regionInjector = None,
      localDSA = Map.empty,
      bottomUpDSA = Map.empty,
      topDownDSA = Map.empty,
      writesToResult = writesTo,
      ssaResults = SSAResults
    )
  }

  def printAnalysisResults(prog: Program, result: Map[CFGPosition, _]): String = {
    val results = mutable.ArrayBuffer[String]()
    val toVisit = mutable.Stack[CFGPosition]()
    val visited = mutable.HashSet[CFGPosition]()
    toVisit.pushAll(prog.procedures)

    while (toVisit.nonEmpty) {
      val next = toVisit.pop()
      visited.add(next)
      toVisit.pushAll(
        IntraProcBlockIRCursor
          .succ(next)
          .diff(visited.collect[Block] { case b: Block =>
            b
          })
      )

      def contentStr(b: CFGPosition) = {
        if result.contains(b) then "\n        :: " + result(b)
        else ""
      }

      val t = next match
        case p: Procedure => s"\nProcedure ${p.name}"
        case b: Block =>
          Seq(
            s"  Block ${b.label}${contentStr(b)}",
            b.statements
              .map(s => {
                "    " + s.toString + contentStr(s)
              })
              .mkString("\n"),
            "    " + b.jump.toString + contentStr(b.jump)
          ).mkString("\n")
        case s: Statement => s"    Statement $s${contentStr(s)}"
        case s: Jump => s"  Jump $s${contentStr(s)}"
      results.addOne(t)
    }
    results.mkString(System.lineSeparator())
  }
}

object RunUtils {

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

  def doSimplify(ctx: IRContext, config: Option[StaticAnalysisConfig], collectedEpochs: Option[ArrayBuffer[IREpoch]] = None): Unit = {
    // writeToFile(dotBlockGraph(program, program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-before-simp.dot")
    Logger.info("[!] Running Simplify")
    val timer = PerformanceTimer("Simplify")
    val program = ctx.program

    val foundLoops = LoopDetector.identify_loops(program)
    val newLoops = foundLoops.reducibleTransformIR()
    newLoops.updateIrWithLoops()

    // --- START OF NEW EPOCH SECTION: BEFORE normalise block names OPTIMIZATIONS ---
    val beforeNormaliseBlockNamesProg = IRToDSL.convertProgram(program).resolve
    // --- END OF AFTER EPOCH SECTION ---

    for (p <- program.procedures) {
      p.normaliseBlockNames()
    }

    // --- START OF NEW EPOCH SECTION: AFTER normalise block names OPTIMIZATIONS ---
    collectedEpochs.foreach { buffer =>
      val afterNormaliseBlockNamesProg = IRToDSL.convertProgram(program).resolve
      buffer += IREpoch("normalise_block_names", beforeNormaliseBlockNamesProg, afterNormaliseBlockNamesProg)
    }
    // --- END OF AFTER EPOCH SECTION ---

    ctx.program.sortProceduresRPO()

    transforms.liftSVComp(ctx.program)

    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-simp.il"), pp_prog(program))
      }
    }

    transforms.applyRPO(program)

    // example of printing a simple analysis

    // --- START OF NEW EPOCH SECTION: BEFORE BLOCK CLEANING OPTIMIZATIONS ---
    val beforeBlockCleaningProgram = IRToDSL.convertProgram(program).resolve // Capture BEFORE state
    // --- END OF AFTER EPOCH SECTION ---

    transforms.removeEmptyBlocks(program)
    transforms.coalesceBlocks(program)
    transforms.removeEmptyBlocks(program)

    // --- START OF NEW EPOCH SECTION: AFTER BLOCK CLEANING OPTIMIZATIONS ---
    val afterBlockCleaningProgram = IRToDSL.convertProgram(program).resolve
    collectedEpochs.foreach { buffer =>
      buffer += IREpoch("block_cleaning", beforeBlockCleaningProgram, afterBlockCleaningProgram)
    }
    // --- END OF AFTER EPOCH SECTION ---

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

    transforms.inlinePLTLaunchpad(ctx.program)

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

    // --- START OF NEW EPOCH SECTION: BEFORE GUARD OPTIMIZATIONS ---
    val beforeGuardOptimizationsProgram = IRToDSL.convertProgram(program).resolve // Capture BEFORE state
    // --- END OF BEFORE EPOCH SECTION ---

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

    // --- START OF NEW EPOCH SECTION: AFTER GUARD OPTIMIZATIONS ---
    collectedEpochs.foreach { buffer =>
      val afterGuardOptimizationsProgram = IRToDSL.convertProgram(program).resolve // Capture AFTER state
      buffer += IREpoch("guard_optimisations", beforeGuardOptimizationsProgram, afterGuardOptimizationsProgram)
    }
    // --- END OF AFTER EPOCH SECTION ---

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

  def loadAndTranslate(conf: BASILConfig, postLoad: IRContext => Unit = s => (), collectedEpochs: Option[ArrayBuffer[IREpoch]] = None): BASILResult = {
    Logger.info("[!] Loading Program")
    val q = conf
    var ctx = q.context.getOrElse(IRLoading.load(q.loading))
    postLoad(ctx) // allows extracting information from the original loaded program

    assert(ir.invariant.checkTypeCorrect(ctx.program))
    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))

    ctx = IRTransform.doCleanup(ctx, conf.simplify)
    assert(ir.invariant.programDiamondForm(ctx.program))

    transforms.inlinePLTLaunchpad(ctx.program)

    assert(ir.invariant.programDiamondForm(ctx.program))
    if (q.loading.trimEarly) {
      val before = ctx.program.procedures.size
      transforms.stripUnreachableFunctions(ctx.program, q.loading.procedureTrimDepth)
      Logger.info(
        s"[!] Removed ${before - ctx.program.procedures.size} functions (${ctx.program.procedures.size} remaining)"
      )
    }

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
    assert(ir.invariant.checkTypeCorrect(ctx.program))

    assert(ir.invariant.programDiamondForm(ctx.program))
    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    assert(invariant.correctCalls(ctx.program))

    // --- START OF NEW EPOCH SECTION: AFTER GUARD OPTIMIZATIONS ---
    val beforeStaticAnalysisProg = IRToDSL.convertProgram(ctx.program).resolve
    // --- END OF AFTER EPOCH SECTION ---

    q.loading.dumpIL.foreach(s => DebugDumpIRLogger.writeToFile(File(s"$s-before-analysis.il"), pp_prog(ctx.program)))
    val analysis = q.staticAnalysis.map { conf =>
      staticAnalysis(conf, ctx)
    }
    q.loading.dumpIL.foreach(s => DebugDumpIRLogger.writeToFile(File(s"$s-after-analysis.il"), pp_prog(ctx.program)))

    // --- START OF NEW EPOCH SECTION: AFTER GUARD OPTIMIZATIONS ---
    val afterStaticAnalysisProg = IRToDSL.convertProgram(ctx.program).resolve
    collectedEpochs.foreach { buffer =>
      buffer += IREpoch("static_analysis", beforeStaticAnalysisProg, afterStaticAnalysisProg)
    }
    // --- END OF AFTER EPOCH SECTION ---

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

    if (conf.summariseProcedures) {
      StaticAnalysisLogger.info("[!] Generating Procedure Summaries")
      IRTransform.generateProcedureSummaries(ctx, ctx.program, q.loading.parameterForm || conf.simplify)
    }

    if (!conf.staticAnalysis.exists(!_.irreducibleLoops) && conf.generateLoopInvariants) {
      if (!conf.staticAnalysis.exists(_.irreducibleLoops)) {
        StaticAnalysis.reducibleLoops(ctx.program)
      }

      StaticAnalysisLogger.info("[!] Generating Loop Invariants")
      IRTransform.generateLoopInvariants(ctx.program)
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

    IRTransform.prepareForTranslation(q, ctx)

    if (conf.generateRelyGuarantees) {
      StaticAnalysisLogger.info("[!] Generating Rely-Guarantee Conditions")
      IRTransform.generateRelyGuaranteeConditions(ctx.program.procedures.toList.filter(p => p.returnBlock != None))
    }

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
      val result = StaticAnalysis.analyse(ctx, config, iteration, analysisResult.lastOption)
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
