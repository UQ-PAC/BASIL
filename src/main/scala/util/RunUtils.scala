package util

import java.io.{BufferedWriter, File, FileInputStream, FileWriter, IOException, PrintWriter}
import java.nio.file.{Files, Path, Paths}
import com.grammatech.gtirb.proto.IR.IR
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Section.Section
import spray.json.*
import ir.eval
import gtirb.*
import translating.PrettyPrinter.*
import ir.dsl.*

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import analysis.solvers.*
import analysis.*
import bap.*
import ir.*
import boogie.*
import specification.*
import Parsers.*
import Parsers.ASLpParser.*
import analysis.data_structure_analysis.{DataStructureAnalysis, Graph, SymbolicAddress, SymbolicAddressAnalysis}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.BailErrorStrategy
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token}
import translating.*
import util.{Logger, DebugDumpIRLogger, SimplifyLogger}

import java.util.Base64
import spray.json.DefaultJsonProtocol.*
import util.intrusive_list.IntrusiveList
import cilvisitor.*

import scala.annotation.tailrec
import scala.collection.mutable

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
  varDepsSummaries: Map[Procedure, Map[Taintable, Set[Taintable]]],
  regionInjector: Option[RegionInjector],
  symbolicAddresses: Map[CFGPosition, Map[SymbolicAddress, TwoElement]],
  localDSA: Map[Procedure, Graph],
  bottomUpDSA: Map[Procedure, Graph],
  topDownDSA: Map[Procedure, Graph],
  writesToResult: Map[Procedure, Set[Register]],
  ssaResults: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
)

/** Results of the main program execution.
  */
case class BASILResult(ir: IRContext, analysis: Option[StaticAnalysisContext], boogie: ArrayBuffer[BProgram])


/** Tools for loading the IR program into an IRContext.
  */
object IRLoading {

  /** Create a context from just an IR program.
    */
  def load(p: Program): IRContext = {
    IRContext(List.empty, Set.empty, Set.empty, Set.empty, Map.empty, IRLoading.loadSpecification(None, p, Set.empty), p)
  }

  /** Load a program from files using the provided configuration.
    */
  def load(q: ILLoadingConfig): IRContext = {
    // TODO: this tuple is large, should be a case class
    val (symbols, externalFunctions, globals, funcEntries, globalOffsets, mainAddress) = IRLoading.loadReadELF(q.relfFile, q)

    val program: Program = if (q.inputFile.endsWith(".adt")) {
      val bapProgram = loadBAP(q.inputFile)
      val IRTranslator = BAPToIR(bapProgram, mainAddress)
      IRTranslator.translate
    } else if (q.inputFile.endsWith(".gts")) {
      loadGTIRB(q.inputFile, mainAddress)
    } else {
      throw Exception(s"input file name ${q.inputFile} must be an .adt or .gts file")
    }

    val specification = IRLoading.loadSpecification(q.specFile, program, globals)

    IRContext(symbols, externalFunctions, globals, funcEntries, globalOffsets, specification, program)
  }

  def loadBAP(fileName: String): BAPProgram = {
    val ADTLexer = BAP_ADTLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(ADTLexer)
    val parser = BAP_ADTParser(tokens)

    parser.setBuildParseTree(true)

    BAPLoader.visitProject(parser.project())
  }

  def loadGTIRB(fileName: String, mainAddress: BigInt): Program = {
    val fIn = FileInputStream(fileName)
    val ir = IR.parseFrom(fIn)
    val mods = ir.modules
    val cfg = ir.cfg.get

    val semantics = mods.map(_.auxData("ast").data.toStringUtf8.parseJson.convertTo[Map[String, Array[Array[String]]]])

    def parse_insn(line: String): StmtContext = {
      val lexer = ASLpLexer(CharStreams.fromString(line))
      val tokens = CommonTokenStream(lexer)
      val parser = ASLpParser(tokens)
      parser.setErrorHandler(BailErrorStrategy())
      parser.setBuildParseTree(true)

      try {
        parser.stmt()
      } catch {
        case e: org.antlr.v4.runtime.misc.ParseCancellationException =>
          val extra = e.getCause match {
            case mismatch: org.antlr.v4.runtime.InputMismatchException =>
              val token = mismatch.getOffendingToken
              s"""
                exn: $mismatch
                offending token: $token

              ${line.replace('\n', ' ')}
              ${" " * token.getStartIndex}^ here!
              """.stripIndent
            case _ => ""
          }
          Logger.error(s"""Semantics parse error:\n  line: $line\n$extra""")
          throw e
      }
    }

    val parserMap = semantics.map(_.map((k: String, v: Array[Array[String]]) => (k, v.map(_.map(parse_insn)))))

    val GTIRBConverter = GTIRBToIR(mods, parserMap.flatten.toMap, cfg, mainAddress)
    GTIRBConverter.createIR()
  }

  def loadReadELF(
      fileName: String,
      config: ILLoadingConfig
  ): (List[ELFSymbol], Set[ExternalFunction], Set[SpecGlobal],  Set[FuncEntry], Map[BigInt, BigInt], BigInt) = {
    val lexer = ReadELFLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(lexer)
    val parser = ReadELFParser(tokens)
    parser.setBuildParseTree(true)
    ReadELFLoader.visitSyms(parser.syms(), config)
  }

  def loadSpecification(filename: Option[String], program: Program, globals: Set[SpecGlobal]): Specification = {
    filename match {
      case Some(s) =>
        val specLexer = SpecificationsLexer(CharStreams.fromFileName(s))
        val specTokens = CommonTokenStream(specLexer)
        val specParser = SpecificationsParser(specTokens)
        specParser.setBuildParseTree(true)
        val specLoader = SpecificationLoader(globals, program)
        specLoader.visitSpecification(specParser.specification())
      case None => Specification(Set(), globals, Map(), List(), List(), List(), Set())
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
  def doCleanup(ctx: IRContext, doSimplify : Boolean = false): IRContext = {
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

    transforms.applyRPO(ctx.program)
    val nonReturning = transforms.findDefinitelyExits(ctx.program)
    ctx.program.mainProcedure.foreach(s => s match {
      case d : DirectCall if nonReturning.nonreturning.contains(d.target) => d.parent.replaceJump(Return())
      case _ => ()
    })

    // FIXME: Main will often maintain the stack by loading R30 from the caller's stack frame
    //        before returning, which makes the R30 assertin faile. Hence we currently skip this 
    //        assertion for main, instead we should precondition the stack layout before main
    //        but the interaction between spec and memory regions is nontrivial currently
    cilvisitor.visit_prog(transforms.ReplaceReturns(proc => doSimplify && ctx.program.mainProcedure != proc), ctx.program)

    transforms.addReturnBlocks(ctx.program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), ctx.program)

    val externalRemover = ExternalRemover(externalNamesLibRemoved.toSet)
    externalRemover.visitProgram(ctx.program)
    for (p <- ctx.program.procedures) {
      p.isExternal = Some(ctx.externalFunctions.find(e => e.name == p.procName || p.address.contains(e.offset)).isDefined)
    }

    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
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

    if (config.staticAnalysis.isEmpty || (config.staticAnalysis.get.memoryRegions == MemoryRegionsMode.Disabled)) {
      val stackIdentification = StackSubstituter()
      stackIdentification.visitProgram(ctx.program)
    }

    val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
    ctx.program.setModifies(specModifies)

    val renamer = Renamer(boogieReserved)
    renamer.visitProgram(ctx.program)

    assert(invariant.singleCallBlockEnd(ctx.program))
  }

  def generateProcedureSummaries(
    ctx: IRContext,
    IRProgram: Program,
    constPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
    varDepsSummaries: Map[Procedure, Map[Taintable, Set[Taintable]]],
  ): Boolean = {
    var modified = false
    // Need to know modifies clauses to generate summaries, but this is probably out of place
    val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
    ctx.program.setModifies(specModifies)

    val specGlobalAddresses = ctx.specification.globals.map(s => s.address -> s.name).toMap
    val summaryGenerator = SummaryGenerator(IRProgram, ctx.specification.globals, specGlobalAddresses, constPropResult, varDepsSummaries)
    IRProgram.procedures.filter {
      p => p != IRProgram.mainProcedure
    }.foreach {
      procedure => {
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

}

/** Methods relating to program static analysis.
  */
object StaticAnalysis {
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

    StaticAnalysisLogger.debug("reducible loops")
    // reducible loops
    if (config.irreducibleLoops) {
      val foundLoops = LoopDetector.identify_loops(IRProgram)
      foundLoops.irreducibleLoops.foreach(l => StaticAnalysisLogger.debug(s"Irreducible loop found: ${l.name}"))

      val newLoops = foundLoops.reducibleTransformIR().identifiedLoops
      newLoops.foreach(l => StaticAnalysisLogger.debug(s"Loop found: ${l.name}"))

      foundLoops.updateIrWithLoops()

      config.analysisDotPath.foreach { s =>
        AnalysisResultDotLogger.writeToFile(File(s"${s}_graph-after-loop-reduce-$iteration.dot"), dotBlockGraph(IRProgram, IRProgram.map(b => b -> b.toString).toMap))
        AnalysisResultDotLogger.writeToFile(File(s"${s}_blockgraph-after-loop-reduce-$iteration.dot"), dotBlockGraph(IRProgram, IRProgram.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap))
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
    val interProcConstPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] = interProcConstProp.analyze()

    config.analysisResultsPath.foreach { s =>
      DebugDumpIRLogger.writeToFile(File(s"${s}OGconstprop$iteration.txt"), printAnalysisResults(IRProgram, interProcConstPropResult))
    }

    StaticAnalysisLogger.debug("[!] Variable dependency summaries")
    val scc = stronglyConnectedComponents(CallGraph, List(IRProgram.mainProcedure))
    val specGlobalAddresses = ctx.specification.globals.map(s => s.address -> s.name).toMap
    val varDepsSummaries = VariableDependencyAnalysis(IRProgram, ctx.specification.globals, specGlobalAddresses, interProcConstPropResult, scc).analyze()

    val intraProcConstProp = IntraProcConstantPropagation(IRProgram)
    val intraProcConstPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] = intraProcConstProp.analyze()

    config.analysisResultsPath.foreach { s =>
      DebugDumpIRLogger.writeToFile(File(s"${s}_new_ir_constprop$iteration.txt"), printAnalysisResults(IRProgram, intraProcConstPropResult))
    }

    config.analysisDotPath.foreach { f =>
      val dumpdomain = computeDomain[CFGPosition, CFGPosition](InterProcIRCursor, IRProgram.procedures)
       AnalysisResultDotLogger.writeToFile(File(s"${f}_new_ir_intercfg$iteration.dot"), toDot(dumpdomain.toSet, InterProcIRCursor, Map.empty))
    }

    val reachingDefinitionsAnalysisSolver = InterprocReachingDefinitionsAnalysisSolver(IRProgram)
    val reachingDefinitionsAnalysisResults = reachingDefinitionsAnalysisSolver.analyze()

    config.analysisDotPath.foreach { s =>
      AnalysisResultDotLogger.writeToFile(
        File(s"${s}_reachingDefinitions$iteration.dot"),
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> reachingDefinitionsAnalysisResults(b).toString).toMap, true)
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
    val graSolver = GlobalRegionAnalysisSolver(IRProgram, domain.toSet, interProcConstPropResult, reachingDefinitionsAnalysisResults, mmm, previousVSAResults)
    val graResult = graSolver.analyze()

    StaticAnalysisLogger.debug("[!] Running MRA")
    val mraSolver = MemoryRegionAnalysisSolver(IRProgram, domain.toSet, interProcConstPropResult, reachingDefinitionsAnalysisResults, graResult, mmm, previousVSAResults)
    val mraResult = mraSolver.analyze()

    config.analysisDotPath.foreach { s =>
      AnalysisResultDotLogger.writeToFile(File(s"${s}_callgraph$iteration.dot"), dotCallGraph(IRProgram))
      AnalysisResultDotLogger.writeToFile(
        File(s"${s}_blockgraph$iteration.dot"),
        dotBlockGraph(IRProgram, IRProgram.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap)
      )

      AnalysisResultDotLogger.writeToFile(
        File(s"${s}_new_ir_constprop$iteration.dot"),
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> intraProcConstPropResult(b).toString).toMap)
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
    mmm.convertMemoryRegions(mraSolver.procedureToStackRegions, mraSolver.procedureToHeapRegions, mraResult, mraSolver.procedureToSharedRegions, graSolver.getDataMap, graResult)
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
      intraProcConstProp = interProcConstPropResult,
      interProcConstProp = intraProcConstPropResult,
      memoryRegionResult = mraResult,
      vsaResult = vsaResult,
      interLiveVarsResults = interLiveVarsResults,
      paramResults = paramResults,
      steensgaardResults = steensgaardResults,
      mmmResults = mmm,
      symbolicAddresses = Map.empty,
      reachingDefs = reachingDefinitionsAnalysisResults,
      varDepsSummaries = varDepsSummaries,
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
        case s: Jump      => s"  Jump $s${contentStr(s)}"
      results.addOne(t)
    }
    results.mkString(System.lineSeparator())
  }

}
object RunUtils {

  def run(q: BASILConfig): Unit = {
    val result = loadAndTranslate(q)
    Logger.info("Writing output")
    writeOutput(result)
  }

  def writeOutput(result: BASILResult): Unit = {
    Logger.debug("[!] Writing file...")
    for (boogie <- result.boogie) {
      val wr = BufferedWriter(FileWriter(boogie.filename))
      boogie.writeToString(wr)
      wr.close()
    }
  }

  def doSimplify(ctx: IRContext, config: Option[StaticAnalysisConfig]) : Unit = {
    // writeToFile(dotBlockGraph(program, program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-before-simp.dot")
    Logger.info("[!] Running Simplify")
    val timer = PerformanceTimer("Simplify")
    val program = ctx.program

    ctx.program.sortProceduresRPO()

    transforms.liftLinuxAssertFail(ctx)
    transforms.liftSVComp(ctx.program)

    DebugDumpIRLogger.writeToFile(File("il-before-simp.il"), pp_prog(program))
    transforms.applyRPO(program)

    // example of printing a simple analysis

    transforms.removeEmptyBlocks(program)
    transforms.coalesceBlocks(program)
    transforms.removeEmptyBlocks(program)

    DebugDumpIRLogger.writeToFile(File("blockgraph-before-dsa.dot"), dotBlockGraph(program.mainProcedure))
    
    Logger.info("[!] Simplify :: DynamicSingleAssignment")
    DebugDumpIRLogger.writeToFile(File("il-before-dsa.il"), pp_prog(program))

    // transforms.DynamicSingleAssignment.applyTransform(program, liveVars)
    transforms.OnePassDSA().applyTransform(program)
    Logger.info(s"DSA ${timer.checkPoint("DSA ")} ms ")

    transforms.removeEmptyBlocks(program)

    AnalysisResultDotLogger.writeToFile(File(s"blockgraph-after-dsa.dot"), 
      dotBlockGraph(program, (program.collect {
      case b : Block => b -> pp_block(b)
    }).toMap))
    DebugDumpIRLogger.writeToFile(File("il-after-dsa.il"), pp_prog(program))

    if (ir.eval.SimplifyValidation.validate) {
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

    DebugDumpIRLogger.writeToFile(File("il-before-copyprop.il"), pp_prog(program))

    // brute force run the analysis twice because it cleans up more stuff
    //assert(program.procedures.forall(transforms.rdDSAProperty))
    AnalysisResultDotLogger.writeToFile(File("blockgraph-before-copyprop.dot"), dotBlockGraph(program.mainProcedure))
    Logger.info("Copyprop Start")
    transforms.copyPropParamFixedPoint(program, ctx.globalOffsets)
    AnalysisResultDotLogger.writeToFile(File("blockgraph-after-simp.dot"), dotBlockGraph(program.mainProcedure))

    for (p <- ctx.program.procedures) {
      DebugDumpIRLogger.writeToFile(File(s"graphs/blockgraph-${p.name}-after-simp.dot"), dotBlockGraph(p))
    }
    transforms.liftLinuxAssertFail(ctx)

    // assert(program.procedures.forall(transforms.rdDSAProperty))

    assert(invariant.blockUniqueLabels(program))
    Logger.info(s"CopyProp ${timer.checkPoint("Simplify")} ms ")
    DebugDumpIRLogger.writeToFile(File("il-after-copyprop.il"), pp_prog(program))


    // val x = program.procedures.forall(transforms.rdDSAProperty)
    //assert(x)
    if (ir.eval.SimplifyValidation.validate) {
      Logger.info("DSA Check (after transform)")
      val x = program.procedures.forall(transforms.rdDSAProperty)
      assert(x)
      Logger.info("DSA Check succeeded")
    }

    // run this after cond recovery because sign bit calculations often need high bits
    // which go away in high level conss
    DebugDumpIRLogger.writeToFile(File("il-after-slices.il"), pp_prog(program))

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

  def loadAndTranslate(conf: BASILConfig): BASILResult = {
    Logger.info("[!] Loading Program")
    var q = conf

    var ctx = IRLoading.load(q.loading)

    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))

    ctx = IRTransform.doCleanup(ctx, conf.simplify)

    if (q.loading.trimEarly) {
      val before = ctx.program.procedures.size
      transforms.stripUnreachableFunctions(ctx.program, q.loading.procedureTrimDepth)
      Logger.info(
        s"[!] Removed ${before - ctx.program.procedures.size} functions (${ctx.program.procedures.size} remaining)"
      )
    }

    if (q.loading.parameterForm && !q.simplify) {
      ir.transforms.clearParams(ctx.program)
      ctx = ir.transforms.liftProcedureCallAbstraction(ctx)
    } else {
      ir.transforms.clearParams(ctx.program)
    }
    assert(invariant.correctCalls(ctx.program))


    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    assert(invariant.correctCalls(ctx.program))

    q.loading.dumpIL.foreach(s => DebugDumpIRLogger.writeToFile(File(s"$s-before-analysis.il"), pp_prog(ctx.program)))
    val analysis = q.staticAnalysis.map {
      conf => staticAnalysis(conf, ctx)
    }
    q.loading.dumpIL.foreach(s => DebugDumpIRLogger.writeToFile(File(s"$s-after-analysis.il"), pp_prog(ctx.program)))

    ir.eval.SimplifyValidation.validate = conf.validateSimp
    if (conf.simplify) {

      ir.transforms.clearParams(ctx.program)

      ir.transforms.liftIndirectCall(ctx.program)
      transforms.liftSVCompNonDetEarlyIR(ctx.program)

      DebugDumpIRLogger.writeToFile(File("il-after-indirectcalllift.il"), pp_prog(ctx.program))
      ctx = ir.transforms.liftProcedureCallAbstraction(ctx)
      DebugDumpIRLogger.writeToFile(File("il-after-proccalls.il"), pp_prog(ctx.program))

      doSimplify(ctx, conf.staticAnalysis)
    }

    if (q.runInterpret) {
      Logger.info("Start interpret")
      val fs = eval.interpretTrace(ctx)

      val stdout = fs._1.memoryState.getMem("stdout").toList.sortBy(_._1.value).map(_._2.value.toChar).mkString("")

      Logger.info(s"Interpreter stdout:\n${stdout}")

      q.loading.dumpIL.foreach(f => {
        val tf = f"${f}-interpret-trace.txt"
        writeToFile((fs._2.t.mkString("\n")), tf)
        Logger.info(s"Finished interpret: trace written to $tf")
      })

      val stopState = fs._1.nextCmd
      if (stopState != eval.Stopped()) {
        Logger.error(s"Interpreter exited with $stopState")
      } else {
        Logger.info("Interpreter stopped normally.")
      }
    }

    IRTransform.prepareForTranslation(q, ctx)

    q.loading.dumpIL.foreach(s => 
      writeToFile(pp_prog(ctx.program), s"$s-output.il")
    )
    Logger.info("[!] Translating to Boogie")

    val regionInjector = analysis.flatMap(a => a.regionInjector)

    val boogiePrograms = if (q.boogieTranslation.threadSplit && ctx.program.threads.nonEmpty) {
      val outPrograms = ArrayBuffer[BProgram]()
      for (thread <- ctx.program.threads) {
        val fileName = q.outputPrefix.stripSuffix(".bpl") + "_" + thread.entry.name + ".bpl"
        val boogieTranslator = IRToBoogie(ctx.program, ctx.specification, Some(thread), fileName, regionInjector, q.boogieTranslation)
        outPrograms.addOne(boogieTranslator.translate)
      }
      outPrograms
    } else {
      val boogieTranslator = IRToBoogie(ctx.program, ctx.specification, None, q.outputPrefix, regionInjector, q.boogieTranslation)
      ArrayBuffer(boogieTranslator.translate)
    }
    assert(invariant.singleCallBlockEnd(ctx.program))


    BASILResult(ctx, analysis, boogiePrograms)
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

      if (config.memoryRegions == MemoryRegionsMode.MRA && (previousResult.isEmpty || result.vsaResult != previousResult.get.vsaResult)) {
        modified = true
      } else {
        modified = transforms.VSAIndirectCallResolution(
          ctx.program,
          result.vsaResult,
          result.mmmResults
        ).resolveIndirectCalls()
      }

      StaticAnalysisLogger.info("[!] Generating Procedure Summaries")
      if (config.summariseProcedures) {
        IRTransform.generateProcedureSummaries(ctx, ctx.program, result.intraProcConstProp, result.varDepsSummaries)
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
    writeToFile(pp_prog(ctx.program), "testo1.il" )
    val symbolTableEntries: Set[SymbolTableEntry] = ctx.globals ++ ctx.funcEntries
    val dsa = DataStructureAnalysis(ctx.program, symResults, analysisResult.last.interProcConstProp, symbolTableEntries, ctx.globalOffsets, ctx.externalFunctions, reachingDefs, analysisResult.last.writesToResult, analysisResult.last.paramResults)
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


def readFormFile(fileName: String) : Iterable[String] = {
  Files.readAllLines(Paths.get(fileName)).asScala
}

def writeToFile(content: String, fileName: String): Unit = {
  Logger.debug(s"Writing $fileName (${content.size} bytes)")
  val outFile = File(fileName)
  val pw = PrintWriter(outFile, "UTF-8")
  pw.write(content)
  pw.close()
}
