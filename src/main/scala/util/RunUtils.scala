package util

import java.io.{File, PrintWriter, FileInputStream, BufferedWriter, FileWriter, IOException}
import com.grammatech.gtirb.proto.IR.IR
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Section.Section
import spray.json.*
import ir.eval
import gtirb.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import java.io.{File, PrintWriter}
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters.*
import analysis.solvers.*
import analysis.*
import bap.*
import ir.*
import boogie.*
import specification.*
import Parsers.*
import Parsers.SemanticsParser.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.BailErrorStrategy
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token}
import translating.*
import util.Logger
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
    globalOffsets: Map[BigInt, BigInt],
    specification: Specification,
    program: Program // internally mutable
)

/** Stores the results of the static analyses.
  */
case class StaticAnalysisContext(
    constPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
    IRconstPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
    memoryRegionResult: Map[CFGPosition, LiftedElement[Set[MemoryRegion]]],
    vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]],
    interLiveVarsResults: Map[CFGPosition, Map[Variable, TwoElement]],
    paramResults: Map[Procedure, Set[Variable]],
    steensgaardResults: Map[RegisterVariableWrapper, Set[RegisterVariableWrapper | MemoryRegion]],
    mmmResults: MemoryModelMap,
    memoryRegionContents: Map[MemoryRegion, Set[BitVecLiteral | MemoryRegion]],
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
    varDepsSummaries: Map[Procedure, Map[Taintable, Set[Taintable]]],
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
    IRContext(List.empty, Set.empty, Set.empty, Map.empty, IRLoading.loadSpecification(None, p, Set.empty), p)
  }

  /** Load a program from files using the provided configuration.
    */
  def load(q: ILLoadingConfig): IRContext = {
    // TODO: this tuple is large, should be a case class
    val (symbols, externalFunctions, globals, globalOffsets, mainAddress) = IRLoading.loadReadELF(q.relfFile, q)

    val program: Program = if (q.inputFile.endsWith(".adt")) {
      val bapProgram = loadBAP(q.inputFile)
      val IRTranslator = BAPToIR(bapProgram, mainAddress)
      IRTranslator.translate
    } else if (q.inputFile.endsWith(".gts")) {
      loadGTIRB(q.inputFile, mainAddress)
    } else {
      throw Exception(s"input file name ${q.inputFile} must be an .adt or .gst file")
    }

    val specification = IRLoading.loadSpecification(q.specFile, program, globals)

    IRContext(symbols, externalFunctions, globals, globalOffsets, specification, program)
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
      val semanticsLexer = SemanticsLexer(CharStreams.fromString(line))
      val tokens = CommonTokenStream(semanticsLexer)
      val parser = SemanticsParser(tokens)
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
  ): (List[ELFSymbol], Set[ExternalFunction], Set[SpecGlobal], Map[BigInt, BigInt], BigInt) = {
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
      case None => Specification(globals, Map(), List(), List(), List(), Set())
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
  def doCleanup(ctx: IRContext): IRContext = {
    Logger.debug("[!] Removing external function calls")
    // Remove external function references (e.g. @printf)
    val externalNames = ctx.externalFunctions.map(e => e.name)
    val externalNamesLibRemoved = mutable.Set[String]()
    externalNamesLibRemoved.addAll(externalNames)

    for (e <- externalNames) {
      if (e.contains('@')) {
        externalNamesLibRemoved.add(e.split('@')(0))
      }
    }

    cilvisitor.visit_prog(transforms.ReplaceReturns(), ctx.program)
    transforms.addReturnBlocks(ctx.program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), ctx.program)

    val externalRemover = ExternalRemover(externalNamesLibRemoved.toSet)
    val renamer = Renamer(boogieReserved)
    externalRemover.visitProgram(ctx.program)
    renamer.visitProgram(ctx.program)

    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    ctx
  }

  /** Cull unneccessary information that does not need to be included in the translation, and infer stack regions, and
    * add in modifies from the spec.
    */
  def prepareForTranslation(config: ILLoadingConfig, ctx: IRContext): Unit = {
    ctx.program.determineRelevantMemory(ctx.globalOffsets)
    transforms.applyRPO(ctx.program)

    Logger.info("[!] Stripping unreachable")
    val before = ctx.program.procedures.size
    transforms.stripUnreachableFunctions(ctx.program, config.procedureTrimDepth)
    Logger.info(
      s"[!] Removed ${before - ctx.program.procedures.size} functions (${ctx.program.procedures.size} remaining)"
    )
    val dupProcNames = (ctx.program.procedures.groupBy(_.name).filter((n,p) => p.size > 1)).toList.flatMap(_._2)

    var dupCounter = 0 
    for (p <- dupProcNames) {
      dupCounter += 1
      p.name = p.name + "$" + p.address.map(_.toString).getOrElse(dupCounter.toString)
    }

    val stackIdentification = StackSubstituter()
    stackIdentification.visitProgram(ctx.program)

    val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
    ctx.program.setModifies(specModifies)
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
      iteration: Int
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
    val externalAddresses = externalFunctions.map(e => e.offset -> e.name).toMap
    Logger.debug("Globals:")
    Logger.debug(globalAddresses)
    Logger.debug("Global Offsets: ")
    Logger.debug(globalOffsets)
    Logger.debug("External: ")
    Logger.debug(externalAddresses)
    Logger.debug("Subroutine Addresses:")
    Logger.debug(subroutines)

    Logger.info("reducible loops")
    // reducible loops
    val detector = LoopDetector(IRProgram)
    val foundLoops = detector.identify_loops()
    foundLoops.foreach(l => Logger.debug(s"Loop found: ${l.name}"))

    val transformer = LoopTransform(foundLoops)
    val newLoops = transformer.llvm_transform()
    newLoops.foreach(l => Logger.debug(s"Loop found: ${l.name}"))

    config.analysisDotPath.foreach { s =>
      writeToFile(dotBlockGraph(IRProgram, IRProgram.map(b => b -> b.toString).toMap), s"${s}_graph-after-reduce-$iteration.dot")
      writeToFile(dotBlockGraph(IRProgram, IRProgram.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"${s}_blockgraph-after-reduce-$iteration.dot")
    }

    val mergedSubroutines = subroutines ++ externalAddresses

    val domain = computeDomain(IntraProcIRCursor, IRProgram.procedures)

    Logger.debug("[!] Running ANR")
    val ANRSolver = ANRAnalysisSolver(IRProgram)
    val ANRResult = ANRSolver.analyze()


    Logger.debug("[!] Running RNA")
    val RNASolver = RNAAnalysisSolver(IRProgram)
    val RNAResult = RNASolver.analyze()

    config.analysisResultsPath.foreach(s =>
      writeToFile(printAnalysisResults(IRProgram, ANRResult), s"${s}-anr-result$iteration.txt")
    )
    config.analysisResultsPath.foreach(s =>
      writeToFile(printAnalysisResults(IRProgram, RNAResult), s"${s}-rna-result$iteration.txt")
    )

    Logger.debug("[!] Running Constant Propagation")
    val constPropSolver = ConstantPropagationSolver(IRProgram)
    val constPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] = constPropSolver.analyze()

    Logger.debug("[!] Variable dependency summaries")
    val scc = stronglyConnectedComponents(CallGraph, List(IRProgram.mainProcedure))
    val specGlobalAddresses = ctx.specification.globals.map(s => s.address -> s.name).toMap
    val varDepsSummaries = VariableDependencyAnalysis(IRProgram, ctx.specification.globals, specGlobalAddresses, constPropResult, scc).analyze()

    val ilcpsolver = IRSimpleValueAnalysis.Solver(IRProgram)
    val newCPResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] = ilcpsolver.analyze()

    config.analysisResultsPath.foreach(s =>
      writeToFile(printAnalysisResults(IRProgram, newCPResult), s"${s}_new_ir_constprop$iteration.txt")
    )

    config.analysisDotPath.foreach(f => {
      val dumpdomain = computeDomain[CFGPosition, CFGPosition](InterProcIRCursor, IRProgram.procedures)
      writeToFile(toDot(dumpdomain, InterProcIRCursor, Map.empty), s"${f}_new_ir_intercfg$iteration.dot")
    })

    val reachingDefinitionsAnalysisSolver = ReachingDefinitionsAnalysisSolver(IRProgram)
    val reachingDefinitionsAnalysisResults = reachingDefinitionsAnalysisSolver.analyze()


    config.analysisDotPath.foreach(s => {
      writeToFile(
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> reachingDefinitionsAnalysisResults(b).toString).toMap),
        s"${s}_reachingDefinitions$iteration.dot"
      )
    })


    Logger.debug("[!] Running RegToMemAnalysisSolver")
    val regionAccessesAnalysisSolver = RegionAccessesAnalysisSolver(IRProgram, constPropResult, reachingDefinitionsAnalysisResults)
    val regionAccessesAnalysisResults = regionAccessesAnalysisSolver.analyze()

//     config.analysisDotPath.foreach(s => writeToFile(cfg.toDot(Output.labeler(regionAccessesAnalysisResults, true), Output.dotIder), s"${s}_RegTo$iteration.dot"))
    config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(IRProgram, regionAccessesAnalysisResults), s"${s}_RegTo$iteration.txt"))
    config.analysisDotPath.foreach(s => {
      writeToFile(
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> regionAccessesAnalysisResults(b).toString).toMap),
        s"${s}_RegTo$iteration.dot"
      )
    })


    Logger.debug("[!] Running Constant Propagation with SSA")
    val constPropSolverWithSSA = ConstantPropagationSolverWithSSA(IRProgram, reachingDefinitionsAnalysisResults)
    val constPropResultWithSSA = constPropSolverWithSSA.analyze()


    Logger.debug("[!] Running MRA")
    val mraSolver = MemoryRegionAnalysisSolver(IRProgram, globalAddresses, globalOffsets, mergedSubroutines, constPropResult, ANRResult, RNAResult, regionAccessesAnalysisResults, reachingDefinitionsAnalysisResults)
    val mraResult = mraSolver.analyze()

    config.analysisDotPath.foreach(s => {
      writeToFile(dotCallGraph(IRProgram), s"${s}_callgraph$iteration.dot")
      writeToFile(
        dotBlockGraph(IRProgram, IRProgram.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap),
        s"${s}_blockgraph$iteration.dot"
      )

      writeToFile(
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> newCPResult(b).toString).toMap),
        s"${s}_new_ir_constprop$iteration.dot"
      )

      writeToFile(
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> mraResult(b).toString).toMap),
        s"${s}_MRA$iteration.dot"
      )
    })

    Logger.debug("[!] Running MMM")
    val mmm = MemoryModelMap()
    mmm.convertMemoryRegions(mraResult, mergedSubroutines, globalOffsets, mraSolver.procedureToSharedRegions)
    mmm.logRegions()

    Logger.debug("[!] Running Steensgaard")
    val steensgaardSolver = InterprocSteensgaardAnalysis(IRProgram, constPropResultWithSSA, regionAccessesAnalysisResults, mmm, reachingDefinitionsAnalysisResults, globalOffsets)
    steensgaardSolver.analyze()
    val steensgaardResults = steensgaardSolver.pointsTo()
    val memoryRegionContents = steensgaardSolver.getMemoryRegionContents
    mmm.logRegions(memoryRegionContents)

    // turn fake procedures into diamonds
    Logger.debug("[!] Running VSA")
    val vsaSolver = ValueSetAnalysisSolver(IRProgram, globalAddresses, externalAddresses, globalOffsets, subroutines, mmm, constPropResult)
    val vsaResult: Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]] = vsaSolver.analyze()

    var paramResults: Map[Procedure, Set[Variable]] = Map.empty
    var interLiveVarsResults: Map[CFGPosition, Map[Variable, TwoElement]] = Map.empty

    if (IRProgram.mainProcedure.blocks.nonEmpty && IRProgram.mainProcedure.returnBlock.isDefined && IRProgram.mainProcedure.entryBlock.isDefined) {
      Logger.debug("[!] Running Interprocedural Live Variables Analysis")
      interLiveVarsResults = InterLiveVarsAnalysis(IRProgram).analyze()

      Logger.debug("[!] Running Parameter Analysis")
      paramResults = ParamAnalysis(IRProgram).analyze()

    } else {
      Logger.warn(s"Disabling IDE solver tests due to external main procedure: ${IRProgram.mainProcedure.name}")
    }

    StaticAnalysisContext(
      constPropResult = constPropResult,
      IRconstPropResult = newCPResult,
      memoryRegionResult = mraResult,
      vsaResult = vsaResult,
      interLiveVarsResults = interLiveVarsResults,
      paramResults = paramResults,
      steensgaardResults = steensgaardResults,
      mmmResults = mmm,
      memoryRegionContents = memoryRegionContents,
      reachingDefs = reachingDefinitionsAnalysisResults,
      varDepsSummaries = varDepsSummaries,
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

    // writeToFile(dotBlockGraph(ctx.program, ctx.program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-before-simp.dot")
    Logger.info("[!] Running Simplify")
    val timer = PerformanceTimer("Simplify")
    val write = false

    transforms.applyRPO(ctx.program)

    transforms.removeEmptyBlocks(ctx.program)
    transforms.coalesceBlocks(ctx.program)
    transforms.removeEmptyBlocks(ctx.program)
    if write then writeToFile(dotBlockGraph(ctx.program, ctx.program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-before-dsa.dot")
    
    Logger.info(s"RPO ${timer.checkPoint("RPO")} ms ")
    Logger.info("[!] Simplify :: DynamicSingleAssignment")
    if write then writeToFile(serialiseIL(ctx.program), s"il-before-dsa.il")

    // transforms.DynamicSingleAssignment.applyTransform(ctx.program, liveVars)
    transforms.OnePassDSA().applyTransform(ctx.program)
    Logger.info(s"DSA ${timer.checkPoint("DSA ")} ms ")
    if write then writeToFile(dotBlockGraph(ctx.program, ctx.program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-after-dsa.dot")
    if (ir.eval.SimplifyValidation.validate) {
      // Logger.info("Live vars difftest")
      // val tipLiveVars : Map[CFGPosition, Set[Variable]] = analysis.IntraLiveVarsAnalysis(ctx.program).analyze()
      // assert(ctx.program.procedures.forall(transforms.difftestLiveVars(_, tipLiveVars)))

      Logger.info("DSA Check")
      val x = ctx.program.procedures.forall(transforms.rdDSAProperty)
      assert(x)
      Logger.info("DSA Check passed")
    }
    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))

    if write then writeToFile(serialiseIL(ctx.program), s"il-before-copyprop.il")

    // brute force run the analysis twice because it cleans up more stuff
    //assert(ctx.program.procedures.forall(transforms.rdDSAProperty))
    transforms.doCopyPropTransform(ctx.program)
    if write then writeToFile(dotBlockGraph(ctx.program, ctx.program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-after-simp.dot")
    // assert(ctx.program.procedures.forall(transforms.rdDSAProperty))

    assert(invariant.blockUniqueLabels(ctx.program))
    Logger.info(s"CopyProp ${timer.checkPoint("CopyProp")} ms ")
    if write then writeToFile(serialiseIL(ctx.program), s"il-after-copyprop.il")


    // val x = ctx.program.procedures.forall(transforms.rdDSAProperty)
    //assert(x)
    if (ir.eval.SimplifyValidation.validate) {
      Logger.info("DSA Check (after transform)")
      val x = ctx.program.procedures.forall(transforms.rdDSAProperty)
      assert(x)
      Logger.info("passed")
    }

    // run this after cond recovery because sign bit calculations often need high bits
    // which go away in high level conss
    if write then writeToFile(serialiseIL(ctx.program), s"il-after-slices.il")

    if write then writeToFile(dotBlockGraph(ctx.program, ctx.program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-after-simp.dot")

    // re-apply dsa
    // transforms.OnePassDSA().applyTransform(ctx.program)
    if write then writeToFile(dotBlockGraph(ctx.program, ctx.program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-after-second-dsa.dot")


    if (ir.eval.SimplifyValidation.validate) {
      Logger.info("[!] Simplify :: Writing simplification validation")
      val w = BufferedWriter(FileWriter("rewrites.smt2"))
      ir.eval.SimplifyValidation.makeValidation(w)
      w.close()
    }

    Logger.info("[!] Simplify :: finished")
  }

  def loadAndTranslate(conf: BASILConfig): BASILResult = {
    Logger.debug("[!] Loading Program")
    var q = conf

    var ctx = IRLoading.load(q.loading)

    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))

    ctx = IRTransform.doCleanup(ctx)

    if (q.loading.trimEarly) {
      val before = ctx.program.procedures.size
      transforms.stripUnreachableFunctions(ctx.program, q.loading.procedureTrimDepth)
      Logger.info(
        s"[!] Removed ${before - ctx.program.procedures.size} functions (${ctx.program.procedures.size} remaining)"
      )
    }

    if (q.loading.parameterForm) {

      ir.transforms.clearParams(ctx.program)
      ctx = ir.transforms.liftProcedureCallAbstraction(ctx)
    } else {
      ir.transforms.clearParams(ctx.program)
    }
    assert(invariant.correctCalls(ctx.program))

    ir.eval.SimplifyValidation.validate = conf.validateSimp
    if (conf.simplify) {
      doSimplify(ctx, conf.staticAnalysis)
    }

    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    assert(invariant.correctCalls(ctx.program))

    q.loading.dumpIL.foreach(s => writeToFile(serialiseIL(ctx.program), s"$s-before-analysis.il"))
    val analysis = q.staticAnalysis.map(conf => staticAnalysis(conf, ctx))
    q.loading.dumpIL.foreach(s => writeToFile(serialiseIL(ctx.program), s"$s-after-all-analysis.il"))

    if (q.runInterpret) {
      val fs = eval.interpretTrace(ctx)
      Logger.info("Interpreter Trace:\n" + fs._2.t.mkString("\n"))
      val stopState = fs._1.nextCmd
      if (stopState != eval.Stopped()) {
        Logger.error(s"Interpreter exited with $stopState")
      } else {
        Logger.info("Interpreter stopped normally.")
      }
    }

    IRTransform.prepareForTranslation(q.loading, ctx)

    Logger.info("[!] Translating to Boogie")

    val boogiePrograms = if (q.boogieTranslation.threadSplit && ctx.program.threads.nonEmpty) {
      val outPrograms = ArrayBuffer[BProgram]()
      for (thread <- ctx.program.threads) {
        val fileName = q.outputPrefix.stripSuffix(".bpl") + "_" + thread.entry.name + ".bpl"
        val boogieTranslator = IRToBoogie(ctx.program, ctx.specification, Some(thread), fileName)
        outPrograms.addOne(boogieTranslator.translate(q.boogieTranslation))
      }
      outPrograms
    } else {
      val boogieTranslator = IRToBoogie(ctx.program, ctx.specification, None, q.outputPrefix)
      ArrayBuffer(boogieTranslator.translate(q.boogieTranslation))
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
      val result = StaticAnalysis.analyse(ctx, config, iteration)
      analysisResult.append(result)
      Logger.debug("[!] Replacing Indirect Calls")
      modified = transforms.resolveIndirectCallsUsingPointsTo(
        result.steensgaardResults,
        result.memoryRegionContents,
        result.reachingDefs,
        ctx.program
      )
      Logger.debug("[!] Generating Procedure Summaries")
      if (config.summariseProcedures) {
        IRTransform.generateProcedureSummaries(ctx, ctx.program, result.constPropResult, result.varDepsSummaries)
      }
      if (modified) {
        iteration += 1
        Logger.debug(s"[!] Analysing again (iter $iteration)")
      }
    }

    // should later move this to be inside while (modified) loop and have splitting threads cause further iterations

    if (config.threadSplit) {
      transforms.splitThreads(ctx.program, analysisResult.last.steensgaardResults, analysisResult.last.memoryRegionContents, analysisResult.last.reachingDefs)
    }

    assert(invariant.singleCallBlockEnd(ctx.program))
    Logger.debug(s"[!] Finished indirect call resolution after $iteration iterations")
    analysisResult.last
  }
}

def writeToFile(content: String, fileName: String): Unit = {
  Logger.debug(s"Writing $fileName (${content.size} bytes)")
  val outFile = File(fileName)
  val pw = PrintWriter(outFile, "UTF-8")
  pw.write(content)
  pw.close()
}
