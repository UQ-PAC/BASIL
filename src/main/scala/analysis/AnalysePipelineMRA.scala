package analysis

import analysis.data_structure_analysis.{DataStructureAnalysis, SymbolicAddress, SymbolicAddressAnalysis}
import analysis.{Interval as _, *}
import boogie.*
import ir.*
import specification.*
import translating.PrettyPrinter.pp_prog
import util.{
  AnalysisResultDotLogger,
  DebugDumpIRLogger,
  Logger,
  MemoryRegionsMode,
  StaticAnalysisConfig,
  StaticAnalysisLogger,
  writeToFile
}

import java.io.File
import scala.collection.mutable

/** Methods relating to program static analysis.
  */
object AnalysisPipelineMRA {

  def reducibleLoops(IRProgram: Program) = {
    StaticAnalysisLogger.debug("reducible loops")

    IRProgram.procedures.foreach { procedure =>
      NewLoopDetector.identify_loops(procedure) match {
        case Some(loops) =>
          loops.values.iterator.flatMap(_.toLoop()).foreach { loop =>
            println(loop)
            LoopTransform.llvm_transform_loop(loop)
          }
        case None => ()
      }
    }

    IRProgram
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

  /** Use static analysis to resolve indirect calls and replace them in the IR until fixed point.
    */
  def runToFixpoint(config: StaticAnalysisConfig, ctx: IRContext): StaticAnalysisContext = {
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
