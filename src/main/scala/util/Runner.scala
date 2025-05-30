package util

import ir.*
import ir.transforms.*
import analysis.*

// --- Base Classes ----------------------------------------------------------------------------------------------------

/** Provides a consistent interface for IR transforms.
  * 
  * @param name The name of this transform.
  */
trait Transform(val name: String) {
  
  // the performance of each transform is implicitly tested
  val t = PerformanceTimer(name)

  // code to run before the transform implementation, such as logging information
  protected def preRun(ctx: IRContext): Unit = {}

  // code to run after the transform implementation, such as logging information or assertions
  protected def postRun(ctx: IRContext): Unit = {}

  /** Override this method to implement the logic for your transform.
    * 
    * @param ctx The IR to be modified in-place.
    * @param analyses Use this to access the results of static analyses. Any results not yet generated will be produced
    * automatically and then cached in the manager for later retrieval.
    * 
    * @return The set of analyses that are *preserved* after the transform. To clear all analyses after the transform is
    * invoked, return Set.empty. (Note that this will negatively impact performance.) To preserve all analyses, return
    * analyses.getAll().
    */
  protected def implementation: (ctx: IRContext, analyses: AnalysisManager) => Set[analyses.Memoizer]

  // instances of transforms can be directly called to invoke this method
  def apply(ctx: IRContext, analyses: AnalysisManager): Unit = {
    if (analyses.program ne ctx.program) {
      // the program we are transforming should be the same one for which the analysis results were produced
      throw new RuntimeException(s"Transform $name was passed an AnalysisManager of an IR Program with a different " +
        s"reference value than the program being transformed.")
    }
    preRun(ctx)
    t.checkPoint("start")
    val toPreserve = implementation(ctx, analyses)
    t.checkPoint("end")
    postRun(ctx)
    analyses.invalidateAllExcept(toPreserve)
  }
}

/** A transform can be a sequence of other transforms. We prefer using this over constructing transforms in the
  * implementations of other transforms.
  * 
  * @param name The name of this transform batch.
  * @param transforms The sequence of other transforms that comprise this transform.
  */
trait TransformBatch(name: String, transforms: List[Transform]) extends Transform(name) {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    // simply apply each transform in-turn
    transforms.foreach(_(ctx, analyses))
    Set.empty
  }
}

/** Provides a consistent interface for static analyses.
  * Similar to Transform, but returns a result rather than modifying the IR in-place.
  * 
  * @tparam ReturnType The type of the result that this analysis generates.
  * @param name The name of this analysis.
  */
trait StaticAnalysis[ReturnType](val name: String) {
  
  val t = PerformanceTimer(name)

  protected def preRun(): Unit = {}

  protected def postRun(): Unit = {}

  protected def implementation: (Program, AnalysisManager) => ReturnType

  def apply(prog: Program, analyses: AnalysisManager): ReturnType = {
    if (analyses.program ne prog) {
      throw new RuntimeException(s"Analysis $name was passed an AnalysisManager of an IR Program with a different " +
        s"reference value than the program being transformed.")
    }
    preRun()
    t.checkPoint("start")
    val ret = implementation(prog, analyses)
    t.checkPoint("end")
    postRun(ret)
    ret
  }
}

// 

/** Analysis manager for caching and invalidating analysis results.
  * 
  * @param program Each analysis manager is defined with respect to a particular program reference, which is always
  * passed to the static analyses invoked via this manager. This ensures that the cached analysis results all relate to
  * the same program reference. It is then the responsibility of Transforms to clear these results when they are
  * invalidated by a modification to this program.
  */
class AnalysisManager(program: Program) {

  // memoizer to wrap static analyses and cache their results
  private class Memoizer[ReturnType](analysis: StaticAnalysis[ReturnType]) {

    private var memo: Option[ReturnType] = None

    def invalidate() = { memo = None }

    // allows this memoizer to be called like a function
    def apply(): ReturnType = {
      // pass this analysis manager and its associated program to the static analysis
      if memo.isEmpty then memo = Some(analysis(program, AnalysisManager.this))
      memo.get
    }
  }

  // keep track of all memoizers to ensure we can invalidate all of them
  private val memoizers: Set[Memoizer[?]] = Nil

  // private helper function for creating and storing memoizers
  private def register[ReturnType](analysis: StaticAnalysis[ReturnType]): Memoizer[ReturnType] = {
    val mem = Memoizer[ReturnType](analysis)
    memoizers ::= mem
    return mem
  }

  // list of memoizers - these can be directly called via this manager, e.g. val result = manager.exampleAnalysis()
  val intraProcConstProp = register(IntraProcConstantPropagationAnalysis())
  val interProcConstProp = register(InterProcConstantPropagationAnalysis())
  val memoryRegionResult = register(MemoryRegionAnalysisSolverAnalysis())
  val vsaResult = register(ValueSetAnalysisSolverAnalysis())
  val interLiveVarsResults = register(/* todo */)
  val paramResults = register(/* todo */)
  val steensgaardSolver = register(/* todo */) // fixme: merge these into one analysis result?
  val steensgaardPointsTo = register(/* todo */)
  val steensgaardCallSiteSummary = register(/* todo */)
  val mmmResults = register(/* todo */)
  val reachingDefs = register(/* todo */)
  val regionInjector = register(/* todo */)
  val symbolicAddresses = register(/* todo */)
  val localDSA = register(/* todo */)
  val bottomUpDSA = register(/* todo */)
  val topDownDSA = register(/* todo */)
  val writesToResult = register(/* todo */)
  val ssaResults = register(/* todo */)
  val graResult = register(/* todo */)
  val intraDomain = register(/* todo */)
  val interDomain = register(/* todo */)

  // clears the cached results of all analyses except for those in the given set
  def invalidateAllExcept(exceptions: Set[Memoizer]): Unit =
    memoizers.filterNot(exceptions.contains).foreach(_.invalidate())

  // useful to pass to 'invalidateAllExcept' when we want to preserve all or nearly all results after a transform
  def getAll(): Set[Memoizer[?]] = memoizers // safe to directly return non-mutable set
}

// --- DoCleanup Transform ---------------------------------------------------------------------------------------------

/** Initial cleanup before analysis.
  */
class DoCleanup(doSimplify: Boolean = false) extends TransformBatch("DoCleanup", List(
  MakeProcEntriesNonLoops(),
  CoalesceBlocksFixpoint(),
  ApplyRpo(),
  ReplaceJumpsInNonReturningProcs(),
  ReplaceReturnsTransform(doSimplify),
  RemoveExternalFunctionReferences()
)) {
  override protected def preRun(): Unit = {
    Logger.info("[!] Removing external function calls")
  }

  override protected def postRun(): Unit = {
    assert(invariant.singleCallBlockEnd(ctx.program))
    assert(invariant.cfgCorrect(ctx.program))
    assert(invariant.blocksUniqueToEachProcedure(ctx.program))
    assert(invariant.procEntryNoIncoming(ctx.program))
  }
}

// --- PrepareForTranslation Transform ---------------------------------------------------------------------------------

/** Cull unneccessary information that does not need to be included in the translation, and infer stack regions, and
  * add in modifies from the spec.
  */
class PrepareForTranslation(config: BASILConfig) extends TransformBatch("PrepareForTranslation", List(
  DetermineRelevantMemory(config),
  StripUnreachableFunctions(config),
  StackSubstitution(config),
  SetModifies(),
  RenameBoogieKeywords()
)) {
  override protected def postRun(ctx: IRContext): Unit = {
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
}

// todo: not sure where to put this
class DetermineRelevantMemory(config: BASILConfig) extends Transform("DetermineRelevantMemory") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    if (config.staticAnalysis.isEmpty || (config.staticAnalysis.get.memoryRegions == MemoryRegionsMode.Disabled)) {
      ctx.program.determineRelevantMemory(ctx.globalOffsets)
    }
    Set.empty
  }
}

// todo: not sure where to put this
class StackSubstitution(config: BASILConfig) extends Transform("StackSubstitution") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    if (!config.memoryTransform &&
        (config.staticAnalysis.isEmpty || (config.staticAnalysis.get.memoryRegions == MemoryRegionsMode.Disabled))) {
      StackSubstituter().visitProgram(ctx.program)
    }
    Set.empty
  }
}

// todo: not sure where to put this
class SetModifies extends Transform("SetModifies") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
    ctx.program.setModifies(specModifies)
    Set.empty
  }
}

// todo: not sure where to put this
class RenameBoogieKeywords extends Transform("RenameBoogieKeywords") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    Renamer(boogieReserved).visitProgram(ctx.program)
    Set.empty
  }

  override protected def postRun(ctx: IRContext): Unit = {
    assert(invariant.singleCallBlockEnd(ctx.program))
  }
}

// --- GenerateProcedureSummaries --------------------------------------------------------------------------------------

class GenerateProcedureSummaries(simplified: Boolean = false) extends Transform("GenerateProcedureSummaries") {
  // (?) removed the 'modified' variable that we used to return from this function
  // (?) removed the 'IRProgram' parameter - using ctx.program instead
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    val prog = ctx.program
    // Need to know modifies clauses to generate summaries, but this is probably out of place (fixme)
    val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
    prog.setModifies(specModifies)

    val summaryGenerator = SummaryGenerator(prog, simplified)
    for procedure <- prog.procedures if procedure != prog.mainProcedure do
      procedure.requires = summaryGenerator.generateRequires(procedure)
      procedure.ensures = summaryGenerator.generateEnsures(procedure)

    Set.empty
  }
}

// --- GenerateRelyGuaranteeConditions ---------------------------------------------------------------------------------

class GenerateRgConditions(threads: List[Procedure]) extends Transform("GenerateRgConditions") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
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
    Set.empty
  }
}

// --- DoSimplify Transform --------------------------------------------------------------------------------------------

/*

class DoSimplify(config: BASILConfig) extends TransformBatch("DoSimplify", List(
  // (?) removed logger command: Logger.info("[!] Running Simplify")
  IdentifyLoops(),
  NormaliseBlockNames(),
  SortProcedures(),
  LiftSvComp(),
  DumpIlBeforeSimp(config),
  ApplyRpo(),
  RemoveEmptyBlocks(),
  CoalesceBlocks(),
  RemoveEmptyBlocks(),
  LogBlockgraphBeforeDsa(config),
  LogIrBeforeDsa(config),
  OnePassDsa(),
  InlinePLTLaunchpad(),
  RemoveEmptyBlocks(),
  LogBlockgraphAfterDsa(config),
  LogIrAfterDsa(config),
  LogDsaResultsAndDoChecks(),
  LogIrBeforeCopyProp(),
  LogBlockGraphBeforeCopyProp(),
  CopyProp(),
  FixUpGuards(),
  RemoveDuplicateGuards(),
  LogBlockGraphAfterSimp(),
  LiftLinuxAssertFail(),
  LogIrAfterCopyProp(),
  DsaCheck(),
  LogIrAfterSlices(),
  LogSimplificationValidation()
))

class IdentifyLoops extends Transform("IdentifyLoops") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    val foundLoops = LoopDetector.identify_loops(ctx.program)
    val newLoops = foundLoops.reducibleTransformIR()
    newLoops.updateIrWithLoops()
    Set.empty
  }
}

class NormaliseBlockNames extends Transform("NormaliseBlockNames") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    ctx.program.procedures.foreach(_.normaliseBlockNames())
    Set.empty
  }
}

class SortProcedures extends Transform("SortProcedures") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    ctx.program.sortProceduresRPO()
    Set.empty
  }
}

class LiftSvComp extends Transform("LiftSvComp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    transforms.liftSVComp(ctx.program)
    Set.empty
  }
}

// ???
class DumpIlBeforeSimp(config: BASILConfig) extends Transform("DumpIlBeforeSimp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-simp.il"), pp_prog(ctx.program))
      }
    }
    Set.empty
  }
}

class RemoveEmptyBlocks extends Transform("RemoveEmptyBlocks") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    transforms.removeEmptyBlocks(ctx.program)
    Set.empty
  }
}

class LogBlockgraphBeforeDsa(config: BASILConfig) extends Transform("LogBlockgraphBeforeDsa") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    config.foreach {
      _.analysisDotPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_blockgraph-before-dsa.dot"), dotBlockGraph(ctx.program.mainProcedure))
      }
    }
    Set.empty
  }
}

class LogIrBeforeDsa(config: BASILConfig) extends Transform("LogIrBeforeDsa") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-dsa.il"), pp_prog(ctx.program))
      }
    }
    Set.empty
  }
}

class LogBlockgraphAfterDsa(config: BASILConfig) extends Transform("LogBlockgraphAfterDsa") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
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
    Set.empty
  }
}

class LogIrAfterDsa(config: BASILConfig) extends Transform("LogIrAfterDsa") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-after-dsa.il"), pp_prog(program))
      }
    }
    Set.empty
  }
}

class OnePassDsa extends Transform("OnePassDsa") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    transforms.OnePassDSA().applyTransform(ctx.program)
    Set.empty
  }
}

class InlinePLTLaunchpad extends Transform("InlinePLTLaunchpad") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    transforms.inlinePLTLaunchpad(ctx.program)
    Set.empty
  }
}

class LogDsaResultsAndDoChecks extends Transform("LogDsaResultsAndDoChecks") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    if (ir.eval.SimplifyValidation.validate) {
      Logger.info("DSA no uninitialised")
      assert(invariant.allVariablesAssignedIndex(ctx.program))
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
    Set.empty
  }
}

class LogIrBeforeCopyProp extends Transform("LogIrBeforeCopyProp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-copyprop.il"), pp_prog(ctx.program))
      }
    }
    Set.empty
  }
}

class LogBlockGraphBeforeCopyProp extends Transform("LogBlockGraphBeforeCopyProp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    config.foreach {
      _.analysisDotPath.foreach { s =>
        AnalysisResultDotLogger.writeToFile(
          File(s"${s}_blockgraph-before-copyprop.dot"),
          dotBlockGraph(ctx.program.mainProcedure)
        )
      }
    }
    Set.empty
  }
}

class CopyProp extends Transform("CopyProp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    Logger.info("Copyprop Start")
    transforms.copyPropParamFixedPoint(ctx.program, ctx.globalOffsets)
    Set.empty
  }
}

class FixUpGuards extends Transforms("FixUpGuards") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    transforms.fixupGuards(ctx.program)
    Set.empty
  }
}

class RemoveDuplicateGuards extends Transforms("RemoveDuplicateGuards") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    transforms.removeDuplicateGuard(ctx.program)
    Set.empty
  }
}

class LogBlockGraphAfterSimp extends Transform("LogBlockGraphAfterSimp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    config.foreach {
      _.analysisDotPath.foreach { s =>
        AnalysisResultDotLogger.writeToFile(
          File(s"${s}_blockgraph-after-simp.dot"),
          dotBlockGraph(program.mainProcedure)
        )
      }
    }
    Set.empty
  }
}

class LiftLinuxAssertFail extends Transform("LogBlockGraphAfterSimp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    transforms.liftLinuxAssertFail(ctx)
    assert(invariant.blockUniqueLabels(ctx.program)) // ??? should this be here?
    // (?) removed command: Logger.info(s"CopyProp ${timer.checkPoint("Simplify")} ms ")
    Set.empty
  }
}

class LogIrAfterCopyProp extends Transform("LogBlockGraphAfterSimp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-after-copyprop.il"), pp_prog(program))
      }
    }
    Set.empty
  }
}

class DsaCheck extends Transform("LogBlockGraphAfterSimp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    if (ir.eval.SimplifyValidation.validate) {
      Logger.info("DSA Check (after transform)")
      val x = program.procedures.forall(transforms.rdDSAProperty)
      assert(x)
      Logger.info("DSA Check succeeded")
    }
    Set.empty
  }
}

class LogIrAfterSlices extends Transform("LogBlockGraphAfterSimp") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    config.foreach {
      _.dumpILToPath.foreach { s =>
        DebugDumpIRLogger.writeToFile(File(s"${s}_il-after-slices.il"), pp_prog(program))
      }
    }
    Set.empty
  }
}

class LogSimplificationValidation extends Transform("LogSimplificationValidation") {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer] = {
    if (ir.eval.SimplifyValidation.validate) {
      Logger.info("[!] Simplify :: Writing simplification validation")
      val w = BufferedWriter(FileWriter("rewrites.smt2"))
      ir.eval.SimplifyValidation.makeValidation(w)
      w.close()
    }
    // (?) removed command: Logger.info("[!] Simplify :: finished")
    Set.empty
  }
}


// --- Static Analyses -------------------------------------------------------------------------------------------------

class IntraProcConstantPropagationAnalysis extends StaticAnalysis[
  Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]
]("IntraProcConstantPropagation") {
  def implementation(prog: Program, analyses: AnalysisManager) = IntraProcConstantPropagation(prog).analyze()
}

class InterProcConstantPropagationAnalysis extends StaticAnalysis[
  Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]
]("InterProcConstantPropagation") {
  def implementation(prog: Program, analyses: AnalysisManager) = InterProcConstantPropagation(prog).analyze()
}

class MemoryRegionAnalysisSolverAnalysis extends StaticAnalysis[
  Map[CFGPosition, ((Set[StackRegion], Set[Variable]), Set[HeapRegion])]
]("MemoryRegionAnalysis") {
  def implementation(prog: Program, analyses: AnalysisManager) =
    MemoryRegionAnalysisSolver(
      prog,
      analyses.intraDomain(), // computeDomain(IntraProcIRCursor, prog.procedures).toSet,
      analyses.interProcConstProp(),
      analyses.reachingDefs(),
      analyses.graResult(),
      analyses.mmmResults(),
      analyses.vsaResult()
    ).analyze()
}

class ValueSetAnalysisSolverAnalysis extends StaticAnalysis[
  Map[CFGPosition, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]]
]("VsaAnalysis") {
  def implementation(prog: Program) =
    ValueSetAnalysisSolverAnalysis(prog, analyses.mmmResults()).analyze()
}

class InterLiveVarsAnalysis extends StaticAnalysis[
  Map[CFGPosition, Map[Variable, TwoElement]],
]("InterLiveVarsAnalysis") {
  def implementation(prog: Program, analyses: AnalysisManager) = InterLiveVarsAnalysis(prog).analyze()
}

class ParamResultsAnalysis extends StaticAnalysis[
  Map[Procedure, Set[Variable]]
]("ParamResultsAnalysis") {
  def implementation(prog: Program, analyses: AnalysisManager) = ParamAnalysis(prog).analyze()
}

class SteensgaardGetSolver extends StaticAnalysis[
  Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]]
]("SteensgaardGetSolver") {
  def implementation(prog: Program, analyses: AnalysisManager) =
    val solver = InterprocSteensgaardAnalysis(
      analyses.interDomain(), // computeDomain(InterProcIRCursor, prog.procedures)
      analyses.mmmResults(),
      analyses.ssaResults()
    )
    solver.analyze()
    solver // ???
}

class SteensgaardGetPointsTo extends StaticAnalysis[
  Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]]
]("SteensgaardGetPointsTo") {
  def implementation(prog: Program, analyses: AnalysisManager) = analyses.steensgaardSolver().pointsTo()
}

class SteensgaardGetCallSiteSummary extends StaticAnalysis[
  mutable.Map[DirectCall, Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]]]
]("SteensgaardGetCallSiteSummary") {
  def implementation(prog: Program, analyses: AnalysisManager) = analyses.steensgaardSolver().callSiteSummary
}

class MmmResults extends StaticAnalysis[
  MemoryModelMap
]("MmmResults") {
  def implementation(prog: Program, analyses: AnalysisManager) =
    val mmm = MemoryModelMap(
      globalOffsets,
      mergedSubroutines,
      globalAddresses,
      globalSizes
    )
    mmm.preLoadGlobals()
    mmm.setCallSiteSummaries(steensgaardSolver.callSiteSummary)
    mmm

}

class reachingDefs extends StaticAnalysis("ReachingDefs") {
  ReturnType = Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
  def implementation(prog: Program, analyses: AnalysisManager) = _(prog).analyze()
}

class regionInjector extends StaticAnalysis[Option[RegionInjector]]("RegionInjector") {
  def implementation(prog: Program, analyses: AnalysisManager) = _(prog).analyze()
}

class symbolicAddresses extends StaticAnalysis[Map[CFGPosition, Map[SymbolicAddress, TwoElement]]]("SymbolicAddresses") {
  def implementation(prog: Program, analyses: AnalysisManager) = _(prog).analyze()
}

class localDSA extends StaticAnalysis[Map[Procedure, Graph]]("LocalDSA") {
  def implementation(prog: Program, analyses: AnalysisManager) = _(prog).analyze()
}

class bottomUpDSA extends StaticAnalysis[Map[Procedure, Graph]]("BottomUpDSA") {
  def implementation(prog: Program, analyses: AnalysisManager) = _(prog).analyze()
}

class topDownDSA extends StaticAnalysis[Map[Procedure, Graph]]("TopDownDSA") {
  def implementation(prog: Program, analyses: AnalysisManager) = _(prog).analyze()
}

class writesToResult extends StaticAnalysis[Map[Procedure, Set[Register]]]("WritesTo") {
  def implementation(prog: Program, analyses: AnalysisManager) = _(prog).analyze()
}

class ssaResults extends StaticAnalysis[
  Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
]("SSA") {
  def implementation(prog: Program, analyses: AnalysisManager) = _(prog).analyze()
}

*/
