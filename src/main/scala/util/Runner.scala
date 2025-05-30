package util

import ir.*
import ir.transforms.*
import analysis.*

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
