package ir.transforms

import analysis.{AnalysisManager, Interval as _, *}
import ir.*
import translating.PrettyPrinter.*
import util.{AnalysisResultDotLogger, DebugDumpIRLogger, Logger, PerformanceTimer, StaticAnalysisConfig}

import java.io.{BufferedWriter, File, FileWriter}

def doSimplify(ctx: IRContext, dumpILToPath: Option[String] = None): Unit = {
  Logger.info("[!] Running Simplify")
  val timer = PerformanceTimer("Simplify")
  val program = ctx.program

  IrreducibleLoops.transform_all_and_update(program)

  for (p <- program.procedures) {
    p.normaliseBlockNames()
  }

  ctx.program.sortProceduresRPO()

  transforms.liftSVComp(ctx.program)

  dumpILToPath.foreach { s =>
    DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-simp.il"), pp_prog(program))
  }

  transforms.applyRPO(program)

  // example of printing a simple analysis

  transforms.removeEmptyBlocks(program)
  transforms.coalesceBlocks(program)
  transforms.removeEmptyBlocks(program)

  // transforms.coalesceBlocksCrossBranchDependency(program)

  Logger.info("[!] Simplify :: DynamicSingleAssignment")
  dumpILToPath.foreach { s =>
    DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-dsa.il"), pp_prog(program))
  }

  transforms.OnePassDSA().applyTransform(program)

  assert(ir.invariant.readUninitialised(ctx.program))
  // fixme: this used to be a plain function but now we have to supply an analysis manager!
  transforms.inlinePLTLaunchpad(ctx, AnalysisManager(ctx.program))

  transforms.removeEmptyBlocks(program)

  dumpILToPath.foreach { s =>
    DebugDumpIRLogger.writeToFile(File(s"${s}_il-after-dsa.il"), pp_prog(program))
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

  dumpILToPath.foreach { s =>
    DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-copyprop.il"), pp_prog(program))
  }

  // brute force run the analysis twice because it cleans up more stuff
  // assert(program.procedures.forall(transforms.rdDSAProperty))

  Logger.info("Copyprop Start")
  transforms.copyPropParamFixedPoint(program)
  assert(ir.invariant.readUninitialised(ctx.program))

  transforms.fixupGuards(program)
  transforms.removeDuplicateGuard(program)

  transforms.liftLinuxAssertFail(ctx)

  // assert(program.procedures.forall(transforms.rdDSAProperty))

  assert(invariant.blockUniqueLabels(program))
  Logger.info(s"CopyProp ${timer.checkPoint("Simplify")} ms ")

  dumpILToPath.foreach { s =>
    DebugDumpIRLogger.writeToFile(File(s"${s}_il-after-copyprop.il"), pp_prog(program))
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
  dumpILToPath.foreach { s =>
    DebugDumpIRLogger.writeToFile(File(s"${s}_il-after-slices.il"), pp_prog(program))
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
