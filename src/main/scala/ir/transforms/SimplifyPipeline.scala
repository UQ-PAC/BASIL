package ir.transforms

import analysis.{AnalysisManager, Interval as _, *}
import ir.*
import ir.dsl.IRToDSL
import server.IREpoch
import translating.PrettyPrinter.*
import util.RunUtils.addEpochSnapshot
import util.{AnalysisResultDotLogger, DebugDumpIRLogger, Logger, PerformanceTimer, StaticAnalysisConfig}

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.ArrayBuffer

def doSimplify(
  ctx: IRContext,
  config: Option[StaticAnalysisConfig],
  collectedEpochs: Option[ArrayBuffer[IREpoch]] = None
): Unit = {
  // writeToFile(dotBlockGraph(program, program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-before-simp.dot")
  Logger.info("[!] Running Simplify")
  val timer = PerformanceTimer("Simplify")
  val program = ctx.program

  val beforeIrreducibleLoops = IRToDSL.convertProgram(ctx.program).resolve
  IrreducibleLoops.transform_all_and_update(program)
  val afterIrreducibleLoops = IRToDSL.convertProgram(ctx.program).resolve
  addEpochSnapshot("irreducible_loops", beforeIrreducibleLoops, afterIrreducibleLoops, collectedEpochs)

  val beforeNormaliseBlockNames = IRToDSL.convertProgram(ctx.program).resolve
  for (p <- program.procedures) {
    p.normaliseBlockNames()
  }
  val afterNormaliseBlockNames = IRToDSL.convertProgram(ctx.program).resolve
  addEpochSnapshot("normalise_block_names", beforeNormaliseBlockNames, afterNormaliseBlockNames, collectedEpochs)

  ctx.program.sortProceduresRPO()

  val beforeLiftSVComp = IRToDSL.convertProgram(ctx.program).resolve
  transforms.liftSVComp(ctx.program)
  val afterLiftSVComp = IRToDSL.convertProgram(ctx.program).resolve
  addEpochSnapshot("lift_svcomp", beforeLiftSVComp, afterLiftSVComp, collectedEpochs)

  config.foreach {
    _.dumpILToPath.foreach { s =>
      DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-simp.il"), pp_prog(program))
    }
  }

  transforms.applyRPO(program)

  // example of printing a simple analysis

  val beforeSimplify = IRToDSL.convertProgram(ctx.program).resolve
  transforms.removeEmptyBlocks(program)
  transforms.coalesceBlocks(program)
  transforms.removeEmptyBlocks(program)
  val afterSimplify = IRToDSL.convertProgram(ctx.program).resolve
  addEpochSnapshot("simplify_blocks", beforeSimplify, afterSimplify, collectedEpochs)

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

  val beforeOnePassDSA = IRToDSL.convertProgram(ctx.program).resolve
  transforms.OnePassDSA().applyTransform(program)
  val afterOnePassDSA = IRToDSL.convertProgram(ctx.program).resolve
  addEpochSnapshot("one_pass_DSA", beforeOnePassDSA, afterOnePassDSA, collectedEpochs)

  val beforeInlinePLTLaunchpad = IRToDSL.convertProgram(ctx.program).resolve
  // fixme: this used to be a plain function but now we have to supply an analysis manager!
  transforms.inlinePLTLaunchpad(ctx, AnalysisManager(ctx.program))
  val afterInlinePLTLaunchpad = IRToDSL.convertProgram(ctx.program).resolve
  addEpochSnapshot("inline_plt_launchpad", beforeInlinePLTLaunchpad, afterInlinePLTLaunchpad, collectedEpochs)

  val beforeEmptyBlocksRemovedProg = IRToDSL.convertProgram(ctx.program).resolve
  transforms.removeEmptyBlocks(program)
  val afterEmptyBlocksRemovedProg = IRToDSL.convertProgram(ctx.program).resolve
  addEpochSnapshot("remove_empty_blocks", beforeEmptyBlocksRemovedProg, afterEmptyBlocksRemovedProg, collectedEpochs)

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
  val beforeCopyPropagationProg = IRToDSL.convertProgram(program).resolve
  transforms.copyPropParamFixedPoint(program, ctx.globalOffsets)
  val afterCopyPropagationProg = IRToDSL.convertProgram(program).resolve
  addEpochSnapshot("copy_propagation", beforeCopyPropagationProg, afterCopyPropagationProg, collectedEpochs)

  val beforeGuardOptimizationsProgram = IRToDSL.convertProgram(program).resolve
  transforms.fixupGuards(program)
  transforms.removeDuplicateGuard(program)
  config.foreach {
    _.analysisDotPath.foreach { s =>
      AnalysisResultDotLogger.writeToFile(File(s"${s}_blockgraph-after-simp.dot"), dotBlockGraph(program.mainProcedure))
    }
  }
  val afterGuardOptimizationsProgram = IRToDSL.convertProgram(program).resolve
  addEpochSnapshot(
    "guard_optimisations",
    beforeGuardOptimizationsProgram,
    afterGuardOptimizationsProgram,
    collectedEpochs
  )

  val beforeLiftLinuxAssertFailProg = IRToDSL.convertProgram(ctx.program).resolve
  transforms.liftLinuxAssertFail(ctx)
  val afterLiftLinuxAssertFailProg = IRToDSL.convertProgram(ctx.program).resolve
  addEpochSnapshot(
    "lift_linux_assert_fail",
    beforeLiftLinuxAssertFailProg,
    afterLiftLinuxAssertFailProg,
    collectedEpochs
  )

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
