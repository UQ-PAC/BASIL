package ir.transforms

import analysis.{AnalysisManager, Interval as _, *}
import ir.*
import ir.dsl.IRToDSL
import server.IREpoch
import translating.PrettyPrinter.*
import util.RunUtils.logTransform
import util.{DebugDumpIRLogger, Logger, PerformanceTimer}

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable.ArrayBuffer

def doSimplify(
  ctx: IRContext,
  dumpILToPath: Option[String] = None,
  collectedSnapshots: Option[ArrayBuffer[IREpoch]] = None
): Unit = {
  // writeToFile(dotBlockGraph(program, program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"blockgraph-before-simp.dot")
  Logger.info("[!] Running Simplify")
  val timer = PerformanceTimer("Simplify")
  val program = ctx.program

  logTransform(collectedSnapshots)("irreducible_loops", ctx => IrreducibleLoops.transform_all_and_update(ctx.program))(
    ctx
  )

  logTransform(collectedSnapshots)("normalise block names", _.program.procedures.foreach(p => p.normaliseBlockNames()))(
    ctx
  )

  ctx.program.sortProceduresRPO()

  val beforeLiftSVComp = IRToDSL.convertProgram(ctx.program).resolve
  logTransform(collectedSnapshots)("lift_svcomp", c => transforms.liftSVComp(c.program))(ctx)

  dumpILToPath.foreach { s =>
    DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-simp.il"), pp_prog(program))
  }

  transforms.applyRPO(program)

  logTransform(collectedSnapshots)(
    "simplifyCFG",
    c => {
      transforms.removeEmptyBlocks(c.program)
      transforms.coalesceBlocks(c.program)
      transforms.removeEmptyBlocks(c.program)
    }
  )(ctx)

  Logger.info("[!] Simplify :: DynamicSingleAssignment")
  dumpILToPath.foreach { s =>
    DebugDumpIRLogger.writeToFile(File(s"${s}_il-before-dsa.il"), pp_prog(program))
  }

  logTransform(collectedSnapshots)("SSA-DSA", c => transforms.OnePassDSA().applyTransform(c.program))(ctx)

  assert(ir.invariant.readUninitialised(ctx.program))
  logTransform(collectedSnapshots)("inline PLT", c => transforms.inlinePLTLaunchpad(ctx, AnalysisManager(ctx.program)))(
    ctx
  )

  logTransform(collectedSnapshots)("removeEmptyBlock", c => transforms.removeEmptyBlocks(c.program))(ctx)

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
  logTransform(collectedSnapshots)(
    "linear expr prop",
    c => { 
      transforms.copyPropParamFixedPoint(c.program);
      assert(ir.invariant.readUninitialised(ctx.program))
      c 
    }
  )(ctx)

  logTransform(collectedSnapshots)(
    "simplify guards",
    c => {
      transforms.fixupGuards(c.program)
      transforms.removeDuplicateGuard(c.program)
    }
  )(ctx)
  logTransform(collectedSnapshots)("lift linux assert", transforms.liftLinuxAssertFail)(ctx)

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
