package ir.transforms.validate
import ir.*
import util.SMT.SatResult
import util.{IRContext, Logger, SimplifyMode}

import java.io.File

import cilvisitor.{visit_proc, visit_prog}

/**
 * Translation-validated simplification pipeline.
 */

def simplifyCFG(p: Program) = {
  for (p <- p.procedures) {
    while (transforms.coalesceBlocks(p)) {}
    transforms.removeEmptyBlocks(p)
  }
}

def simplifyCFGValidated(config: TVJob, p: Program): TVJob = {
  wrapShapePreservingTransformInValidation(config, simplifyCFG, "simplifyCFG")(p)
}

def dynamicSingleAssignment(config: TVJob, p: Program) = {
  val validator = TranslationValidator()
  validator.setTargetProg(p)
  transforms.OnePassDSA().applyTransform(p)
  validator.setSourceProg(p)

  def sourceToTarget(p: ProcID, b: Option[BlockID])(v: Variable | Memory) = v match {
    case l @ LocalVar(n, t, i) => Seq(LocalVar(l.varName, t))
    case g => Seq(g)
  }

  validator.getValidationSMT(config, "DSA", sourceToTarget)
}

def dsaCopyPropCombined(config: TVJob, p: Program) = {

  def dsa(p: Program) = transforms.OnePassDSA().applyTransform(p)
  def copyprop(p: Program) =
    p.procedures.foreach(ir.eval.AlgebraicSimplifications(_))
    p.procedures.map(p => p.name -> transforms.OffsetProp.transform(p)).toMap

  def transform(p: Program) = {
    val dr = dsa(p)
    val cp = copyprop(p)
    (dr, cp)
  }

  def sourceToTarget(b: Option[String])(v: Variable | Memory): Option[Expr] = v match {
    case l @ LocalVar(n, t, i) => Some(LocalVar(l.varName, t))
    case g =>
      Some(g)
  }

  val (validator, (dsaRes, copypropRes)) = validatorForTransform(transform)(p)
}

def copyProp(config: TVJob, p: Program) = {
  def transform(p: Program) = {
    p.procedures.foreach(ir.eval.AlgebraicSimplifications(_))
    val r = p.procedures.map(p => p.name -> transforms.OffsetProp.transform(p)).toMap
    transforms.CleanupAssignments().transform(p)
    r
  }

  val (validator, results) = validatorForTransform(transform)(p)

  def flowFacts(b: String): Map[Variable, Expr] = {
    results.getOrElse(b, Map())
  }

  val revResults: Map[ProcID, Map[Variable, Set[Variable]]] = results.map { case (p, r) =>
    val m: Map[Variable, Set[Variable]] = (r.toSeq
      .collect { case (v1: Variable, v2: Variable) =>
        v2 -> v1
      }
      .groupBy(_._1)
      .map { case (k, v) =>
        (k, v.map(_._2).toSet)
      })
    p -> m
  }.toMap

  def renamingTgt(proc: ProcID, b: Option[BlockID])(v: Variable | Memory) = v match {
    case v: Variable => results.get(proc).flatMap(_.get(v)).toSeq
  }

  def renaming(proc: ProcID, b: Option[BlockID])(v: Variable | Memory) = v match {
    case g =>
      Seq(g) ++ (v match {
        case v: Variable =>
          val rr = revResults.get(proc).toSeq.flatMap(_.get(v).toSeq.flatten)
          results.get(proc).flatMap(_.get(v)) ++ rr
        case _ => Seq()
      })
  }

  validator.getValidationSMT(config, "CopyProp", renaming, renamingTgt)
}

def parameters(config: TVJob, ctx: IRContext) = {

  val localInTarget = ctx.program.procedures.view.map { case p =>
    p.name -> (freeVarsPos(p).collect { case l: LocalVar =>
      l
    }.toSet)
  }.toMap

  val (validator, res) =
    validatorForTransform(p => transforms.liftProcedureCallAbstraction(p, Some(ctx.specification)))(ctx.program)

  val entryBlocks = ctx.program.procedures.collect {
    case p if p.entryBlock.isDefined => p.name -> p.entryBlock.get.label
  }.toMap

  val returnBlocks = ctx.program.procedures.collect {
    case p if p.returnBlock.isDefined => p.name -> p.returnBlock.get.label
  }.toMap

  def sourceToTarget(p: ProcID, b: Option[String])(v: Variable | Memory): Seq[Expr] = v match {
    // in/out params only map to the registers at the procedure entry and exit
    case LocalVar(s"${i}_in", t, 0) if b.forall(_.endsWith("ENTRY")) => Seq(GlobalVar(s"$i", t))
    case LocalVar(s"${i}_out", t, 0) if b.forall(_.endsWith("EXIT")) => Seq(GlobalVar(s"$i", t))
    case LocalVar(s"${i}_in", t, 0) if b.forall(b => entryBlocks.get(p).contains(b)) => Seq(GlobalVar(s"$i", t))
    case LocalVar(s"${i}_out", t, 0) if b.forall(b => returnBlocks.get(p).contains(b)) => Seq(GlobalVar(s"$i", t))
    case LocalVar(s"${i}_in", t, 0) if b.forall(_ == p) => Seq(GlobalVar(s"$i", t))
    case LocalVar(s"${i}_out", t, 0) if b.forall(_ == p) => Seq(GlobalVar(s"$i", t))
    case LocalVar(s"${i}_in", t, 0) =>
      Seq()
    case LocalVar(s"${i}_out", t, 0) =>
      Seq()

    case local @ LocalVar(n, t, 0) if localInTarget.get(p).exists(_.contains(local)) =>
      // local variables
      Seq(local)
    case LocalVar(n, t, 0) =>
      // the rest map to global variables with the same name
      Seq(GlobalVar(n, t))
    case g => Seq(g)
  }

  (validator.getValidationSMT(config, "Parameters", sourceToTarget), ctx.copy(specification = res.get))

}

def guardCleanupTransforms(p: Program) = {
  def simplifyGuards(prog: Program) = {
    for (p <- prog.procedures) {
      transforms.fixupGuards(p)
      val gvis = transforms.GuardVisitor()
      visit_proc(gvis, p)
    }
  }

  def deadAssignmentElimination(prog: Program) = {
    transforms.CleanupAssignments().transform(prog)
    visit_prog(transforms.CleanupAssignments(), prog)
  }

  p.procedures.foreach(ir.eval.AlgebraicSimplifications(_))
  simplifyGuards(p)
  p.procedures.foreach(p => {
    ir.eval.AlgebraicSimplifications(p)
    ir.eval.AssumeConditionSimplifications(p)
    ir.eval.AlgebraicSimplifications(p)
    ir.eval.cleanupSimplify(p)
  })
  deadAssignmentElimination(p)
  simplifyCFG(p)
  transforms.removeDuplicateGuard(p)
}

def guardCleanup(config: TVJob, p: Program) = {
  wrapShapePreservingTransformInValidation(config, guardCleanupTransforms, "GuardCleanup")(p)
}

def nop(config: TVJob, p: Program) = {
  val validator = TranslationValidator()
  validator.setTargetProg(p)
  validator.setSourceProg(p)
  validator.getValidationSMT(config, "NOP", (p, b) => v => Seq(v))
}

def assumePreservedParams(config: TVJob, p: Program) = {
  val (validator, asserts) = validatorForTransform(transforms.CalleePreservedParam.transform)(p)
  validator.getValidationSMT(config, "AssumeCallPreserved", introducedAsserts = asserts.toSet)
}

def validatedSimplifyPipeline(ctx: IRContext, mode: util.SimplifyMode): (TVJob, IRContext) = {
  // passing through ircontext like this just for spec transform is horrible
  // also the translation validation doesn't really consider spec at all
  // maybe it should; in ackermann phase and observable variables...
  val p = ctx.program
  var config = mode match {
    case SimplifyMode.ValidatedSimplify(verifyMode, filePrefix) =>
      TVJob(outputPath = filePrefix, verify = verifyMode, debugDumpAlways = false)
    case _ => TVJob(None, None)
  }
  transforms.applyRPO(p)
  // nop(config, p)
  // Logger.writeToFile(File("beforeParams.il"), translating.PrettyPrinter.pp_prog(ctx.program))
  val (res, nctx) = parameters(config, ctx)
  config = res
  config = assumePreservedParams(config, p)
  transforms.applyRPO(p)
  config = simplifyCFGValidated(config, p)
  transforms.applyRPO(p)
  config = dynamicSingleAssignment(config, p)
  transforms.applyRPO(p)
  // config = config.copy(verify = Some(util.SMT.Solver.Z3))
  config = copyProp(config, p)
  transforms.applyRPO(p)
  config = guardCleanup(config, p)

  val failed = config.results.filter(_.verified.exists(_.isInstanceOf[SatResult.SAT]))

  if (failed.nonEmpty) {
    val fnames = failed.map(f => s"  ${f.runName}::${f.proc} ${f.smtFile}").reverse.mkString("\n  ")
    // Logger.error(s"Failing cases: $fnames")
    throw Exception(s"TranslationValidationFailed:\n  $fnames")
  } else if (config.verify.isDefined) {
    Logger.info("[!] Translation validation passed")
  }

  config.outputPath.foreach(p => {
    val csv = (config.results.map(_.toCSV).groupBy(_._1).toList match {
      case (h, vs) :: Nil => h :: vs.map(_._2)
      case _ => {
        Logger.error("Broken header structure for csv metrics file")
        List()
      }
    }).mkString("\n")
    Logger.writeToFile(File(p + "/stats.csv"), csv)
  })

  ir.transforms.genStackAllocationSpec(p)

  (config, nctx)
}
