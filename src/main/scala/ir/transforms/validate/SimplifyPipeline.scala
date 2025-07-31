package ir.transforms.validate
import ir.*
import util.SMT.SatResult
import util.{IRContext, Logger, SimplifyMode}

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

  def sourceToTarget(b: Option[String])(v: Variable | Memory): Option[Expr] = v match {
    case l @ LocalVar(n, t, i) => Some(LocalVar(l.varName, t))
    case g =>
      Some(g)
  }

  validator.getValidationSMT(config, "DSA", sourceToTarget)
}

def copyProp(config: TVJob, p: Program) = {
  def transform(p: Program) = {
    p.procedures.foreach(ir.eval.AlgebraicSimplifications(_))
    p.procedures.map(p => p.name -> transforms.OffsetProp.transform(p)).toMap
  }

  val (validator, results) = validatorForTransform(transform)(p)

  def flowFacts(b: String): Map[Variable, Expr] = {
    results.getOrElse(b, Map())
  }
  def renaming(b: Option[String])(v: Variable | Memory): Option[Expr] = v match {
    case g => Some(g)
  }

  validator.getValidationSMT(config, "CopyProp", renaming, flowFacts)
}

def parameters(config: TVJob, ctx: IRContext) = {
  val (validator, res) =
    validatorForTransform(p => transforms.liftProcedureCallAbstraction(p, Some(ctx.specification)))(ctx.program)

  def sourceToTarget(b: Option[String])(v: Variable | Memory): Option[Expr] = v match {
    case LocalVar(s"${i}_in", t, 0) => Some(GlobalVar(s"$i", t))
    case LocalVar(s"${i}_out", t, 0) => Some(GlobalVar(s"$i", t))
    case LocalVar(n, t, 0) if (!n.startsWith("TRACE")) => Some(GlobalVar(n, t))
    case g => Some(g)
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
  validator.getValidationSMT(config, "NOP", b => v => Some(v))
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
      TVJob(outputPath = filePrefix, verify = verifyMode)
    case _ => TVJob(None, None)
  }
  transforms.applyRPO(p)
  nop(config, p)
  val (res, nctx) = parameters(config, ctx)
  config = res
  config = assumePreservedParams(config, p)
  transforms.applyRPO(p)
  config = simplifyCFGValidated(config, p)
  transforms.applyRPO(p)
  config = dynamicSingleAssignment(config, p)
  transforms.applyRPO(p)
  config = copyProp(config, p)
  transforms.applyRPO(p)
  config = guardCleanup(config, p)

  val failed = config.results.filter(_.verified.exists(_.isInstanceOf[SatResult.SAT]))

  if (failed.nonEmpty) {
    val fnames = failed.map(f => s"  ${f.runName}::${f.proc} ${f.smtFile}").mkString("\n")
    // Logger.error(s"Failing cases: $fnames")
    throw Exception(s"TranslationValidationFailed: $fnames")
  } else if (config.verify.isDefined) {
    Logger.info("[!] Translation validation passed")
  }

  ir.transforms.genStackAllocationSpec(p)

  (config, nctx)
}
