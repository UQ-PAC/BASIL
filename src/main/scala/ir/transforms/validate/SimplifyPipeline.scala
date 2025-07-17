package ir.transforms.validate
import ir.*

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

def simplifyCFGValidated(p: Program) = {
  wrapShapePreservingTransformInValidation(simplifyCFG, "simplifyCFG")(p)
}

def dynamicSingleAssignment(p: Program) = {
  val validator = TranslationValidator()
  validator.setTargetProg(p)
  transforms.OnePassDSA().applyTransform(p)
  validator.setSourceProg(p)

  def sourceToTarget(b: Option[String])(v: Variable | Memory): Option[Expr] = v match {
    case l @ LocalVar(n, t, i) => Some(LocalVar(l.varName, t))
    case g =>
      Some(g)
  }

  def targetToSource(b: Option[String])(v: Variable | Memory): Option[Expr] = v match {
    case g: GlobalVar => None
    case o => None
  }

  // validator.setEqualVarsInvariantRenaming(renamingSrcTgt = sourceToTarget)

  validator.getValidationSMT(sourceToTarget, "tvsmt/" + "DSA")
}

def copyProp(p: Program) = {
  val validator = TranslationValidator()
  validator.setTargetProg(p)
  // transform

  p.procedures.foreach(ir.eval.AlgebraicSimplifications(_))
  val results: Map[String, Map[Variable, Expr]] =
    p.procedures.map(p => p.name -> transforms.OffsetProp.transform(p)).toMap

  // end transform
  validator.setSourceProg(p)

  def nopRenaming(b: Option[String])(v: Variable | Memory): Option[Expr] = None

  def renaming(b: Option[String])(v: Variable | Memory): Option[Expr] = v match {
    case v: Variable if b.isDefined && results.contains(b.get) => results(b.get).get(v).orElse(Some(v))
    case g => Some(g)
  }

  validator.getValidationSMT(renaming, "tvsmt/" + "CopyProp")
}

def parameters(p: Program) = {
  val validator = TranslationValidator()
  validator.setTargetProg(p)
  transforms.liftProcedureCallAbstraction(p, None)
  validator.setSourceProg(p)

  def sourceToTarget(b: Option[String])(v: Variable | Memory): Option[Expr] = v match {
    case LocalVar(s"${i}_in", t, 0) => Some(GlobalVar(s"$i", t))
    case LocalVar(s"${i}_out", t, 0) => Some(GlobalVar(s"$i", t))
    case LocalVar(n, t, 0) if (!n.startsWith("TRACE")) => Some(GlobalVar(n, t))
    case g => Some(g)
  }

  // validator.setEqualVarsInvariantRenaming(renamingSrcTgt = sourceToTarget)
  validator.getValidationSMT(sourceToTarget, "tvsmt/" + "Parameters")

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
  transforms.removeDuplicateGuard(p)
  deadAssignmentElimination(p)
  simplifyCFG(p)
}

def guardCleanup(p: Program) = {
  wrapShapePreservingTransformInValidation(guardCleanupTransforms, "GuardCleanup")(p)
}

def nop(p: Program) = {
  val validator = TranslationValidator()
  validator.setTargetProg(p)
  validator.setSourceProg(p)
  validator.getValidationSMT(b => v => Some(v), "tvsmt/" + "NOP")
}

def validatedSimplifyPipeline(p: Program) = {
  transforms.applyRPO(p)
  parameters(p)
  // transforms.liftProcedureCallAbstraction(p, None)
  transforms.applyRPO(p)
  simplifyCFG(p)
  transforms.applyRPO(p)
  dynamicSingleAssignment(p)
  transforms.applyRPO(p)
  nop(p)
  transforms.applyRPO(p)
  copyProp(p)
  transforms.applyRPO(p)
  guardCleanup(p)
  ()
}
