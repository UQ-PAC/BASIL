package analysis.data_structure_analysis

import ir.{Call, DirectCall, Expr, IndirectCall, LocalVar, MemoryLoad, MemoryStore, Procedure, Statement, Variable}
import util.ConstGenLogger

sealed trait Constraint {
  def source: Statement
  val label: String
  def eval(evaluator: Expr => Any = identity): String
}

def content(value: Any): String = s"[|${value.toString}|]}"

case class ConstraintArg(value: Expr, contents: Boolean = false) {
  override def toString: String = eval()
  def ignoreContents: ConstraintArg = ConstraintArg(value)
  def eval(evaluator: Expr => Any = identity): String = {
    val evaluated = evaluator(value)
    if contents then content(evaluated) else evaluated.toString
  }
}

sealed trait BinaryConstraint extends Constraint {
  val arg1: ConstraintArg
  val arg2: ConstraintArg

  private def name = this.getClass.getSimpleName
  override def toString: String = eval()
  override def eval(evaluator: Expr => Any = identity): String =
    s"(${arg1.eval(evaluator)} <==> ${arg2.eval(evaluator)}"
}

case class AssignmentConstraint(pos: Statement, ar1: Expr, ar2: Expr) extends BinaryConstraint {
  override def source: Statement = pos
  override val label: String = labelToPC(Some(pos.toString))
  override val arg1: ConstraintArg = ConstraintArg(ar1)
  override val arg2: ConstraintArg = ConstraintArg(ar2)
}

sealed trait MemoryAccessConstraint[T <: MemoryStore | MemoryLoad](pos: T, index: Expr, value: Expr, val size: Int)
    extends BinaryConstraint {
  val label: String = labelToPC(pos.label)
  override def source: T = pos

  override val arg1: ConstraintArg = ConstraintArg(index, true)
  override val arg2: ConstraintArg = ConstraintArg(value)

  override def toString: String = eval()

  override def eval(evaluator: Expr => Any = identity): String =
    s"${this.getClass.getSimpleName}($label, size: $size): " + super.eval(evaluator)
}

case class MemoryReadConstraint(pos: MemoryLoad)
    extends MemoryAccessConstraint[MemoryLoad](pos, pos.index, pos.lhs, pos.size / 8)

case class MemoryWriteConstraint(pos: MemoryStore)
    extends MemoryAccessConstraint[MemoryStore](pos, pos.index, pos.value, pos.size / 8)

sealed trait CallConstraint[T <: Call, V <: Procedure | Variable](call: T) extends Constraint {
  val label: String = labelToPC(call.label)
  def caller: Procedure = call.parent.parent
  def target: V
  val inParams: Map[LocalVar, Expr]
  val outParams: Map[LocalVar, LocalVar]
  def inConstraints: Set[AssignmentConstraint] =
    inParams.map(pair => AssignmentConstraint(call, pair._1, pair._2)).toSet
  def outConstraints: Set[AssignmentConstraint] =
    outParams.map(pair => AssignmentConstraint(call, pair._1, pair._2)).toSet
  override def source: T = call
  override def toString: String = eval()

  override def eval(evaluator: Expr => Any = identity): String = {
    s"${this.getClass.getSimpleName}($label, Proc ${caller.name} calls $target" ++ s"\nIn Param Constraints: ${inConstraints.map(_.eval(evaluator))}"
      ++ s"\nOut Param Constraints: ${outConstraints.map(_.eval(evaluator))}"
  }
}

case class DirectCallConstraint(call: DirectCall) extends CallConstraint[DirectCall, Procedure](call) {
  override def target: Procedure = call.target

  override val inParams: Map[LocalVar, Expr] = call.actualParams
  override val outParams: Map[LocalVar, LocalVar] = call.outParams.view.mapValues(_.asInstanceOf[LocalVar]).toMap
}

case class IndirectCallConstraint(call: IndirectCall) extends CallConstraint[IndirectCall, Variable](call) {

  override def target: Variable = call.target

  override val inParams: Map[LocalVar, Expr] = Map.empty
  override val outParams: Map[LocalVar, LocalVar] = Map.empty
}

def generateConstraints(proc: Procedure): Set[Constraint] = {
  ConstGenLogger.info(s"Generating Constraints for ${proc.name}")
  var constraints: Set[Constraint] = Set.empty

  proc.foreach {
    case load: MemoryLoad =>
      constraints += MemoryReadConstraint(load)
    case write: MemoryStore =>
      constraints += MemoryWriteConstraint(write)
    case call: DirectCall =>
      constraints += DirectCallConstraint(call)
    case indirectCall: IndirectCall => constraints += IndirectCallConstraint(indirectCall)
    case _ =>
  }

  constraints
}
