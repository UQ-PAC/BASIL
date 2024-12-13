package analysis.data_structure_analysis

import analysis.{Analysis, FlatElement}
import ir.{BitVecLiteral, CFGPosition, Expr, IntraProcIRCursor, MemoryLoad, MemoryStore, Procedure, Program, Variable, computeDomain}

import scala.collection.mutable


trait ConstraintArg(val SSAVar: Map[SymBase, Option[Set[BitVecLiteral]]])
case class EEV(override val SSAVar: Map[SymBase, Option[Set[BitVecLiteral]]]) extends ConstraintArg(SSAVar)
case class EV(override val SSAVar: Map[SymBase, Option[Set[BitVecLiteral]]]) extends ConstraintArg(SSAVar)


trait Constraint(val pos: CFGPosition, val expr1: Expr, val expr2: Expr, val arg1: ConstraintArg, val arg2: ConstraintArg, val size: Int = 0)


case class DereferenceConstraint(override val pos: CFGPosition, value: Expr, index: Expr, override val arg1: EV, override val arg2: EEV, override val size: Int) extends Constraint(pos, value, index, arg1, arg2, size)
//case class AssignmentConstraint(arg1: EV, arg2: EV) extends Constraint(arg1, arg2)

class ConstraintGen(proc: Procedure,  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends Analysis[Set[Constraint]] {
  
  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, Set(proc)).toSet
  val sva = SVA(proc, constProp)

  override def analyze(): Set[Constraint] = {
    var constraints: Set[Constraint] = Set.empty
    domain.foreach {
      case load @ MemoryLoad(lhs, _, index, _, size, _) =>
        constraints += DereferenceConstraint(load, lhs, index, EV(sva.exprToSymValSet(load, lhs)), EEV(sva.exprToSymValSet(load, index)), size/8)
      case store @ MemoryStore(_, index, value, _, size, _) =>
        constraints += DereferenceConstraint(store, value, index, EV(sva.exprToSymValSet(store, value)), EEV(sva.exprToSymValSet(store, index)), size/8)
      case _ =>
    }

    constraints
  }
}

