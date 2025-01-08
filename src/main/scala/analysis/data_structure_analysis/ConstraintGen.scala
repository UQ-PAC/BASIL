package analysis.data_structure_analysis

import analysis.{Analysis, FlatElement, Loop}
import ir.{BitVecLiteral, CFGPosition, Expr, IntraProcIRCursor, MemoryLoad, MemoryStore, Procedure, Program, Variable, computeDomain}

import scala.collection.mutable


trait ConstraintArg(val SSAVar: Map[SymBase, Option[Set[Int]]])
case class EEV(override val SSAVar: Map[SymBase, Option[Set[Int]]]) extends ConstraintArg(SSAVar)
case class EV(override val SSAVar: Map[SymBase, Option[Set[Int]]]) extends ConstraintArg(SSAVar)

trait Counter(val init: Int = 0) {
  private var counter = init
  def increment(by: Int = 1): Int = {
      counter += by
      counter
    }
  
  def decrement(by: Int = 1): Int = {
    counter -= by
    counter
  }
  
  def get: Int = counter
  
  def reset(): Unit = counter = init
}

object ConstraintCounter extends Counter

case class Constraint(pos: CFGPosition, value: Expr, index: Expr, arg1: EV, arg2: EEV, size: Int, id: Int = ConstraintCounter.increment()) {

  override def toString: String = {
    s"Constraint: $id\n$pos\nEV:$arg1\nEEV:$arg2"
  }
}

class ConstraintGen(proc: Procedure,  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], loops: Set[Loop]) extends Analysis[Set[Constraint]] {
  
  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, Set(proc)).toSet
  val sva = SVA(proc, constProp, loops)

  override def analyze(): Set[Constraint] = {
    var constraints: Set[Constraint] = Set.empty
    domain.foreach {
      case load @ MemoryLoad(lhs, _, index, _, size, _) =>
        constraints += Constraint(load, lhs, index, EV(sva.exprToSymValSet(load, lhs)), EEV(sva.exprToSymValSet(load, index)), size/8)
      case store @ MemoryStore(_, index, value, _, size, _) =>
        constraints += Constraint(store, value, index, EV(sva.exprToSymValSet(store, value)), EEV(sva.exprToSymValSet(store, index)), size/8)
      case _ =>
    }

    constraints
  }
}

