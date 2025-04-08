package analysis
import ir.transforms.*
import ir.*

import analysis.data_structure_analysis.Interval
import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver, UnionFindSolver}
import ir.{Expr, Procedure, Program}
import specification.{ExternalFunction, SymbolTableEntry}
import util.{DSALogger, IRContext}
import java.io.File

sealed trait Value {

  def top() : Value

  def bvnot(): Value = top()
  def bvneg(): Value = top()
  def boolnot(): Value = top()

  def equal(other: Value): Value = top()
  def bvcomp(other: Value): Value = top()
  def bvand(other: Value): Value = top()
  def bvor(other: Value): Value = top()
  def bvadd(other: Value): Value = top()
  def bvmul(other: Value): Value = top()
  def bvshl(other: Value): Value = top()
  def bvlshr(other: Value): Value = top()
  def bvashr(other: Value): Value = top()
  def bvshr(other: Value): Value = top()
  def bvult(other: Value): Value = top()
  def bvxor(other: Value): Value = top()
  def bvsub(other: Value): Value = top()
  def bvurem(other: Value): Value = top()
  def bvsrem(other: Value): Value = top()
  def bvsmod(other: Value): Value = top()
  def bvudiv(other: Value): Value = top()
  def bvsdiv(other: Value): Value = top()
  def bvule(other: Value): Value = top()
  def bvugt(other: Value): Value = top()
  def bvslt(other: Value): Value = top()
  def bvsle(other: Value): Value = top()
  def bvsgt(other: Value): Value = top()
  def bvsge(other: Value): Value = top()
  def bvuge(other: Value): Value = top()
  def bvconcat(other: Value): Value = top()

  def intlt(other: Value): Value = top()
  def intle(other: Value): Value = top()
  def intgt(other: Value): Value = top()
  def intge(other: Value): Value = top()
  def intadd(other: Value): Value = top()
  def intsub(other: Value): Value = top()
  def intmul(other: Value): Value = top()
  def intdiv(other: Value): Value = top()
  def intmod(other: Value): Value = top()

  def booland(other: Value): Value = top()
  def boolor(other: Value): Value = top()


}

class IntraStateDomain(d: Value) extends AbstractDomain[Map[Variable, Value]] {

  override def join(l: Map[Variable, Value], r: Map[Variable, Value], loc: Command) = {

  }

  def evalBinExpr(op: BinOp, l: Value, r: Value) : Value = {
    op match {
      case BVADD => l.bvadd(r)
      case BVSUB => l.bvsub(r)
      case BVMUL =>  l.bvmul(r)
      case BVUDIV => l.bvudiv(r)
      case BVSDIV => l.bvsdiv(r)
      case BVSREM => l.bvsrem(r)
      case BVUREM => l.bvurem(r)
      case BVSMOD => l.bvsmod(r)
      case BVAND =>  l.bvand(r)
      case BVOR => l.bvor(r)
      case BVXOR =>  l.bvxor(r)
      case BVNAND => l.bvand(r).bvnot()
      case BVNOR => l.bvor(r).bvnot()
      case BVXNOR => l.bvxor(r).bvnot()
      case BVSHL => l.bvshl(r)
      case BVLSHR => l.bvlshr(r)
      case BVASHR => l.bvashr(r)
      case BVCOMP => l.bvcomp(r)
      case BVCONCAT => l.bvconcat(r)
      case BVULE => l.bvule(r)
      case BVUGT => l.bvugt(r)
      case BVUGE => l.bvuge(r)
      case BVULT => l.bvult(r)
      case BVSLT => l.bvslt(r)
      case BVSLE => l.bvsle(r)
      case BVSGT => l.bvsgt(r)
      case BVSGE => l.bvsge(r)
      case BVEQ  => l.equal(r)
      case BVNEQ => l.equal(r).boolnot()
      case BoolEQ => l.equal(r)
      case BoolEQUIV => l.equal(r)
      case BoolNEQ => l.equal(r).boolnot()
      case BoolAND => l.booland(r)
      case BoolOR => l.boolor(r)
      case BoolIMPLIES  => l.boolor(r.boolnot())
      case IntEQ => l.equal(r) 
      case IntNEQ => l.equal(r).boolnot()
      case IntLT => l.intlt(r) 
      case IntLE => l.intle(r)
      case IntGT => l.intgt(r)
      case IntGE => l.intge(r)
      case IntADD => l.intadd(r)
      case IntSUB => l.intsub(r)
      case IntMUL => l.intmul(r)
      case IntDIV => l.intdiv(r)
      case IntMOD => l.intmod(r)
    }
  }

  def evalExpr(s: Map[Variable, Value], e: Expr) : Value = {
    e match {
      case BinaryExpr(op, l, r) => evalBinExpr(op, evalExpr(s, l), evalExpr(s, r))
    }
  }

  def transfer(l: Map[Variable, Value], c: Command) = {
    c match {
      case LocalAssign(lhs, rhs, _) => l.updated(lhs, evalExpr(l, rhs))
      case c: DirectCall => {
        var s = l
        c.outParams.foreach {
          case (formal, actual) => s = s.updated(actual, d.top())
        }
        s
      }
      case a : Assume => l
      case a : Assert => l
      case n: NOP => l
      case i: IndirectCall => Map()
    }
  }


}
