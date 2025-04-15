package analysis
import ir.transforms.*
import ir.*

import analysis.data_structure_analysis.Interval
import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver, UnionFindSolver}
import ir.{Expr, Procedure, Program}
import specification.{ExternalFunction, SymbolTableEntry}
import util.{DSALogger, IRContext}
import java.io.File

trait AbsValue {

  def top(): AbsValue

  def fromliteral(l: Literal): AbsValue = top()
  def frominterval(lIncl: BigInt, hIncl: BigInt): AbsValue = top()

  def intersect(other: AbsValue): AbsValue
  def union(other: AbsValue): AbsValue
  def bvnot(): AbsValue = top()
  def bvneg(): AbsValue = top()
  def intneg(): AbsValue = top()
  def boolnot(): AbsValue = top()
  def booltobv1(): AbsValue = top()

  def equal(other: AbsValue): AbsValue = top()
  def bvcomp(other: AbsValue): AbsValue = top()
  def bvand(other: AbsValue): AbsValue = top()
  def bvor(other: AbsValue): AbsValue = top()
  def bvadd(other: AbsValue): AbsValue = top()
  def bvmul(other: AbsValue): AbsValue = top()
  def bvshl(other: AbsValue): AbsValue = top()
  def bvlshr(other: AbsValue): AbsValue = top()
  def bvashr(other: AbsValue): AbsValue = top()
  def bvshr(other: AbsValue): AbsValue = top()
  def bvult(other: AbsValue): AbsValue = top()
  def bvxor(other: AbsValue): AbsValue = top()
  def bvsub(other: AbsValue): AbsValue = top()
  def bvurem(other: AbsValue): AbsValue = top()
  def bvsrem(other: AbsValue): AbsValue = top()
  def bvsmod(other: AbsValue): AbsValue = top()
  def bvudiv(other: AbsValue): AbsValue = top()
  def bvsdiv(other: AbsValue): AbsValue = top()
  def bvule(other: AbsValue): AbsValue = top()
  def bvugt(other: AbsValue): AbsValue = top()
  def bvslt(other: AbsValue): AbsValue = top()
  def bvsle(other: AbsValue): AbsValue = top()
  def bvsgt(other: AbsValue): AbsValue = top()
  def bvsge(other: AbsValue): AbsValue = top()
  def bvuge(other: AbsValue): AbsValue = top()
  def bvconcat(other: AbsValue): AbsValue = top()

  def intlt(other: AbsValue): AbsValue = top()
  def intle(other: AbsValue): AbsValue = top()
  def intgt(other: AbsValue): AbsValue = top()
  def intge(other: AbsValue): AbsValue = top()
  def intadd(other: AbsValue): AbsValue = top()
  def intsub(other: AbsValue): AbsValue = top()
  def intmul(other: AbsValue): AbsValue = top()
  def intdiv(other: AbsValue): AbsValue = top()
  def intmod(other: AbsValue): AbsValue = top()

  def booland(other: AbsValue): AbsValue = top()
  def boolor(other: AbsValue): AbsValue = top()

  def bvextract(hi: Int, lo: Int): AbsValue = top()
  def zeroextend(bits: Int): AbsValue = top()
  def signextend(bits: Int): AbsValue = top()
  def repeat(bits: Int): AbsValue = top()
}

//class IntervalValue extends ValueDom[Interval] {
//  import analysis.data_structure_analysis.Interval.*
//
//  override def top() = Top
//  extension (t: Interval)
//    def intersect(other: Interval) : Interval = t.intersect(other)
//    def union(other: Interval) : Interval = t.union(other)
//
//    def bvnot(): Interval = top()
//    def bvneg(): Interval = top()
//    def intneg(): Interval = top()
//    def boolnot(): Interval = top()
//    def booltobv1() : Interval = top()
//
//    def equal(other: Interval): Interval = top()
//    def bvcomp(other: Interval): Interval = top()
//    def bvand(other: Interval): Interval = top()
//    def bvor(other: Interval): Interval = top()
//    def bvadd(other: Interval): Interval = top()
//    def bvmul(other: Interval): Interval = top()
//    def bvshl(other: Interval): Interval = top()
//    def bvlshr(other: Interval): Interval = top()
//    def bvashr(other: Interval): Interval = top()
//    def bvshr(other: Interval): Interval = top()
//    def bvult(other: Interval): Interval = top()
//    def bvxor(other: Interval): Interval = top()
//    def bvsub(other: Interval): Interval = top()
//    def bvurem(other: Interval): Interval = top()
//    def bvsrem(other: Interval): Interval = top()
//    def bvsmod(other: Interval): Interval = top()
//    def bvudiv(other: Interval): Interval = top()
//    def bvsdiv(other: Interval): Interval = top()
//    def bvule(other: Interval): Interval = top()
//    def bvugt(other: Interval): Interval = top()
//    def bvslt(other: Interval): Interval = top()
//    def bvsle(other: Interval): Interval = top()
//    def bvsgt(other: Interval): Interval = top()
//    def bvsge(other: Interval): Interval = top()
//    def bvuge(other: Interval): Interval = top()
//    def bvconcat(other: Interval): Interval = top()
//
//    def intlt(other: Interval): Interval = top()
//    def intle(other: Interval): Interval = top()
//    def intgt(other: Interval): Interval = top()
//    def intge(other: Interval): Interval = top()
//    def intadd(other: Interval): Interval = top()
//    def intsub(other: Interval): Interval = top()
//    def intmul(other: Interval): Interval = top()
//    def intdiv(other: Interval): Interval = top()
//    def intmod(other: Interval): Interval = top()
//
//    def booland(other: Interval): Interval = top()
//    def boolor(other: Interval): Interval = top()
//
//    def bvextract(hi: Int, lo: Int) : Interval = top()
//    def zeroextend(bits: Int) : Interval = top()
//    def signextend(bits: Int) : Interval = top()
//    def repeat(bits: Int) : Interval = top()
//
//}

class IntraStateDomain(d: AbsValue) extends AbstractDomain[Map[Variable, AbsValue]] {
  import d.*

  override def bot = Map[Variable, AbsValue]()
  override def top = Map[Variable, AbsValue]()

  override def join(l: Map[Variable, AbsValue], r: Map[Variable, AbsValue], loc: Block) = {
    (l.keys ++ r.keys)
      .map(v => v -> (l.get(v), r.get(v)))
      .map {
        case (v, (Some(l), Some(r))) => v -> l.intersect(r)
        case (v, (None, Some(r))) => v -> r
        case (v, (Some(r), None)) => v -> r
        case (v, (None, None)) => ???
      }
      .toMap
  }

  def evalUnaryExpr(op: UnOp, arg: AbsValue): AbsValue = {
    op match {
      case BoolNOT => arg.boolnot()
      case BVNEG => arg.bvneg()
      case BVNOT => arg.bvnot()
      case IntNEG => arg.intneg()
      case BoolToBV1 => arg.booltobv1()
    }
  }

  def evalBinExpr(op: BinOp, l: AbsValue, r: AbsValue): AbsValue = {
    op match {
      case BVADD => l.bvadd(r)
      case BVSUB => l.bvsub(r)
      case BVMUL => l.bvmul(r)
      case BVUDIV => l.bvudiv(r)
      case BVSDIV => l.bvsdiv(r)
      case BVSREM => l.bvsrem(r)
      case BVUREM => l.bvurem(r)
      case BVSMOD => l.bvsmod(r)
      case BVAND => l.bvand(r)
      case BVOR => l.bvor(r)
      case BVXOR => l.bvxor(r)
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
      case BVEQ => l.equal(r)
      case BVNEQ => l.equal(r).boolnot()
      case BoolEQ => l.equal(r)
      case BoolEQUIV => l.equal(r)
      case BoolNEQ => l.equal(r).boolnot()
      case BoolAND => l.booland(r)
      case BoolOR => l.boolor(r)
      case BoolIMPLIES => l.boolor(r.boolnot())
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

  def evalExpr(s: Map[Variable, AbsValue], e: Expr): AbsValue = {
    e match {
      case BinaryExpr(op, l, r) => evalBinExpr(op, evalExpr(s, l), evalExpr(s, r))
      case UnaryExpr(op, arg) => evalUnaryExpr(op, evalExpr(s, arg))
      case u: UninterpretedFunction => d.top()
      case l: Literal => d.fromliteral(l)
      case Extract(hi, lo, e) => evalExpr(s, e).bvextract(hi, lo)
      case v: Variable => s.get(v).getOrElse(d.top())
      case ZeroExtend(sz, e) => evalExpr(s, e).zeroextend(sz)
      case SignExtend(sz, e) => evalExpr(s, e).signextend(sz)
      case Repeat(repeats, e) => evalExpr(s, e).repeat(repeats)
      case _: Memory => d.top()
      case l: LambdaExpr => d.top()
      case l: QuantifierExpr => d.top()
      case l: OldExpr => d.top()
    }
  }

  def narrow(st: Map[Variable, AbsValue], e: Expr) = e match {
    case (BinaryExpr(BVEQ, v: Variable, v2: Literal)) => {
      st.updated(v, d.fromliteral(v2))
    }
    case (BinaryExpr(BVULE, v: Variable, v2: BitVecLiteral)) => {
      val nv = d.frominterval(0, v2.value)
      val ov = st.get(v).getOrElse(d.top())
      st.updated(v, ov.intersect(nv))
    }
    case (BinaryExpr(BVUGE, v: Variable, v2: BitVecLiteral)) => {
      val nv = d.frominterval(v2.value, BigInt(2).pow(v2.getType.size) - 1)
      val ov = st.get(v).getOrElse(d.top())
      st.updated(v, ov.intersect(nv))
    }
    case (BinaryExpr(BVULT, v: Variable, v2: BitVecLiteral)) => {
      val nv = d.frominterval(0, v2.value - 1)
      val ov = st.get(v).getOrElse(d.top())
      st.updated(v, ov.intersect(nv))
    }
    case (BinaryExpr(BVUGT, v: Variable, v2: BitVecLiteral)) => {
      val nv = d.frominterval(v2.value + 1, BigInt(2).pow(v2.getType.size) - 1)
      val ov = st.get(v).getOrElse(d.top())
      st.updated(v, ov.intersect(nv))
    }
    case _ => st
  }

  def transfer(st: Map[Variable, AbsValue], c: Command) = {
    c match {
      case LocalAssign(lhs, rhs, _) => st.updated(lhs, evalExpr(st, rhs))
      case c: DirectCall => {
        st ++ (c.outParams.map { case (formal, actual) =>
          actual -> d.top()
        })
      }
      case a: MemoryStore => st
      case a: MemoryLoad => st.updated(a.lhs, d.top())
      case a: Assume => narrow(st, a.body)
      case a: Assert => st
      case n: NOP => st
      case i: IndirectCall => Map()
      case j: SimulAssign => {
        st ++ j.assignments.toSeq.map { case (l, r) =>
          l -> evalExpr(st, r)
        }
      }
      case r: Return => st
      case r: GoTo => st
      case r: Unreachable => st
    }
  }

}
