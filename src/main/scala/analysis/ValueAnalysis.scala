package analysis
import ir.*

trait ValueLattice[ValueType <: ValueLattice[ValueType]] extends InternalLattice[ValueType] {

  def top: ValueType
  def bottom: ValueType

  def join(x: ValueType): ValueType
  def meet(x: ValueType): ValueType

  def lub(x: ValueType): ValueType = join(x)
  def glb(x: ValueType): ValueType = ???

  def bvnot(): ValueType
  def bvneg(): ValueType
  def boolnot(): ValueType
  def intneg(): ValueType
  def booltobv1(): ValueType

  def equal(other: ValueType): ValueType
  def bvcomp(other: ValueType): ValueType
  def bvand(other: ValueType): ValueType
  def bvor(other: ValueType): ValueType
  def bvadd(other: ValueType): ValueType
  def bvmul(other: ValueType): ValueType
  def bvshl(other: ValueType): ValueType
  def bvlshr(other: ValueType): ValueType
  def bvashr(other: ValueType): ValueType
  def bvult(other: ValueType): ValueType
  def bvxor(other: ValueType): ValueType
  def bvsub(other: ValueType): ValueType
  def bvurem(other: ValueType): ValueType
  def bvsrem(other: ValueType): ValueType
  def bvsmod(other: ValueType): ValueType
  def bvudiv(other: ValueType): ValueType
  def bvsdiv(other: ValueType): ValueType
  def bvule(other: ValueType): ValueType
  def bvugt(other: ValueType): ValueType
  def bvslt(other: ValueType): ValueType
  def bvsle(other: ValueType): ValueType
  def bvsgt(other: ValueType): ValueType
  def bvsge(other: ValueType): ValueType
  def bvuge(other: ValueType): ValueType
  def bvconcat(other: ValueType): ValueType

  def intlt(other: ValueType): ValueType
  def intle(other: ValueType): ValueType
  def intgt(other: ValueType): ValueType
  def intge(other: ValueType): ValueType
  def intadd(other: ValueType): ValueType
  def intsub(other: ValueType): ValueType
  def intmul(other: ValueType): ValueType
  def intdiv(other: ValueType): ValueType
  def intmod(other: ValueType): ValueType

  def booland(other: ValueType): ValueType
  def boolor(other: ValueType): ValueType

  def constant(v: Literal): ValueType
  def zero_extend(extend: Int): ValueType
  def sign_extend(extend: Int): ValueType
  def repeat(repeats: Int): ValueType
  def extract(hi: Int, lo: Int): ValueType

}

class NOPValueAnalysis[ValueType <: NOPValueAnalysis[ValueType]] extends ValueLattice[ValueType] {

  def top: ValueType = ???
  def bottom: ValueType = ???
  def join(x: ValueType): ValueType = ???
  def meet(x: ValueType): ValueType = ???

  def bvnot(): ValueType = top
  def bvneg(): ValueType = top
  def boolnot(): ValueType = top
  def intneg(): ValueType = top
  def booltobv1(): ValueType = top

  def equal(other: ValueType): ValueType = top
  def bvcomp(other: ValueType): ValueType = top
  def bvand(other: ValueType): ValueType = top
  def bvor(other: ValueType): ValueType = top
  def bvadd(other: ValueType): ValueType = top
  def bvmul(other: ValueType): ValueType = top
  def bvshl(other: ValueType): ValueType = top
  def bvlshr(other: ValueType): ValueType = top
  def bvashr(other: ValueType): ValueType = top
  def bvshr(other: ValueType): ValueType = top
  def bvult(other: ValueType): ValueType = top
  def bvxor(other: ValueType): ValueType = top
  def bvsub(other: ValueType): ValueType = top
  def bvurem(other: ValueType): ValueType = top
  def bvsrem(other: ValueType): ValueType = top
  def bvsmod(other: ValueType): ValueType = top
  def bvudiv(other: ValueType): ValueType = top
  def bvsdiv(other: ValueType): ValueType = top
  def bvule(other: ValueType): ValueType = top
  def bvugt(other: ValueType): ValueType = top
  def bvslt(other: ValueType): ValueType = top
  def bvsle(other: ValueType): ValueType = top
  def bvsgt(other: ValueType): ValueType = top
  def bvsge(other: ValueType): ValueType = top
  def bvuge(other: ValueType): ValueType = top
  def bvconcat(other: ValueType): ValueType = top

  def intlt(other: ValueType): ValueType = top
  def intle(other: ValueType): ValueType = top
  def intgt(other: ValueType): ValueType = top
  def intge(other: ValueType): ValueType = top
  def intadd(other: ValueType): ValueType = top
  def intsub(other: ValueType): ValueType = top
  def intmul(other: ValueType): ValueType = top
  def intdiv(other: ValueType): ValueType = top
  def intmod(other: ValueType): ValueType = top

  def booland(other: ValueType): ValueType = top
  def boolor(other: ValueType): ValueType = top

  def constant(v: Literal): ValueType = top
  def zero_extend(extend: Int): ValueType = top
  def sign_extend(extend: Int): ValueType = top
  def repeat(repeats: Int): ValueType = top
  def extract(hi: Int, lo: Int): ValueType = top

}

class EvaluateInLattice[Value <: ValueLattice[Value]](top: Value) {

  def evalExpr(evalVar: Variable => Option[Value])(e: Expr): Value = {
    e match {
      case e: Variable => evalVar(e).getOrElse(top)
      case BinaryExpr(op, l, r) => evalBinExpr(evalVar)(op, evalExpr(evalVar)(l), evalExpr(evalVar)(r))
      case l: Literal => top.constant(l)
      case ZeroExtend(extend, e) => evalExpr(evalVar)(e).zero_extend(extend)
      case SignExtend(extend, e) => evalExpr(evalVar)(e).sign_extend(extend)
      case Extract(hi, lo, e) => evalExpr(evalVar)(e).extract(hi, lo)
      case Repeat(reps, e) => evalExpr(evalVar)(e).repeat(reps)
      case UnaryExpr(op, e) => evalUnaryExpr(op, evalExpr(evalVar)(e))
      case _: FApplyExpr => top
      case _: Memory => top
      case _: LambdaExpr => top
      case _: QuantifierExpr => top
      case _: OldExpr => top
      case AssocExpr(BoolAND, exprs) => exprs.map(evalExpr(evalVar)).reduce((a, b) => a.booland(b))
      case AssocExpr(BoolOR, exprs) => exprs.map(evalExpr(evalVar)).reduce((a, b) => a.boolor(b))
      case AssocExpr(BoolIMPLIES, exprs) => exprs.map(evalExpr(evalVar)).reduce((l, r) => l.boolor(r.boolnot()))
    }
  }

  def evalUnaryExpr(op: UnOp, v: Value): Value = {
    op match {
      case BVNOT => v.bvnot()
      case BVNEG => v.bvneg()
      case BoolNOT => v.boolnot()
      case BoolToBV1 => v.booltobv1()
      case IntNEG => v.intneg()
    }
  }

  def evalBinExpr(evalVar: Variable => Option[Value])(op: BinOp, l: Value, r: Value): Value = {
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
      case BoolAND => l.booland(r)
      case BoolOR => l.boolor(r)
      case BoolIMPLIES => l.boolor(r.boolnot())
      case EQ => l.equal(r)
      case NEQ => l.equal(r).boolnot()
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
}

trait AbsEvalExpr[AbsValue] extends Lattice[AbsValue] {
  def top: AbsValue
  def evalExpr(read: Variable => Option[AbsValue])(e: Expr): AbsValue
}

case class ProductInternalLattice[V1 <: InternalLattice[V1], V2 <: InternalLattice[V2]](a: V1, b: V2)
    extends InternalLattice[ProductInternalLattice[V1, V2]] {
  def join(other: ProductInternalLattice[V1, V2]) = {
    ProductInternalLattice(a.join(other.a), b.join(other.b))
  }
  def meet(other: ProductInternalLattice[V1, V2]): ProductInternalLattice[V1, V2] = {
    ProductInternalLattice(a.meet(other.a), b.meet(other.b))
  }

  def top = (ProductInternalLattice(a.top, b.top))
  def bottom = (ProductInternalLattice(a.bottom, b.bottom))
}

class ProductValueLattice[Value1 <: ValueLattice[Value1], Value2 <: ValueLattice[Value2]](
  topValue1: Value1,
  topValue2: Value2
) extends AbsEvalExpr[ProductInternalLattice[Value1, Value2]] {

  override val top = ProductInternalLattice(topValue1, topValue2)
  override val bottom: ProductInternalLattice[Value1, Value2] = ???
  override def lub(x: ProductInternalLattice[Value1, Value2], y: ProductInternalLattice[Value1, Value2]) = x.join(y)

  def refine(v: (Value1, Value2)): ProductInternalLattice[Value1, Value2] = {
    val (v1, v2) = v

    // TODO:
    // cast v1 to v2' ; v2.meet(v2')
    // case v2 to v1' ; v1.meet(v1')
    // loop to fixed point etc

    ???
  }

  val eval1 = EvaluateInLattice[Value1](topValue1)
  val eval2 = EvaluateInLattice[Value2](topValue2)

  def evalExpr(
    read: Variable => Option[ProductInternalLattice[Value1, Value2]]
  )(e: Expr): ProductInternalLattice[Value1, Value2] = {
    val v1 = eval1.evalExpr(v => read(v).map(_._1))(e)
    val v2 = eval2.evalExpr(v => read(v).map(_._2))(e)
    refine((v1, v2))
  }
}

class DefaultValueLattice[Value <: ValueLattice[Value]](topValue: Value, bottomValue: Option[Value] = None)
    extends AbsEvalExpr[Value]
    with Lattice[Value] {
  override val top: Value = topValue
  val v = EvaluateInLattice[Value](topValue)

  override val bottom: Value = bottomValue.get
  override def lub(x: Value, y: Value) = x.join(y)
  def evalExpr(read: Variable => Option[Value])(e: Expr): Value = v.evalExpr(read)(e)
}

trait TransferFun[L] {
  def assign(v: L, e: Seq[(Variable, Expr)]): L
  def indirect_call(v: L, e: Variable): L
  def direct_call(v: L, d: DirectCall): L
  def memory_store(v: L, m: MemoryStore): L
  def memory_load(v: L, m: MemoryLoad): L
  def memory_assign(v: L, m: MemoryAssign): L
  def jump(v: L, m: Jump): L
}

class DefaultTransfer[L <: InternalLattice[L]](innerLattice: AbsEvalExpr[L])
    extends TransferFun[LatticeMap[Variable, L]] {

  given v: L = innerLattice.top

  def assign(v: LatticeMap[Variable, L], assignments: Seq[(Variable, Expr)]): LatticeMap[Variable, L] = {
    val get = v.toMap.get
    val n = assignments.map { case (v, e) =>
      v -> innerLattice.evalExpr(get)(e)
    }
    n.foldLeft(v)((v, e) => v.update(e))
  }

  def indirect_call(v: LatticeMap[Variable, L], e: Variable): LatticeMap[Variable, L] = LatticeMap.Top()

  def direct_call(v: LatticeMap[Variable, L], d: DirectCall): LatticeMap[Variable, L] =
    d.outParams.map(_._2 -> innerLattice.top).foldLeft(v)((v, e) => v.update(e))

  def memory_store(v: LatticeMap[Variable, L], m: MemoryStore): LatticeMap[Variable, L] = v

  def memory_load(v: LatticeMap[Variable, L], m: MemoryLoad): LatticeMap[Variable, L] =
    v.update(m.lhs, innerLattice.top)

  def memory_assign(v: LatticeMap[Variable, L], m: MemoryAssign): LatticeMap[Variable, L] = v

  def jump(v: LatticeMap[Variable, L], m: Jump): LatticeMap[Variable, L] = v
}

class ValueStateDomain[L <: InternalLattice[L]](
  topValue: L,
  transferFn: TransferFun[LatticeMap[Variable, L]],
  innerLattice: AbsEvalExpr[L]
) extends MapDomain[Variable, L] {
  //  val innerLattice : DefaultValueLattice[L] = DefaultValueLattice[L](topValue)

  given v: L = topValue

  def botTerm: L = topValue.bottom
  def joinTerm(a: L, b: L, pos: ir.Block): L = a.join(b)
  def topTerm: L = topValue

  override def transfer(v: LatticeMap[Variable, L], b: Command): LatticeMap[Variable, L] = {
    val get = v.toMap.get
    b match {
      case SimulAssign(assignments, _) => transferFn.assign(v, assignments)
      // case a: Assign => a.assignees.map(_ -> innerLattice.top).foldLeft(v)((v, e) => v.update(e))
      case i: IndirectCall => transferFn.indirect_call(v, i.target)
      case i: MemoryLoad => transferFn.memory_load(v, i)
      case i: MemoryStore => transferFn.memory_store(v, i)
      case i: MemoryAssign => transferFn.memory_assign(v, i)
      case i: DirectCall => transferFn.direct_call(v, i)
      case i: Jump => v
      case _: NOP => v
      case a: Assert => v
      case a: Assume => v
    }
  }

}

def valueAnalysis[L <: ValueLattice[L]](topValue: L)(p: Procedure) = {
  val l = DefaultValueLattice(topValue)
  val d = ValueStateDomain(topValue, DefaultTransfer(l), l)
  val solve = ir.transforms.worklistSolver(d)
  solve.solveProc(p, backwards = false)
}

def productValueAnalysis[L1 <: ValueLattice[L1], L2 <: ValueLattice[L2]](topValue1: L1, topValue2: L2)(p: Procedure) = {
  val l = ProductValueLattice[L1, L2](topValue1, topValue2)
  val d = ValueStateDomain[ProductInternalLattice[L1, L2]](
    ProductInternalLattice(topValue1, topValue2),
    DefaultTransfer(l),
    l
  )
  val solve = ir.transforms.worklistSolver(d)
  solve.solveProc(p, backwards = false)
}
