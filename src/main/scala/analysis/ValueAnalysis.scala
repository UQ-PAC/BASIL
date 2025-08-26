package analysis
import ir.*

trait ValueLattice[ValueType <: ValueLattice[ValueType]] extends InternalLattice[ValueType] {

  /** Returns a type-independent top value. */
  def top: ValueType

  /** Returns a type-independent bottom value. */
  def bottom: ValueType

  /** Returns a type-*dependent* top value. */
  def top(ty: IRType): ValueType

  /** Returns a type-*dependent* bottom value. */
  def bottom(ty: IRType): ValueType

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

/**
 * A lattice which has type-specific values. Each lattice element is expected
 * to have a known [[ir.IRType]], defined by its [[TypedValueLattice#getType]] method.
 *
 * This trait is mainly intended to be used use within a [[TypedLattice]].
 */
trait TypedValueLattice[T <: ValueLattice[T]] extends ValueLattice[T] {

  /**
   * Returns the [[ir.IRType]] of the current lattice element.
   */
  def getType: IRType

  /**
   * Called by [[TypedLattice]] to handle two lattice elements of incompatible types.
   * The default implementation will throw an exception.
   */
  def handleConflictingTypes[T <: TypedValueLattice[T]](x: T, y: T): TypedLattice[T] = throw new Exception(
    s"attempted lattice operation on incompatible ${getClass.getName} values: '$x' and '$y'"
  )
  // NOTE: this makes the TypedLattice and TypedValueLattice classes /mutually referential/.
  // this is not the best. maybe typeclasses could avoid this.

}

/**
 * Derives a lattice for all [[ir.IRType]]s from the given type-specific [[TypedValueLattice]].
 * This is done by wrapping the "inner" lattice into [[TypedLattice.Elem]]. New
 * [[TypedLattice.Top]] and [[TypedLattice.Bot]] values are also introduced to act as
 * universal (type-independent) top and bottom elements.
 *
 * If a type-specific operation is attempted on inner lattice values of incompatible type,
 * the [[TypedValueLattice#handleConflictingTypes]] function is called.
 *
 * This [[TypedLattice]] class guarantees that the type-dependent methods of the inner
 * [[TypedValueLattice]] are only called with values of compatible type. It also guarantees that
 * the inner lattice's parameter-less [[ValueLattice#top]] and [[ValueLattice#bottom]]
 * methods are _never_ called&mdash;instead, these are implemented by returning [[TypedLattice.Top]]
 * and [[TypedLattice.Bot]], respectively.
 */
enum TypedLattice[T <: TypedValueLattice[T]](lattice: T) extends ValueLattice[TypedLattice[T]] {
  case Bot(lattice: T) extends TypedLattice[T](lattice)
  case Elem(inner: T) extends TypedLattice[T](inner)
  case Top(lattice: T) extends TypedLattice[T](lattice)

  inline def checkBinaryTopBot(x: TypedLattice[T], y: TypedLattice[T])(
    f: (Elem[T], Elem[T]) => TypedLattice[T]
  ): TypedLattice[T] =
    (x, y) match {
      case (Bot(_), _) | (_, Bot(_)) => Bot(lattice)
      case (Top(_), _) | (_, Top(_)) => Top(lattice)
      case (x: Elem[T], y: Elem[T]) => f(x, y)
    }

  inline def checkBinaryWidths(x: Elem[T], y: Elem[T])(f: (T, T) => T): TypedLattice[T] =
    (x, y) match {
      case (Elem(x), Elem(y)) if x.getType != y.getType => lattice.handleConflictingTypes(x, y)
      case (Elem(x), Elem(y)) => Elem(f(x, y))
    }

  def checkBinary(x: TypedLattice[T], y: TypedLattice[T])(f: (T, T) => T): TypedLattice[T] =
    checkBinaryTopBot(x, y)(checkBinaryWidths(_, _)(f))

  def checkUnary(x: TypedLattice[T])(f: T => T): TypedLattice[T] = {
    x match {
      case Bot(_) => Bot(lattice)
      case Top(_) => Top(lattice)
      case Elem(x) => Elem(f(x))
    }
  }

  def top: TypedLattice[T] = Top(lattice)
  def bottom: TypedLattice[T] = Bot(lattice)

  def top(ty: IRType): TypedLattice[T] = Elem(lattice.top(ty))
  def bottom(ty: IRType): TypedLattice[T] = Elem(lattice.bottom(ty))

  def join(x: TypedLattice[T]) = checkBinary(this, x)(_ `join` _)
  def meet(x: TypedLattice[T]) = checkBinary(this, x)(_ `meet` _)

  override def lub(x: TypedLattice[T]) = join(x)
  override def glb(x: TypedLattice[T]) = meet(x)

  def bvnot() = checkUnary(this)(_.bvnot())
  def bvneg() = checkUnary(this)(_.bvneg())
  def boolnot() = checkUnary(this)(_.boolnot())
  def intneg() = checkUnary(this)(_.intneg())
  def booltobv1() = checkUnary(this)(_.booltobv1())

  def equal(other: TypedLattice[T]) = checkBinary(this, other)(_ `equal` _)
  def bvcomp(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvcomp` _)
  def bvand(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvand` _)
  def bvor(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvor` _)
  def bvadd(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvadd` _)
  def bvmul(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvmul` _)
  def bvshl(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvshl` _)
  def bvlshr(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvlshr` _)
  def bvashr(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvashr` _)
  def bvult(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvult` _)
  def bvxor(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvxor` _)
  def bvsub(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvsub` _)
  def bvurem(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvurem` _)
  def bvsrem(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvsrem` _)
  def bvsmod(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvsmod` _)
  def bvudiv(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvudiv` _)
  def bvsdiv(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvsdiv` _)
  def bvule(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvule` _)
  def bvugt(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvugt` _)
  def bvslt(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvslt` _)
  def bvsle(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvsle` _)
  def bvsgt(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvsgt` _)
  def bvsge(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvsge` _)
  def bvuge(other: TypedLattice[T]) = checkBinary(this, other)(_ `bvuge` _)
  def bvconcat(other: TypedLattice[T]) = checkBinaryTopBot(this, other)(_ `bvconcat` _)

  def intlt(other: TypedLattice[T]) = checkBinary(this, other)(_ `intlt` _)
  def intle(other: TypedLattice[T]) = checkBinary(this, other)(_ `intle` _)
  def intgt(other: TypedLattice[T]) = checkBinary(this, other)(_ `intgt` _)
  def intge(other: TypedLattice[T]) = checkBinary(this, other)(_ `intge` _)
  def intadd(other: TypedLattice[T]) = checkBinary(this, other)(_ `intadd` _)
  def intsub(other: TypedLattice[T]) = checkBinary(this, other)(_ `intsub` _)
  def intmul(other: TypedLattice[T]) = checkBinary(this, other)(_ `intmul` _)
  def intdiv(other: TypedLattice[T]) = checkBinary(this, other)(_ `intdiv` _)
  def intmod(other: TypedLattice[T]) = checkBinary(this, other)(_ `intmod` _)

  def booland(other: TypedLattice[T]) = checkBinary(this, other)(_ `booland` _)
  def boolor(other: TypedLattice[T]) = checkBinary(this, other)(_ `boolor` _)

  def constant(v: Literal) = Elem(lattice.constant(v))
  def zero_extend(extend: Int) = checkUnary(this)(_.zero_extend(extend))
  def sign_extend(extend: Int) = checkUnary(this)(_.zero_extend(extend))
  def repeat(repeats: Int) = checkUnary(this)(_.repeat(repeats))
  def extract(hi: Int, lo: Int) = checkUnary(this)(_.extract(hi, lo))

}

class NOPValueAnalysis[ValueType <: NOPValueAnalysis[ValueType]] extends ValueLattice[ValueType] {
  def top: ValueType = ???
  def bottom: ValueType = ???
  def top(ty: IRType): ValueType = ???
  def bottom(ty: IRType): ValueType = ???
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

class EvaluateInLattice[Value <: ValueLattice[Value]](lattice: Value) {

  def evalExpr(evalVar: Variable => Option[Value])(e: Expr): Value = {
    e match {
      case e: Variable => evalVar(e).getOrElse(lattice.top(e.getType))
      case BinaryExpr(op, l, r) => evalBinExpr(evalVar)(op, evalExpr(evalVar)(l), evalExpr(evalVar)(r))
      case l: Literal => lattice.constant(l)
      case ZeroExtend(extend, e) => evalExpr(evalVar)(e).zero_extend(extend)
      case SignExtend(extend, e) => evalExpr(evalVar)(e).sign_extend(extend)
      case Extract(hi, lo, e) => evalExpr(evalVar)(e).extract(hi, lo)
      case Repeat(reps, e) => evalExpr(evalVar)(e).repeat(reps)
      case UnaryExpr(op, e) => evalUnaryExpr(op, evalExpr(evalVar)(e))
      case _: FApplyExpr => lattice.top(e.getType)
      case _: Memory => lattice.top(e.getType)
      case _: LambdaExpr => lattice.top(e.getType)
      case _: QuantifierExpr => lattice.top(e.getType)
      case _: OldExpr => lattice.top(e.getType)
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
  lattice1: Value1,
  lattice2: Value2
) extends AbsEvalExpr[ProductInternalLattice[Value1, Value2]] {

  override val top = ProductInternalLattice(lattice1, lattice2)
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

  val eval1 = EvaluateInLattice[Value1](lattice1)
  val eval2 = EvaluateInLattice[Value2](lattice2)

  def evalExpr(
    read: Variable => Option[ProductInternalLattice[Value1, Value2]]
  )(e: Expr): ProductInternalLattice[Value1, Value2] = {
    val v1 = eval1.evalExpr(v => read(v).map(_._1))(e)
    val v2 = eval2.evalExpr(v => read(v).map(_._2))(e)
    refine((v1, v2))
  }
}

class DefaultValueLattice[Value <: ValueLattice[Value]](lattice: Value, bottomValue: Option[Value] = None)
    extends AbsEvalExpr[Value]
    with Lattice[Value] {
  override val top: Value = lattice.top
  val eval = EvaluateInLattice[Value](lattice)

  override def bottom: Value = bottomValue.getOrElse(throw Exception("bottom not defined for this lattice"))
  override def lub(x: Value, y: Value) = x.join(y)
  def evalExpr(read: Variable => Option[Value])(e: Expr): Value = eval.evalExpr(read)(e)
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
  lattice: L,
  transferFn: TransferFun[LatticeMap[Variable, L]],
  innerLattice: AbsEvalExpr[L]
) extends MapDomain[Variable, L] {
  //  val innerLattice : DefaultValueLattice[L] = DefaultValueLattice[L](topValue)

  given v: L = lattice

  def botTerm: L = lattice.bottom
  def joinTerm(a: L, b: L, pos: ir.Block): L = a.join(b)
  def topTerm: L = lattice.top

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
