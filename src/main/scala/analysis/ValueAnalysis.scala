package analysis
import ir.*

trait ValueLattice[ValueType] extends Lattice[ValueType] {

  def constant(v: Literal): ValueType

  def bvnot(x: ValueType): ValueType
  def bvneg(x: ValueType): ValueType
  def boolnot(x: ValueType): ValueType
  def intneg(x: ValueType): ValueType
  def booltobv1(x: ValueType): ValueType

  def equal(x: ValueType, y: ValueType): ValueType
  def bvcomp(x: ValueType, y: ValueType): ValueType
  def bvand(x: ValueType, y: ValueType): ValueType
  def bvor(x: ValueType, y: ValueType): ValueType
  def bvadd(x: ValueType, y: ValueType): ValueType
  def bvmul(x: ValueType, y: ValueType): ValueType
  def bvshl(x: ValueType, y: ValueType): ValueType
  def bvlshr(x: ValueType, y: ValueType): ValueType
  def bvashr(x: ValueType, y: ValueType): ValueType
  def bvult(x: ValueType, y: ValueType): ValueType
  def bvxor(x: ValueType, y: ValueType): ValueType
  def bvsub(x: ValueType, y: ValueType): ValueType
  def bvurem(x: ValueType, y: ValueType): ValueType
  def bvsrem(x: ValueType, y: ValueType): ValueType
  def bvsmod(x: ValueType, y: ValueType): ValueType
  def bvudiv(x: ValueType, y: ValueType): ValueType
  def bvsdiv(x: ValueType, y: ValueType): ValueType
  def bvule(x: ValueType, y: ValueType): ValueType
  def bvugt(x: ValueType, y: ValueType): ValueType
  def bvslt(x: ValueType, y: ValueType): ValueType
  def bvsle(x: ValueType, y: ValueType): ValueType
  def bvsgt(x: ValueType, y: ValueType): ValueType
  def bvsge(x: ValueType, y: ValueType): ValueType
  def bvuge(x: ValueType, y: ValueType): ValueType
  def bvconcat(x: ValueType, y: ValueType): ValueType

  def intlt(x: ValueType, y: ValueType): ValueType
  def intle(x: ValueType, y: ValueType): ValueType
  def intgt(x: ValueType, y: ValueType): ValueType
  def intge(x: ValueType, y: ValueType): ValueType
  def intadd(x: ValueType, y: ValueType): ValueType
  def intsub(x: ValueType, y: ValueType): ValueType
  def intmul(x: ValueType, y: ValueType): ValueType
  def intdiv(x: ValueType, y: ValueType): ValueType
  def intmod(x: ValueType, y: ValueType): ValueType

  def booland(x: ValueType, y: ValueType): ValueType
  def boolor(x: ValueType, y: ValueType): ValueType

  def zero_extend(x: ValueType, extend: Int): ValueType
  def sign_extend(x: ValueType, extend: Int): ValueType
  def repeat(x: ValueType, repeats: Int): ValueType
  def extract(x: ValueType, hi: Int, lo: Int): ValueType

}

extension [ValueType](x: ValueType)(using lattice: ValueLattice[ValueType]) {

  def bvnot(): ValueType = lattice.bvnot(x)
  def bvneg(): ValueType = lattice.bvneg(x)
  def boolnot(): ValueType = lattice.boolnot(x)
  def intneg(): ValueType = lattice.intneg(x)
  def booltobv1(): ValueType = lattice.booltobv1(x)

  def equal(y: ValueType): ValueType = lattice.equal(x, y: ValueType)
  def bvcomp(y: ValueType): ValueType = lattice.bvcomp(x, y: ValueType)
  def bvand(y: ValueType): ValueType = lattice.bvand(x, y: ValueType)
  def bvor(y: ValueType): ValueType = lattice.bvor(x, y: ValueType)
  def bvadd(y: ValueType): ValueType = lattice.bvadd(x, y: ValueType)
  def bvmul(y: ValueType): ValueType = lattice.bvmul(x, y: ValueType)
  def bvshl(y: ValueType): ValueType = lattice.bvshl(x, y: ValueType)
  def bvlshr(y: ValueType): ValueType = lattice.bvlshr(x, y: ValueType)
  def bvashr(y: ValueType): ValueType = lattice.bvashr(x, y: ValueType)
  def bvult(y: ValueType): ValueType = lattice.bvult(x, y: ValueType)
  def bvxor(y: ValueType): ValueType = lattice.bvxor(x, y: ValueType)
  def bvsub(y: ValueType): ValueType = lattice.bvsub(x, y: ValueType)
  def bvurem(y: ValueType): ValueType = lattice.bvurem(x, y: ValueType)
  def bvsrem(y: ValueType): ValueType = lattice.bvsrem(x, y: ValueType)
  def bvsmod(y: ValueType): ValueType = lattice.bvsmod(x, y: ValueType)
  def bvudiv(y: ValueType): ValueType = lattice.bvudiv(x, y: ValueType)
  def bvsdiv(y: ValueType): ValueType = lattice.bvsdiv(x, y: ValueType)
  def bvule(y: ValueType): ValueType = lattice.bvule(x, y: ValueType)
  def bvugt(y: ValueType): ValueType = lattice.bvugt(x, y: ValueType)
  def bvslt(y: ValueType): ValueType = lattice.bvslt(x, y: ValueType)
  def bvsle(y: ValueType): ValueType = lattice.bvsle(x, y: ValueType)
  def bvsgt(y: ValueType): ValueType = lattice.bvsgt(x, y: ValueType)
  def bvsge(y: ValueType): ValueType = lattice.bvsge(x, y: ValueType)
  def bvuge(y: ValueType): ValueType = lattice.bvuge(x, y: ValueType)
  def bvconcat(y: ValueType): ValueType = lattice.bvconcat(x, y: ValueType)

  def intlt(y: ValueType): ValueType = lattice.intlt(x, y: ValueType)
  def intle(y: ValueType): ValueType = lattice.intle(x, y: ValueType)
  def intgt(y: ValueType): ValueType = lattice.intgt(x, y: ValueType)
  def intge(y: ValueType): ValueType = lattice.intge(x, y: ValueType)
  def intadd(y: ValueType): ValueType = lattice.intadd(x, y: ValueType)
  def intsub(y: ValueType): ValueType = lattice.intsub(x, y: ValueType)
  def intmul(y: ValueType): ValueType = lattice.intmul(x, y: ValueType)
  def intdiv(y: ValueType): ValueType = lattice.intdiv(x, y: ValueType)
  def intmod(y: ValueType): ValueType = lattice.intmod(x, y: ValueType)

  def booland(y: ValueType): ValueType = lattice.booland(x, y: ValueType)
  def boolor(y: ValueType): ValueType = lattice.boolor(x, y: ValueType)

  def zero_extend(extend: Int): ValueType = lattice.zero_extend(x, extend: Int)
  def sign_extend(extend: Int): ValueType = lattice.sign_extend(x, extend: Int)
  def repeat(repeats: Int): ValueType = lattice.repeat(x, repeats: Int)
  def extract(hi: Int, lo: Int): ValueType = lattice.extract(x, hi: Int, lo: Int)
}

/**
 * A lattice which extends [[ValueLattice]] with an understanding of lattice
 * element *types*. That is, lattice elements each have an intrinsic "type",
 * represented by the value returned by [[TypedValueLattice#getType]].
 *
 * For example, a lattice which is bit-vector dependent might define `TypeType`
 * to be [[ir.IRType]]. Lattice elements with differing types need not be
 * interoperable. That is, it is allowable for the bit-vector transfer functions
 * to throw if given values of incompatible type.
 *
 * The type of a lattice element is also called its "index".
 * This trait is primarly intended for use in conjunction with [[IndexedLattice]],
 * which wraps a [[TypedValueLattice]] into type-specific elements.
 */
trait TypedValueLattice[ValueType, TypeType] extends ValueLattice[ValueType] {

  /**
   * Returns the "type" of the given lattice element.
   */
  def getType(x: ValueType): TypeType

  /** Returns a type-*dependent* top value. */
  def top(ty: TypeType): ValueType

  /** Returns a type-*dependent* bottom value. */
  def bottom(ty: TypeType): ValueType

  def handleConflictingTypes(x: ValueType, y: ValueType): IndexedLattice[ValueType, TypeType] =
    throw new Exception(s"attempted lattice operation on incompatible ${getClass.getName} values: '$x' and '$y'")
}

extension [ValueType, TypeType](x: ValueType)(using lattice: TypedValueLattice[ValueType, TypeType]) {
  def getType: TypeType = lattice.getType(x)
}

/**
 * Derives a lattice for all types from the given type-specific [[TypedValueLattice]].
 * This is done by wrapping the "inner" lattice into [[IndexedLattice.Elem]]. New
 * [[IndexedLattice.Top]] and [[IndexedLattice.Bot]] values are also introduced to act as
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
enum IndexedLattice[T, I] {
  case Bot[T, I]() extends IndexedLattice[T, I]
  case Elem[T, I](inner: T) extends IndexedLattice[T, I]
  case Top[T, I]() extends IndexedLattice[T, I]
}

given [T, I](using lattice: TypedValueLattice[T, I]): ValueLattice[IndexedLattice[T, I]] with {

  import IndexedLattice.{Bot, Elem, Top}

  inline def checkBinaryTopBot(x: IndexedLattice[T, I], y: IndexedLattice[T, I]): Either[IndexedLattice[T, I], (T, T)] =
    (x, y) match {
      case (Bot(), _) | (_, Bot()) => Left(Bot())
      case (Top(), _) | (_, Top()) => Left(Top())
      case (Elem(x), Elem(y)) => Right((x, y))
    }

  inline def checkBinaryWidths(x: T, y: T): Either[IndexedLattice[T, I], (T, T)] =
    (x, y) match {
      case (x, y) if x.getType != y.getType => Left(lattice.handleConflictingTypes(x, y))
      case (x, y) => Right((x, y))
    }

  def checkBinary(x: IndexedLattice[T, I], y: IndexedLattice[T, I])(f: (T, T) => T): IndexedLattice[T, I] =
    checkBinaryTopBot(x, y)
      .flatMap { case (x, y) => checkBinaryWidths(x, y) }
      .map { case (x, y) => Elem[T, I](f(x, y)) }
      .fold(identity, identity)

  def checkUnary(x: IndexedLattice[T, I])(f: T => T): IndexedLattice[T, I] = {
    x match {
      case Bot() => Bot()
      case Top() => Top()
      case Elem(x) => Elem(f(x))
    }
  }

  def top: IndexedLattice[T, I] = Top()
  def bottom: IndexedLattice[T, I] = Bot()

  def top(ty: I): IndexedLattice[T, I] = Elem(lattice.top(ty))
  def bottom(ty: I): IndexedLattice[T, I] = Elem(lattice.bottom(ty))

  def lub(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `join` _)
  def glb(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `meet` _)

  def constant(v: Literal) = Elem(lattice.constant(v))

  def bvnot(x: IndexedLattice[T, I]) = checkUnary(x)(_.bvnot())
  def bvneg(x: IndexedLattice[T, I]) = checkUnary(x)(_.bvneg())
  def boolnot(x: IndexedLattice[T, I]) = checkUnary(x)(_.boolnot())
  def intneg(x: IndexedLattice[T, I]) = checkUnary(x)(_.intneg())
  def booltobv1(x: IndexedLattice[T, I]) = checkUnary(x)(_.booltobv1())

  def equal(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `equal` _)
  def bvcomp(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvcomp` _)
  def bvand(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvand` _)
  def bvor(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvor` _)
  def bvadd(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvadd` _)
  def bvmul(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvmul` _)
  def bvshl(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvshl` _)
  def bvlshr(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvlshr` _)
  def bvashr(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvashr` _)
  def bvult(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvult` _)
  def bvxor(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvxor` _)
  def bvsub(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvsub` _)
  def bvurem(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvurem` _)
  def bvsrem(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvsrem` _)
  def bvsmod(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvsmod` _)
  def bvudiv(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvudiv` _)
  def bvsdiv(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvsdiv` _)
  def bvule(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvule` _)
  def bvugt(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvugt` _)
  def bvslt(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvslt` _)
  def bvsle(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvsle` _)
  def bvsgt(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvsgt` _)
  def bvsge(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvsge` _)
  def bvuge(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `bvuge` _)
  def bvconcat(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) =
    checkBinaryTopBot(x, y).map(_ `bvconcat` _).map(Elem[T, I](_)).fold(identity, identity)

  def intlt(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `intlt` _)
  def intle(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `intle` _)
  def intgt(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `intgt` _)
  def intge(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `intge` _)
  def intadd(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `intadd` _)
  def intsub(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `intsub` _)
  def intmul(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `intmul` _)
  def intdiv(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `intdiv` _)
  def intmod(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `intmod` _)

  def booland(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `booland` _)
  def boolor(x: IndexedLattice[T, I], y: IndexedLattice[T, I]) = checkBinary(x, y)(_ `boolor` _)

  def zero_extend(x: IndexedLattice[T, I], extend: Int) = checkUnary(x)(_.zero_extend(extend))
  def sign_extend(x: IndexedLattice[T, I], extend: Int) = checkUnary(x)(_.zero_extend(extend))
  def repeat(x: IndexedLattice[T, I], repeats: Int) = checkUnary(x)(_.repeat(repeats))
  def extract(x: IndexedLattice[T, I], hi: Int, lo: Int) = checkUnary(x)(_.extract(hi, lo))
}

class NOPValueAnalysis[ValueType]() extends ValueLattice[ValueType] {
  def top: ValueType = ???
  def bottom: ValueType = ???
  def top(ty: IRType): ValueType = ???
  def bottom(ty: IRType): ValueType = ???

  def glb(x: ValueType, y: ValueType) = top
  def lub(x: ValueType, y: ValueType) = top

  def bvnot(x: ValueType): ValueType = top
  def bvneg(x: ValueType): ValueType = top
  def boolnot(x: ValueType): ValueType = top
  def intneg(x: ValueType): ValueType = top
  def booltobv1(x: ValueType): ValueType = top

  def equal(x: ValueType, other: ValueType): ValueType = top
  def bvcomp(x: ValueType, other: ValueType): ValueType = top
  def bvand(x: ValueType, other: ValueType): ValueType = top
  def bvor(x: ValueType, other: ValueType): ValueType = top
  def bvadd(x: ValueType, other: ValueType): ValueType = top
  def bvmul(x: ValueType, other: ValueType): ValueType = top
  def bvshl(x: ValueType, other: ValueType): ValueType = top
  def bvlshr(x: ValueType, other: ValueType): ValueType = top
  def bvashr(x: ValueType, other: ValueType): ValueType = top
  def bvult(x: ValueType, other: ValueType): ValueType = top
  def bvxor(x: ValueType, other: ValueType): ValueType = top
  def bvsub(x: ValueType, other: ValueType): ValueType = top
  def bvurem(x: ValueType, other: ValueType): ValueType = top
  def bvsrem(x: ValueType, other: ValueType): ValueType = top
  def bvsmod(x: ValueType, other: ValueType): ValueType = top
  def bvudiv(x: ValueType, other: ValueType): ValueType = top
  def bvsdiv(x: ValueType, other: ValueType): ValueType = top
  def bvule(x: ValueType, other: ValueType): ValueType = top
  def bvugt(x: ValueType, other: ValueType): ValueType = top
  def bvslt(x: ValueType, other: ValueType): ValueType = top
  def bvsle(x: ValueType, other: ValueType): ValueType = top
  def bvsgt(x: ValueType, other: ValueType): ValueType = top
  def bvsge(x: ValueType, other: ValueType): ValueType = top
  def bvuge(x: ValueType, other: ValueType): ValueType = top
  def bvconcat(x: ValueType, other: ValueType): ValueType = top

  def intlt(x: ValueType, other: ValueType): ValueType = top
  def intle(x: ValueType, other: ValueType): ValueType = top
  def intgt(x: ValueType, other: ValueType): ValueType = top
  def intge(x: ValueType, other: ValueType): ValueType = top
  def intadd(x: ValueType, other: ValueType): ValueType = top
  def intsub(x: ValueType, other: ValueType): ValueType = top
  def intmul(x: ValueType, other: ValueType): ValueType = top
  def intdiv(x: ValueType, other: ValueType): ValueType = top
  def intmod(x: ValueType, other: ValueType): ValueType = top

  def booland(x: ValueType, other: ValueType): ValueType = top
  def boolor(x: ValueType, other: ValueType): ValueType = top

  def constant(v: Literal): ValueType = top
  def zero_extend(x: ValueType, extend: Int): ValueType = top
  def sign_extend(x: ValueType, extend: Int): ValueType = top
  def repeat(x: ValueType, repeats: Int): ValueType = top
  def extract(x: ValueType, hi: Int, lo: Int): ValueType = top
}

class EvaluateInLattice[Value](using lattice: TypedValueLattice[Value, IRType]) {

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

trait AbsEvalExpr[AbsValue] {
  def top: AbsValue
  def evalExpr(read: Variable => Option[AbsValue])(e: Expr): AbsValue
}

case class ProductInternalLattice[V1, V2](a: V1, b: V2)

given [V1, V2](using a: Lattice[V1], b: Lattice[V2]): Lattice[ProductInternalLattice[V1, V2]] with {

  def lub(x: Element, other: ProductInternalLattice[V1, V2]) = {
    ProductInternalLattice(x.a.join(other.a), x.b.join(other.b))
  }
  def glb(x: Element, other: ProductInternalLattice[V1, V2]): ProductInternalLattice[V1, V2] = {
    ProductInternalLattice(x.a.meet(other.a), x.b.meet(other.b))
  }

  def top = (ProductInternalLattice(a.top, b.top))
  def bottom = (ProductInternalLattice(a.bottom, b.bottom))
}

class ProductValueLattice[V1, V2](using
  lattice1: TypedValueLattice[V1, IRType],
  lattice2: TypedValueLattice[V2, IRType]
) extends AbsEvalExpr[ProductInternalLattice[V1, V2]] {

  val top = ProductInternalLattice(lattice1.top, lattice2.top)
  def bottom: ProductInternalLattice[V1, V2] = ???
  def lub(x: ProductInternalLattice[V1, V2], y: ProductInternalLattice[V1, V2]) = x.join(y)
  def glb(x: ProductInternalLattice[V1, V2], y: ProductInternalLattice[V1, V2]) = ???

  def refine(v: (V1, V2)): ProductInternalLattice[V1, V2] = {
    val (v1, v2) = v

    // TODO:
    // cast v1 to v2' ; v2.meet(v2')
    // case v2 to v1' ; v1.meet(v1')
    // loop to fixed point etc

    ???
  }

  val eval1 = EvaluateInLattice[V1]()
  val eval2 = EvaluateInLattice[V2]()

  def evalExpr(read: Variable => Option[ProductInternalLattice[V1, V2]])(e: Expr): ProductInternalLattice[V1, V2] = {
    val v1 = eval1.evalExpr(v => read(v).map(_._1))(e)
    val v2 = eval2.evalExpr(v => read(v).map(_._2))(e)
    refine((v1, v2))
  }
}

class DefaultValueLattice[Value](using lattice: TypedValueLattice[Value, IRType]) extends AbsEvalExpr[Value] {
  val eval = EvaluateInLattice[Value]()

  def top = lattice.top
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

class DefaultTransfer[L](eval: AbsEvalExpr[L])(using lattice: Lattice[L]) extends TransferFun[LatticeMap[Variable, L]] {

  def assign(v: LatticeMap[Variable, L], assignments: Seq[(Variable, Expr)]): LatticeMap[Variable, L] = {
    val get = v.toMap.get
    val n = assignments.map { case (v, e) =>
      v -> eval.evalExpr(get)(e)
    }
    n.foldLeft(v)((v, e) => v.update(e))
  }

  def indirect_call(v: LatticeMap[Variable, L], e: Variable): LatticeMap[Variable, L] = LatticeMap.Top()

  def direct_call(v: LatticeMap[Variable, L], d: DirectCall): LatticeMap[Variable, L] =
    d.outParams.map(_._2 -> lattice.top).foldLeft(v)((v, e) => v.update(e))

  def memory_store(v: LatticeMap[Variable, L], m: MemoryStore): LatticeMap[Variable, L] = v

  def memory_load(v: LatticeMap[Variable, L], m: MemoryLoad): LatticeMap[Variable, L] =
    v.update(m.lhs, lattice.top)

  def memory_assign(v: LatticeMap[Variable, L], m: MemoryAssign): LatticeMap[Variable, L] = v

  def jump(v: LatticeMap[Variable, L], m: Jump): LatticeMap[Variable, L] = v
}

class ValueStateDomain[L](transferFn: TransferFun[LatticeMap[Variable, L]], innerLattice: AbsEvalExpr[L])(using
  lattice: Lattice[L]
) extends MapDomain[Variable, L] {
  //  val innerLattice : DefaultValueLattice[L] = DefaultValueLattice[L](topValue)

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

def valueAnalysis[L](lattice: TypedValueLattice[L, IRType])(p: Procedure) = {
  val l = DefaultValueLattice(using lattice)
  val d = ValueStateDomain(DefaultTransfer(l)(using lattice), l)(using lattice)
  val solve = ir.transforms.worklistSolver(d)
  solve.solveProc(p, backwards = false)
}

def productValueAnalysis[L1, L2](l1: TypedValueLattice[L1, IRType], l2: TypedValueLattice[L2, IRType])(p: Procedure) = {
  val productLattice = given_Lattice_ProductInternalLattice(using l1, l2)
  val l = ProductValueLattice[L1, L2](using l1, l2)
  val d = ValueStateDomain[ProductInternalLattice[L1, L2]](
    DefaultTransfer(l)(using productLattice),
    ProductValueLattice(using l1, l2)
  )(using productLattice)
  val solve = ir.transforms.worklistSolver(d)
  solve.solveProc(p, backwards = false)
}
