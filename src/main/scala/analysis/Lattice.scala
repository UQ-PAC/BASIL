package analysis

trait Lattice[LatticeElement] {

  /** A union or join of two lattice elements. Should contain all the information from the first state as well as all
    * the information from the second state - even if this introduces uncertainty.
    */
  def join(x: LatticeElement, y: LatticeElement): LatticeElement

  /** An intersection or meet of two lattice elements. Should contain all the information that appears in both states.
    */
  def meet(x: LatticeElement, y: LatticeElement): LatticeElement

  /** An ordering relation on the lattice elements so we check any given transfer for loss of precision. A return value
    * of true indicates that x is at least as precise as y
    */
  def leq(x: LatticeElement, y: LatticeElement): Boolean = join(x, y) == y
}

enum FlatElement[+T]:
  case Top
  case Bottom
  case Elem(value: T)

class FlatLattice[T] extends Lattice[FlatElement[T]] {
  override def join(x: FlatElement[T], y: FlatElement[T]): FlatElement[T] = x match
    case FlatElement.Top         => x
    case FlatElement.Bottom      => y
    case FlatElement.Elem(value) => if y == FlatElement.Bottom then x else FlatElement.Top

  override def meet(x: FlatElement[T], y: FlatElement[T]): FlatElement[T] = x match
    case FlatElement.Top         => y
    case FlatElement.Bottom      => x
    case FlatElement.Elem(value) => if y == FlatElement.Top then x else FlatElement.Bottom
}

object ConstantPropagationLattice extends FlatLattice[Int]
