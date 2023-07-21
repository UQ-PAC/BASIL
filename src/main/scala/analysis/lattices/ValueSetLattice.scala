package analysis.lattices

import ir._
import analysis.util._

/** Value Set lattice
 */
object ValueSetLattice extends FlatLattice[Literal]() with LatticeWithDefaultOps:

  override def apply(op: (Literal, Literal) => Literal, a: Element, b: Element): Element = (a, b) match
    case (FlatElement.FlatEl(x), FlatElement.FlatEl(y)) => FlatElement.FlatEl(op(x, y))
    case (FlatElement.Bot, _) => FlatElement.Bot
    case (_, FlatElement.Bot) => FlatElement.Bot
    case (_, FlatElement.Top) => FlatElement.Top
    case (FlatElement.Top, _) => FlatElement.Top

  override def apply(op: Literal => Literal, a: Element): Element = a match
    case FlatElement.FlatEl(x) => FlatElement.FlatEl(op(x))
    case FlatElement.Top => FlatElement.Top
    case FlatElement.Bot => FlatElement.Bot

  def literal(l: Literal): Element = FlatElement.FlatEl(l)