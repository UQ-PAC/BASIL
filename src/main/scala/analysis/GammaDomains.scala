package analysis

import analysis.*
import ir.*
import ir.transforms.{AbstractDomain, worklistSolver}

/**
 * Lattice structure internal to a type.
 */
trait InternalLattice[T <: InternalLattice[T]] {
  def join(other: T): T
  def meet(other: T): T

  def \/ (other: T): T = this.join(other)
  def /\ (other: T): T = this.meet(other)
  def ⊔ (other: T): T = this.join(other)
  def ⊓ (other: T): T = this.meet(other)

  def top: T
  def bottom: T
}

/**
 * A set belonging to a powerset lattice of the set (type) T.
 * We encode an abstract Top and Bottom value, along with sets that represent a finite set, along with T - some finite set.
 */
enum LatticeSet[T] extends InternalLattice[LatticeSet[T]] {
  def contains(v: T): Boolean = {
    this match {
      case Top() => true
      case Bottom() => true
      case FiniteSet(s) => s.contains(v)
      case DiffSet(s) => !s.contains(v)
    }
  }

  def join(other: LatticeSet[T]): LatticeSet[T] = {
    (this, other) match {
      case (Top(), _) => Top()
      case (Bottom(), b) => b
      case (FiniteSet(a), FiniteSet(b)) => FiniteSet(a.union(b))
      case (FiniteSet(a), DiffSet(b)) => DiffSet(b -- a)
      case (DiffSet(a), DiffSet(b)) => DiffSet(a.intersect(b))
      case (a, b) => b.join(a)
    }
  }

  def meet(other: LatticeSet[T]): LatticeSet[T] = {
    (this, other) match {
      case (Top(), b) => b
      case (Bottom(), _) => Bottom()
      case (FiniteSet(a), FiniteSet(b)) => FiniteSet(a.intersect(b))
      case (FiniteSet(a), DiffSet(b)) => FiniteSet(a.filter(x => !b.contains(x)))
      case (DiffSet(a), DiffSet(b)) => DiffSet(a.union(b))
      case (a, b) => b.meet(a)
    }
  }

  def union(other: LatticeSet[T]): LatticeSet[T] = this.join(other)
  def intersect(other: LatticeSet[T]): LatticeSet[T] = this.meet(other)
  def ∪ (other: LatticeSet[T]): LatticeSet[T] = this.join(other)
  def ∩ (other: LatticeSet[T]): LatticeSet[T] = this.meet(other)

  def top: LatticeSet[T] = Top()
  def bottom: LatticeSet[T] = Bottom()

  case Top[T1]() extends LatticeSet[T1]
  case Bottom[T1]() extends LatticeSet[T1]
  case FiniteSet[T1](s: Set[T1]) extends LatticeSet[T1]
  // Represents T - s
  case DiffSet[T1](s: Set[T1]) extends LatticeSet[T1]
}


/*
/*
 * D: Domain
 * C: Codomain
 */
enum LatticeMap[D, L <: InternalLattice[L]] extends InternalLattice[LatticeMap[D, L]] {
  case Top[D1, L1 <: InternalLattice[L1]]() extends LatticeMap[D1, L1]
  case Bottom[D1, L1 <: InternalLattice[L1]]() extends LatticeMap[D1, L1]
  // A Map which defaults to Top
  case TopMap[D1, L1 <: InternalLattice[L1]](m: Map[D1, L1]) extends LatticeMap[D1, L1]
  // A Map which defaults to Bottom
  case BottomMap[D1, L1 <: InternalLattice[L1]](m: Map[D1, L1]) extends LatticeMap[D1, L1]

  def join(other: LatticeMap[D, L]): LatticeMap[D, L] = {
    (this, other) match {
      case (Top(), _) => Top()
      case (Bottom(), _) => Bottom()
      case (TopMap(a), TopMap(b)) => TopMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.top).join(v))
        }))
      case (TopMap(a), BottomMap(b)) => TopMap(
        b.foldLeft(a)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.top).join(v))
        }))
      case (BottomMap(a), BottomMap(b)) => BottomMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.bottom).join(v))
        }))
      case (a, b) => b.join(a)
    }
  }

  def meet(other: LatticeMap[D, L]): LatticeMap[D, L] = {
    (this, other) match {
      case (Top(), b) => b
      case (Bottom(), _) => Bottom()
      case (TopMap(a), TopMap(b)) => TopMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.top).meet(v))
        }))
      case (TopMap(a), BottomMap(b)) => BottomMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.bottom).meet(v))
        }))
      case (BottomMap(a), BottomMap(b)) => BottomMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.bottom).meet(v))
        }))
      case (a, b) => b.meet(a)
    }
  }

  def top: LatticeMap[D, L] = Top()
  def bottom: LatticeMap[D, L] = Bottom()
}
*/

/**
 * A map lattice map which has an abstract Top and Bottom value, along with variants for maps that default to Top and Bottom.
 * Because of the way I designed this unfortunately I wasn't able to make it generic! :( Oh well
 * This probably needs to be rewritten
 */
enum VarGammaMap extends InternalLattice[VarGammaMap] with (Taintable => LatticeSet[Taintable]) {
  case Top
  case Bottom
  // A Map which defaults to Top
  case TopMap(m: Map[Taintable, LatticeSet[Taintable]])
  // A Map which defaults to Bottom
  case BottomMap(m: Map[Taintable, LatticeSet[Taintable]])

  override def toString(): String = this match {
    case Top => "Top"
    case Bottom => "Bottom"
    case TopMap(m) => "TopMap("+m.toString()+")"
    case BottomMap(m) => "BottomMap("+m.toString()+")"
  }

  def apply(v: Taintable): LatticeSet[Taintable] = this match {
    case Top => LatticeSet.Top()
    case Bottom => LatticeSet.Bottom()
    case TopMap(m) => m.getOrElse(v, LatticeSet.Top())
    case BottomMap(m) => m.getOrElse(v, LatticeSet.Bottom())
  }

  def join(other: VarGammaMap): VarGammaMap = {
    (this, other) match {
      case (Top, _) => Top
      case (Bottom, _) => Bottom
      case (TopMap(a), TopMap(b)) => TopMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.top).join(v))
        }))
      case (TopMap(a), BottomMap(b)) => TopMap(
        b.foldLeft(a)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.top).join(v))
        }))
      case (BottomMap(a), BottomMap(b)) => BottomMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.bottom).join(v))
        }))
      case (a, b) => b.join(a)
    }
  }

  def meet(other: VarGammaMap): VarGammaMap = {
    (this, other) match {
      case (Top, b) => b
      case (Bottom, _) => Bottom
      case (TopMap(a), TopMap(b)) => TopMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.top).meet(v))
        }))
      case (TopMap(a), BottomMap(b)) => BottomMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.bottom).meet(v))
        }))
      case (BottomMap(a), BottomMap(b)) => BottomMap(
        a.foldLeft(b)((m, p) => {
          val (k, v) = p
          m + (k -> m.getOrElse(k, v.bottom).meet(v))
        }))
      case (a, b) => b.meet(a)
    }
  }

  def top: VarGammaMap = Top
  def bottom: VarGammaMap = Bottom
}

/**
 * An abstract domain that determines for each variable at each block, a set of variables
 * whose gammas (at the start of a procedure) definitely "affected" this variable's gamma.
 *
 * This is not perfectly precise. For example, if (given three variables x, y, z) the only
 * two possible states for gammas was
 * [x -> Gamma_old(y), y -> Gamma_old(z), z -> Gamma_old(z)]
 * and
 * [x -> Gamma_old(x), y -> Gamma_old(y), z -> Gamma_old(z)]
 * we would store
 * [x -> Set(), y -> Set(), z -> Set(z)]
 * which loses information. It would be more precise to store a set of maps as opposed to a
 * map to sets, however this would be significantly more expensive. Hopefully this loss of
 * precision isn't very significant!
 */
class MustGammaDomain(
  globals: Map[BigInt, String],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
) extends AbstractDomain[VarGammaMap] {
  import VarGammaMap.{Top, Bottom, TopMap, BottomMap}

  // Meeting on places we would normally join is how we get our must analysis.
  def join(a: VarGammaMap, b: VarGammaMap, pos: Block): VarGammaMap = a.meet(b)

  def transfer(m: VarGammaMap, c: Command): VarGammaMap = {
    c match {
      case c: LocalAssign => m match {
        case Top => Top
        case Bottom => Bottom
        case TopMap(m) =>
          TopMap(m + (c.lhs -> c.rhs.variables.foldLeft(LatticeSet.Bottom())((s: LatticeSet[Taintable], v) => s.union(m.getOrElse(v, LatticeSet.Top())))))
        case BottomMap(m) =>
          BottomMap(m + (c.lhs -> c.rhs.variables.foldLeft(LatticeSet.Bottom())((s: LatticeSet[Taintable], v) => s.union(m.getOrElse(v, LatticeSet.Bottom())))))
      }
      case c: MemoryLoad => {
        val v = getMemoryVariable(c, c.mem, c.index, c.size, constProp, globals).getOrElse(UnknownMemory())
        (m, v) match {
          case (Top, _) => Top
          case (Bottom, _) => Bottom
          // If we load unknown memory, make no statements about the gammas of the loaded-into variable.
          case (TopMap(m), UnknownMemory()) => TopMap(m + (c.lhs -> LatticeSet.Bottom()))
          case (BottomMap(m), UnknownMemory()) => BottomMap(m + (c.lhs -> LatticeSet.Bottom()))
          case (TopMap(m), v) =>
            TopMap(m + (c.lhs -> c.index.variables.foldLeft(m.getOrElse(v, LatticeSet.Top()))((s: LatticeSet[Taintable], v) => s.union(m.getOrElse(v, LatticeSet.Top())))))
          case (BottomMap(m), v) =>
            BottomMap(m + (c.lhs -> c.index.variables.foldLeft(m.getOrElse(v, LatticeSet.Bottom()))((s: LatticeSet[Taintable], v) => s.union(m.getOrElse(v, LatticeSet.Bottom())))))
        }
      }
      case c: MemoryStore => {
        val v = getMemoryVariable(c, c.mem, c.index, c.size, constProp, globals).getOrElse(UnknownMemory())
        (m, v) match {
         // If we write to UnknownMemory, we won't ever use the results of what is stored there (since we bottom on read).
         // Hence we simply store no new information.
          case (m, UnknownMemory()) => m
          case (Top, _) => Top
          case (Bottom, _) => Bottom
          case (TopMap(m), v) =>
            TopMap(m + (v -> c.value.variables.union(c.index.variables).foldLeft(LatticeSet.Bottom())((s: LatticeSet[Taintable], v) => s.union(m.getOrElse(v, LatticeSet.Top())))))
          case (BottomMap(m), v) =>
            BottomMap(m + (v -> c.value.variables.union(c.index.variables).foldLeft(LatticeSet.Bottom())((s: LatticeSet[Taintable], v) => s.union(m.getOrElse(v, LatticeSet.Bottom())))))
        }
      }
      case c: Assume       => m
      case c: Assert       => m
      case c: IndirectCall => Bottom
      case c: DirectCall   => Bottom
      case c: GoTo         => m
      case c: Return       => m
      case c: Unreachable  => m
      case c: NOP          => m
    }
  }

  def top: VarGammaMap = VarGammaMap.Bottom
  // Since bottom is set to Top, it is important to ensure that when running this analysis, the entry point
  // to the procedure you are analysing has things mostly set to bottom. That way, Top does not propagate.
  def bot: VarGammaMap = VarGammaMap.Top
}

/**
 * Computes sufficient conditions for a block to be reached. Currently, we only know that the blocks
 * prior to a branch are definitely reachable, and (soundly) assume everything else might not be
 * reachable.
 */
class ReachabilityConditions extends AbstractDomain[Boolean] {
  import VarGammaMap.{Top, Bottom, TopMap, BottomMap}

  def join(a: Boolean, b: Boolean, pos: Block): Boolean = a && b

  def transfer(b: Boolean, c: Command): Boolean = {
    c match {
      case a: LocalAssign  => b
      case a: MemoryLoad   => b
      case m: MemoryStore  => b
      case a: Assume       => b
      case a: Assert       => b
      case i: IndirectCall => b
      case c: DirectCall   => b
      case g: GoTo         => g.targets.size == 1 && b
      case r: Return       => b
      case r: Unreachable  => b
      case n: NOP          => b
    }
  }

  def top: Boolean = true
  def bot: Boolean = false
}
