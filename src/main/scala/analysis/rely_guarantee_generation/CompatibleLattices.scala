package analysis

import ir.*
import analysis.*

/** To be compatible for use in interference domains, state lattices must define
  * these additional functions.
  */
trait InterferenceCompatibleLattice[S] extends Lattice[S] {
  // weakens s by eliminating v
  def drop(v: Variable, s: S): S
  // greatest lower bound, i.e. meet
  def glb(s1: S, s2: S): S
  // display s as a boogie predicate
  def toPredString(s: S): String
}

/** A compatible LatticeMapLattice representing the interval domain, where each
  * element of the lattice maps Variables to Intervals, and where these
  * Intervals are ordered by the IntervalLattice.
  * 
  * @param l: A lattice over individual intervals, like [4, 7].
  */
class IntervalLatticeExtension(l: IntervalLattice)
    extends LatticeMapLattice[Variable, Interval, IntervalLattice](l)
    with InterferenceCompatibleLattice[LatticeMap[Variable, Interval]] {

  def drop(v: Variable, s: LatticeMap[Variable, Interval]): LatticeMap[Variable, Interval] =
    s + (v -> Interval.Top)

  // glb is already defined in LatticeMapLattice

  /* this is very bodgy but we assume here that the transfer function that is
  coupled with this lattice is SignedIntervalDomain().transfer */
  def toPredString(s: LatticeMap[Variable, Interval]): String = SignedIntervalDomain().toPred(s).toString()
}
