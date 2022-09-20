/**
  * Copyright 2022 The University of Queensland
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *   http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  *
  * The following classes were adapted from TIP (https://github.com/cs-au-dk/TIP)
  *    LatticeSolver, SimpleFixpointSolver, MapLatticeSolver, SimpleMapLatticeFixpointSolver
  */
package analysis.solvers

import analysis._

/** Base trait for lattice solvers.
  */
trait LatticeSolver:

  /** The lattice used by the solver.
    */
  val lattice: Lattice

  /** The analyze function.
    */
  def analyze(): lattice.Element

/** Simple fixpoint solver.
  */
trait SimpleFixpointSolver extends LatticeSolver:

  /** The constraint function for which the least fixpoint is to be computed.
    * @param x
    *   the input lattice element
    * @return
    *   the output lattice element
    */
  def fun(x: lattice.Element): lattice.Element

  /** The basic Kleene fixpoint solver.
    */
  def analyze(): lattice.Element =
    var x = lattice.bottom
    var t = x
    while
      t = x
      x = fun(x)
      x != t
    do ()
    x

/** Base trait for map lattice solvers.
  * @tparam N
  *   type of the elements in the map domain.
  */
trait MapLatticeSolver[N] extends LatticeSolver with Dependencies[N]:

  /** Must be a map lattice.
    */
  val lattice: MapLattice[N, Lattice]

  /** The transfer function.
    */
  def transfer(n: N, s: lattice.sublattice.Element): lattice.sublattice.Element

  /** The constraint function for individual elements in the map domain. First computes the join of the incoming
    * elements and then applies the transfer function.
    * @param n
    *   the current location in the map domain
    * @param x
    *   the current lattice element for all locations
    * @return
    *   the output sublattice element
    */
  def funsub(n: N, x: lattice.Element): lattice.sublattice.Element =
    transfer(n, join(n, x))

  /** Computes the least upper bound of the incoming elements.
    */
  def join(n: N, o: lattice.Element): lattice.sublattice.Element =
    val states = indep(n).map(o(_))
    states.foldLeft(lattice.sublattice.bottom)((acc, pred) => lattice.sublattice.lub(acc, pred))

/** Simple fixpoint solver for map lattices where the constraint function is defined pointwise.
  * @tparam N
  *   type of the elements in the map domain.
  */
trait SimpleMapLatticeFixpointSolver[N] extends SimpleFixpointSolver with MapLatticeSolver[N]:

  /** The map domain.
    */
  val domain: Set[N]

  /** The function for which the least fixpoint is to be computed. Applies the sublattice constraint function pointwise
    * to each entry.
    * @param x
    *   the input lattice element
    * @return
    *   the output lattice element
    */
  def fun(x: lattice.Element): lattice.Element = {
    domain.foldLeft(lattice.bottom)((m, a) =>
      m + (a -> {
        funsub(a, x)
      })
    )
  }
