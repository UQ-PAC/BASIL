package analysis

import ir.*
import analysis.solvers.*

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable
import util.Logger

/** Trait for program analyses.
  *
  * @tparam R
  *   the type of the analysis result
  */
trait Analysis[+R]:

  /** Performs the analysis and returns the result.
    */
  def analyze(): R

/** Base class for value analysis with simple (non-lifted) lattice.
  */
trait ConstantPropagation(val program: Program, val assumeR31: Boolean) {
  /** The lattice of abstract states.
    */

  val valuelattice: ConstantPropagationLattice = ConstantPropagationLattice()

  val statelattice: MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice] = MapLattice(valuelattice)

  /** Default implementation of eval.
    */
  def eval(exp: Expr, env: Map[Variable, FlatElement[BitVecLiteral]]): FlatElement[BitVecLiteral] =
    import valuelattice._
    exp match
      case id: Variable => env(id)
      case n: BitVecLiteral => bv(n)
      case ze: ZeroExtend => zero_extend(ze.extension, eval(ze.body, env))
      case se: SignExtend => sign_extend(se.extension, eval(se.body, env))
      case e: Extract => extract(e.end, e.start, eval(e.body, env))
      case bin: BinaryExpr =>
        val left = eval(bin.arg1, env)
        val right = eval(bin.arg2, env)
        bin.op match
          case BVADD => bvadd(left, right)
          case BVSUB => bvsub(left, right)
          case BVMUL => bvmul(left, right)
          case BVUDIV => bvudiv(left, right)
          case BVSDIV => bvsdiv(left, right)
          case BVSREM => bvsrem(left, right)
          case BVUREM => bvurem(left, right)
          case BVSMOD => bvsmod(left, right)
          case BVAND => bvand(left, right)
          case BVOR => bvor(left, right)
          case BVXOR => bvxor(left, right)
          case BVNAND => bvnand(left, right)
          case BVNOR => bvnor(left, right)
          case BVXNOR => bvxnor(left, right)
          case BVSHL => bvshl(left, right)
          case BVLSHR => bvlshr(left, right)
          case BVASHR => bvashr(left, right)
          case BVCOMP => bvcomp(left, right)
          case BVCONCAT => concat(left, right)

      case un: UnaryExpr =>
        val arg = eval(un.arg, env)

        un.op match
          case BVNOT => bvnot(arg)
          case BVNEG => bvneg(arg)

      case _ => valuelattice.top

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CFGPosition, s: Map[Variable, FlatElement[BitVecLiteral]]): Map[Variable, FlatElement[BitVecLiteral]] =
    var m = s
    n match
      case r: Command =>
        r match
          // assignments
          case la: Assign =>
            m + (la.lhs -> eval(la.rhs, m))
          // all others: like no-ops
          case _ => m
      case _ => m

  /** The analysis lattice.
    */
  val lattice: MapLattice[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]], MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice]] = MapLattice(statelattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CFGPosition, s: Map[Variable, FlatElement[BitVecLiteral]]): Map[Variable, FlatElement[BitVecLiteral]] = localTransfer(n, s)
}

class ConstantPropagationSolver(program: Program, assumeR31: Boolean = false) extends ConstantPropagation(program, assumeR31)
    with SimplePushDownWorklistFixpointSolver[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]], MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice]]
    with IRInterproceduralForwardDependencies
    with Analysis[Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]]


/** Base class for value analysis with simple (non-lifted) lattice.
 */
trait ConstantPropagationWithSSA(val program: Program, val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]) {
  /** The lattice of abstract states.
   */

  val valuelattice: ConstantPropagationLatticeWithSSA = ConstantPropagationLatticeWithSSA()

  val statelattice: MapLattice[RegisterWrapperEqualSets, Set[BitVecLiteral], ConstantPropagationLatticeWithSSA] = MapLattice(valuelattice)

  /** Default implementation of eval.
   */
  def eval(exp: Expr, env: Map[RegisterWrapperEqualSets, Set[BitVecLiteral]], n: CFGPosition): Set[BitVecLiteral] =
    import valuelattice._
    exp match
      case id: Variable => env(RegisterWrapperEqualSets(id, getUse(id, n, reachingDefs)))
      case n: BitVecLiteral => bv(n)
      case ze: ZeroExtend => zero_extend(ze.extension, eval(ze.body, env, n))
      case se: SignExtend => sign_extend(se.extension, eval(se.body, env, n))
      case e: Extract => extract(e.end, e.start, eval(e.body, env, n))
      case bin: BinaryExpr =>
        val left = eval(bin.arg1, env, n)
        val right = eval(bin.arg2, env, n)
        bin.op match
          case BVADD => bvadd(left, right)
          case BVSUB => bvsub(left, right)
          case BVMUL => bvmul(left, right)
          case BVUDIV => bvudiv(left, right)
          case BVSDIV => bvsdiv(left, right)
          case BVSREM => bvsrem(left, right)
          case BVUREM => bvurem(left, right)
          case BVSMOD => bvsmod(left, right)
          case BVAND => bvand(left, right)
          case BVOR => bvor(left, right)
          case BVXOR => bvxor(left, right)
          case BVNAND => bvnand(left, right)
          case BVNOR => bvnor(left, right)
          case BVXNOR => bvxnor(left, right)
          case BVSHL => bvshl(left, right)
          case BVLSHR => bvlshr(left, right)
          case BVASHR => bvashr(left, right)
          case BVCOMP => bvcomp(left, right)
          case BVCONCAT => concat(left, right)

      case un: UnaryExpr =>
        val arg = eval(un.arg, env, n)

        un.op match
          case BVNOT => bvnot(arg)
          case BVNEG => bvneg(arg)

      case _ => Set.empty

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition, s: Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]): Map[RegisterWrapperEqualSets, Set[BitVecLiteral]] =
    n match {
      case r: Command =>
        r match {
          // assignments
          case a: Assign =>
            val lhsWrappers = s.collect {
              case (k, v) if RegisterVariableWrapper(k.variable, k.assigns) == RegisterVariableWrapper(a.lhs, getDefinition(a.lhs, r, reachingDefs)) => (k, v)
            }
            if (lhsWrappers.nonEmpty) {
              s ++ lhsWrappers.map((k, v) => (k, v.union(eval(a.rhs, s, r))))
            } else {
              s + (RegisterWrapperEqualSets(a.lhs, getDefinition(a.lhs, r, reachingDefs)) -> eval(a.rhs, s, n))
            }
          // all others: like no-ops
          case _ => s
        }
      case _ => s
    }

  /** The analysis lattice.
   */
  val lattice: MapLattice[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]], MapLattice[RegisterWrapperEqualSets, Set[BitVecLiteral], ConstantPropagationLatticeWithSSA]] = MapLattice(statelattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
   */
  def transfer(n: CFGPosition, s: Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]): Map[RegisterWrapperEqualSets, Set[BitVecLiteral]] = localTransfer(n, s)
}

class ConstantPropagationSolverWithSSA(program: Program, reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]) extends ConstantPropagationWithSSA(program, reachingDefs)
  with SimplePushDownWorklistFixpointSolver[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]], MapLattice[RegisterWrapperEqualSets, Set[BitVecLiteral], ConstantPropagationLatticeWithSSA]]
  with IRInterproceduralForwardDependencies
  with Analysis[Map[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]]]
