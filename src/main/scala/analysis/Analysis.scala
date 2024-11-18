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
trait ConstantPropagation(val program: Program) {
  /** The lattice of abstract states.
    */

  val valuelattice: ConstantPropagationLattice = ConstantPropagationLattice()

  val statelattice: MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice] = MapLattice(valuelattice)

  /** Default implementation of eval.
    */
  def eval(exp: Expr, env: Map[Variable, FlatElement[BitVecLiteral]]): FlatElement[BitVecLiteral] = {
    import valuelattice._
    exp match {
      case id: Variable => env(id)
      case n: BitVecLiteral => bv(n)
      case ze: ZeroExtend => zero_extend(ze.extension, eval(ze.body, env))
      case se: SignExtend => sign_extend(se.extension, eval(se.body, env))
      case e: Extract => extract(e.end, e.start, eval(e.body, env))
      case bin: BinaryExpr =>
        val left = eval(bin.arg1, env)
        val right = eval(bin.arg2, env)
        bin.op match {
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
        }
      case un: UnaryExpr =>
        val arg = eval(un.arg, env)
        un.op match {
          case BVNOT => bvnot(arg)
          case BVNEG => bvneg(arg)
        }
      case _ => valuelattice.top
    }
  }


  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CFGPosition, s: Map[Variable, FlatElement[BitVecLiteral]]): Map[Variable, FlatElement[BitVecLiteral]] = {
    n match {
      // assignments
      case la: Assign =>
        s + (la.lhs -> eval(la.rhs, s))
      // all others: like no-ops
      case _ => s
    }
  }

  /** The analysis lattice.
    */
  val lattice: MapLattice[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]], MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice]] = MapLattice(statelattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CFGPosition, s: Map[Variable, FlatElement[BitVecLiteral]]): Map[Variable, FlatElement[BitVecLiteral]] = localTransfer(n, s)
}

class ConstantPropagationSolver(program: Program) extends ConstantPropagation(program)
    with SimplePushDownWorklistFixpointSolver[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]], MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice]]
    with IRInterproceduralForwardDependencies
    with Analysis[Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]]