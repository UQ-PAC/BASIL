package analysis

import ir._
import analysis.solvers._
import analysis.lattices._
import boogie.BExpr
import specification.SpecGlobal

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable


/** Base class for memory region analysis (non-lifted) lattice.
 */
abstract class ValueSetAnalysis(val cfg: ProgramCfg,
                                val globals: Map[BigInt, String],
                                val externalFunctions: Map[BigInt, String],
                                val globalOffsets: Map[BigInt, BigInt],
                                val subroutines: Map[BigInt, String],
                                val mmm: MemoryModelMap)
  extends FlowSensitiveAnalysis(true) with MemoryRegionValueSetAnalysis {

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = localTransfer(n, s)

}


/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
 */
abstract class IntraprocValueSetAnalysisWorklistSolver[L <: MapLattice[Expr, PowersetLattice[Value]]]
(
  cfg: ProgramCfg,
  globals: Map[BigInt, String],
  externalFunctions: Map[BigInt, String],
  globalOffsets: Map[BigInt, BigInt],
  subroutines: Map[BigInt, String],
  mmm: MemoryModelMap,
  val powersetLattice: L
)
  extends ValueSetAnalysis(cfg, globals, externalFunctions, globalOffsets, subroutines, mmm)
    with SimpleMonotonicSolver[CfgNode]
    with ForwardDependencies

object ValueSetAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
   */
  class WorklistSolver(cfg: ProgramCfg,
                       globals: Map[BigInt, String],
                       externalFunctions: Map[BigInt, String],
                       globalOffsets: Map[BigInt, BigInt],
                       subroutines: Map[BigInt, String],
                       mmm: MemoryModelMap)
    extends IntraprocValueSetAnalysisWorklistSolver(
      cfg,
      globals,
      externalFunctions,
      globalOffsets,
      subroutines,
      mmm,
      MapLattice[Expr, PowersetLattice[Value]](PowersetLattice[Value])
    )




trait ValueAnalysisMisc:

  val cfg: ProgramCfg

  /** The lattice of abstract values.
    */
  val valuelattice: LatticeWithOps

  /** The lattice of abstract states.
    */
  val statelattice: MapLattice[Variable, valuelattice.type] = new MapLattice(valuelattice)

  /** Default implementation of eval.
    */
  def eval(exp: Expr, env: statelattice.Element): valuelattice.Element =
    import valuelattice._
    exp match
      case id: Variable        => env(id)
      case n: Literal          => literal(n)
      case ze: ZeroExtend => zero_extend(ze.extension, eval(ze.body, env))
      case se: SignExtend => sign_extend(se.extension, eval(se.body, env))
      case e: Extract          => extract(e.end, e.start, eval(e.body, env))
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
          case BVSMOD => ??? // Signed modulus. NOTE: this is used.
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

          case BVULE => bvule(left, right)
          case BVUGE => bvuge(left, right)
          case BVULT => bvult(left, right)
          case BVUGT => bvugt(left, right)

          case BVSLE => bvsle(left, right)
          case BVSGE => bvsge(left, right)
          case BVSLT => bvslt(left, right)
          case BVSGT => bvsgt(left, right)
          
          case BVCONCAT => concat(left, right)
          case BVNEQ => bvneq(left, right)
          case BVEQ => bveq(left, right)

          
      case un: UnaryExpr =>
        val arg = eval(un.arg, env)

        un.op match
          case BVNOT => bvnot(arg)
          case BVNEG => bvneg(arg)

      case _ => valuelattice.top

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element =
    n match
      case r: CfgCommandNode =>
        r.data match
          // assignments
          case la: LocalAssign => 
            s + (la.lhs -> eval(la.rhs, s))
          // all others: like no-ops
          case _ => s
      case _ => s

/** Base class for value analysis with simple (non-lifted) lattice.
  */
abstract class SimpleValueAnalysis(val cfg: ProgramCfg) extends FlowSensitiveAnalysis(true) with ValueAnalysisMisc:

  /** The analysis lattice.
    */
  val lattice: MapLattice[CfgNode, statelattice.type] = MapLattice(statelattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element = localTransfer(n, s)

/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
  */
abstract class ValueAnalysisWorklistSolver[L <: LatticeWithOps](
    cfg: ProgramCfg,
    val valuelattice: L
) extends SimpleValueAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies