package analysis

import astnodes._
import analysis.solvers._

/** Trait for program analyses.
  *
  * @tparam R
  *   the type of the analysis result
  */
trait Analysis[+R]:

  /** Performs the analysis and returns the result.
    */
  def analyze(): R

/** A flow-sensitive analysis.
  * @param stateAfterNode
  *   true if the abstract state of a CFG node represents the program point <em>after</em> the node, false if represents
  *   the program point <em>before</em> the node (used when outputting analysis results)
  */
abstract class FlowSensitiveAnalysis(val stateAfterNode: Boolean) extends Analysis[Any]

trait ValueAnalysisMisc:

  val cfg: ProgramCfg

  /** The lattice of abstract values.
    */
  val valuelattice: LatticeWithOps

  /** Set of declared variables, used by `statelattice`.
    */
  val declaredVars: Set[LocalVar] = cfg.prog.functions.flatMap(_.blocks).flatMap(_.locals).toSet

  /** The lattice of abstract states.
    */
  val statelattice: MapLattice[LocalVar, valuelattice.type] = new MapLattice(valuelattice)

  /** Default implementation of eval.
    */
  def eval(exp: Expr, env: statelattice.Element): valuelattice.Element =
    import valuelattice._
    exp match
      case id: LocalVar => env(id)
      case n: Literal   => literal(n)
      case se: SignedExtend => 
        
    case use: UnsignedExtend => 

      case e: Extract =>
        val body = eval(e.body, env)
        extract(e.high, e.low, body)
      case bin: BinOp =>
        val left = eval(bin.lhs, env)
        val right = eval(bin.rhs, env)

        bin.operator match
          case PLUS    => plus(left, right)
          case MINUS   => minus(left, right)
          case TIMES   => times(left, right)
          case DIVIDE  => divide(left, right)
          case SDIVIDE => sdivide(left, right)
          case AND     => and(left, right)
          case OR      => or(left, right)
          case XOR     => xor(left, right)
          case MOD     => mod(left, right)
          case SMOD    => smod(left, right)
          case LSHIFT  => lshift(left, right)
          case RSHIFT  => rshift(left, right)
          case ARSHIFT => arshift(left, right)
          case EQ      => eqq(left, right)
          case NEQ     => neq(left, right)
          case LT      => lt(left, right)
          case LE      => le(left, right)
          case SLT     => slt(left, right)
          case SLE     => sle(left, right)

      case un: UnOp =>
        val arg = eval(un.exp, env)

        un.operator match
          case NEG => neg(arg)
          case NOT => not(arg)

      case _ => valuelattice.top

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element =
    n match
      case r: CfgStatementNode =>
        r.data match
          // assignments
          case LocalAssign(lhs: LocalVar, rhs: Expr) => s + (lhs -> eval(rhs, s))

          // all others: like no-ops
          case _ => s
      case _ => s

/** Base class for value analysis with simple (non-lifted) lattice.
  */
abstract class SimpleValueAnalysis(val cfg: ProgramCfg) extends FlowSensitiveAnalysis(true) with ValueAnalysisMisc:

  /** The analysis lattice.
    */
  val lattice: MapLattice[CfgNode, statelattice.type] = new MapLattice(statelattice)

  val domain: Set[CfgNode] = cfg.nodes

  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
    */
  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element = localTransfer(n, s)

/** Intraprocedural value analysis that uses [[SimpleWorklistFixpointSolver]].
  */
abstract class IntraprocValueAnalysisWorklistSolver[L <: LatticeWithOps](
    cfg: IntraproceduralProgramCfg,
    val valuelattice: L
) extends SimpleValueAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies

object ConstantPropagationAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
    */
  class WorklistSolver(cfg: IntraproceduralProgramCfg)
      extends IntraprocValueAnalysisWorklistSolver(cfg, ConstantPropagationLattice)
