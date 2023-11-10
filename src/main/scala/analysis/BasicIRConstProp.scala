package analysis
import ir.*

import analysis.solvers.*

trait ILValueAnalysisMisc:
  val valuelattice: LatticeWithOps
  val statelattice: MapLattice[Variable, valuelattice.type] = new MapLattice(valuelattice)

  def eval(exp: Expr, env: statelattice.Element): valuelattice.Element =
    import valuelattice._
    exp match
      case id: Variable   => env(id)
      case n: Literal     => literal(n)
      case ze: ZeroExtend => zero_extend(ze.extension, eval(ze.body, env))
      case se: SignExtend => sign_extend(se.extension, eval(se.body, env))
      case e: Extract     => extract(e.end, e.start, eval(e.body, env))
      case bin: BinaryExpr =>
        val left = eval(bin.arg1, env)
        val right = eval(bin.arg2, env)
        bin.op match
          case BVADD  => bvadd(left, right)
          case BVSUB  => bvsub(left, right)
          case BVMUL  => bvmul(left, right)
          case BVUDIV => bvudiv(left, right)
          case BVSDIV => bvsdiv(left, right)
          case BVSREM => bvsrem(left, right)
          case BVUREM => bvurem(left, right)
          case BVSMOD => bvsmod(left, right)
          case BVAND  => bvand(left, right)
          case BVOR   => bvor(left, right)
          case BVXOR  => bvxor(left, right)
          case BVNAND => bvnand(left, right)
          case BVNOR  => bvnor(left, right)
          case BVXNOR => bvxnor(left, right)
          case BVSHL  => bvshl(left, right)
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
          case BVNEQ    => bvneq(left, right)
          case BVEQ     => bveq(left, right)

      case un: UnaryExpr =>
        val arg = eval(un.arg, env)

        un.op match
          case BVNOT => bvnot(arg)
          case BVNEG => bvneg(arg)

      case _ => valuelattice.top


  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: IntraProcIRCursor.Node, s: statelattice.Element): statelattice.Element =
    n match
      case la: LocalAssign =>
        s + (la.lhs -> eval(la.rhs, s))
      case _ => s


type IRNode = IntraProcIRCursor.Node

object IRSimpleValueAnalysis:
  class Solver[+L <: LatticeWithOps](prog: Program, val valuelattice: L) extends FlowSensitiveAnalysis(true)
    with IntraProcDependencies
    with Dependencies[IRNode](true)
    with ILValueAnalysisMisc
    with SimplePushDownWorklistFixpointSolver[IRNode]
    :
      /* Worklist initial set */
      override val lattice: MapLattice[IRNode, statelattice.type] = MapLattice(statelattice)

      override val domain : Set[IRNode] = computeDomain(prog).toSet
      def transfer(n: IRNode, s: statelattice.Element): statelattice.Element = localTransfer(n, s)
