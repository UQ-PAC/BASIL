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

///** Base class for value analysis with simple (non-lifted) lattice.
//  */
//abstract class SimpleValueAnalysis(val cfg: ICFG) extends FlowSensitiveAnalysis(true) with ValueAnalysisMisc:
//
//  /** The analysis lattice.
//    */
//  val lattice: MapLattice[CfgNode, statelattice.type] = MapLattice(statelattice)
//
//  //  val domain: Set[CfgNode] = cfg.nodes.toSet
//
//  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
//    */
//  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element = localTransfer(n, s)
//
//abstract class ValueAnalysisWorklistSolver[L <: LatticeWithOps](
//    cfg: ICFG,
//    val valuelattice: L
//) extends SimpleValueAnalysis(cfg)
//    with SimplePushDownWorklistFixpointSolver[CfgNode]
//    with ForwardDependencies




//trait WorklistFixPointSolver[NodeType, L <: Lattice, +W <: Worklist[NodeType]]() {
//  val worklist: W
//  val lattice: MapLattice[NodeType, L]
//  val dependencies: Dependencies[NodeType]
//
//  def transfer(n: NodeType, s: lattice.sublattice.Element): lattice.sublattice.Element
//
//  def funsub(n: NodeType, x: lattice.Element): lattice.sublattice.Element =
//    transfer(n, join(n, x))
//
//
//  def join(n: NodeType, o: lattice.Element): lattice.sublattice.Element =
//    val states = dependencies.indep(n).map(o(_))
//    states.foldLeft(lattice.sublattice.bottom)((acc, pred) => lattice.sublattice.lub(acc, pred))
//}
//
//object constprop extends WorklistFixPointSolver[IntraProcIRCursor.Node, ConstantPropagationLattice, ListSetWorklist[IntraProcIRCursor.Node]] {
//
//
//}


