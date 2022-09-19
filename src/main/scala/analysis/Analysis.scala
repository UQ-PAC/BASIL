//package analysis
//
///** Trait for program analyses.
//  *
//  * @tparam R
//  *   the type of the analysis result
//  */
//trait Analysis[+R]:
//
//  /** Performs the analysis and returns the result.
//    */
//  def analyze(): R
//
//trait ValueAnalysisMisc {
//
//  implicit val declData: DeclarationData
//
//  val cfg: ProgramCfg
//
//  /** The lattice of abstract values.
//    */
//  val valuelattice: LatticeWithOps
//
//  /** Set of declared variables, used by `statelattice`.
//    */
//  val declaredVars: Set[ADeclaration] = cfg.nodes.flatMap(_.declaredVarsAndParams)
//
//  /** The lattice of abstract states.
//    */
//  val statelattice: MapLattice[ADeclaration, valuelattice.type] = new MapLattice(valuelattice)
//
//  /** Default implementation of eval.
//    */
//  def eval(exp: AExpr, env: statelattice.Element)(implicit declData: DeclarationData): valuelattice.Element = {
//    import valuelattice._
//    exp match {
//      case id: AIdentifier => env(id)
//      case n: ANumber      => num(n.value)
//      case bin: ABinaryOp =>
//        val left = eval(bin.left, env)
//        val right = eval(bin.right, env)
//        bin.operator match {
//          case Eqq       => eqq(left, right)
//          case GreatThan => gt(left, right)
//          case Divide    => div(left, right)
//          case Minus     => minus(left, right)
//          case Plus      => plus(left, right)
//          case Times     => times(left, right)
//          case _         => ???
//        }
//      case _: AInput => valuelattice.top
//      case _         => ???
//    }
//  }
//
//  /** Transfer function for state lattice elements.
//    */
//  def localTransfer(n: CfgNode, s: statelattice.Element): statelattice.Element = {
//    NoPointers.assertContainsNode(n.data)
//    NoCalls.assertContainsNode(n.data)
//    NoRecords.assertContainsNode(n.data)
//    n match {
//      case r: CfgStmtNode =>
//        r.data match {
//          // var declarations
//          case varr: AVarStmt => ??? //<--- Complete here
//
//          // assignments
//          case AAssignStmt(id: AIdentifier, right, _) => ??? //<--- Complete here
//
//          // all others: like no-ops
//          case _ => s
//        }
//      case _ => s
//    }
//  }
//}
//
///** Base class for value analysis with simple (non-lifted) lattice.
//  */
//abstract class SimpleValueAnalysis(val cfg: ProgramCfg)(implicit val decl: DeclarationData)
//    extends FlowSensitiveAnalysis(true)
//    with ValueAnalysisMisc {
//
//  /** The analysis lattice.
//    */
//  val lattice: MapLattice[CfgNode, statelattice.type] = new MapLattice(statelattice)
//
//  val domain: Set[CfgNode] = cfg.nodes
//
//  /** Transfer function for state lattice elements. (Same as `localTransfer` for simple value analysis.)
//    */
//  def transfer(n: CfgNode, s: statelattice.Element): statelattice.Element = localTransfer(n, s)
//}
//
///** Intraprocedural value analysis that uses [[tip.solvers.SimpleFixpointSolver]].
//  */
//abstract class IntraprocValueAnalysisSimpleSolver[L <: LatticeWithOps](
//    cfg: IntraproceduralProgramCfg,
//    val valuelattice: L
//)(implicit
//    override val declData: DeclarationData
//) extends SimpleValueAnalysis(cfg)
//    with SimpleMapLatticeFixpointSolver[CfgNode]
//    with ForwardDependencies
