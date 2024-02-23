package analysis
import ir.*
import analysis.solvers.*

trait ILValueAnalysisMisc:
  val valuelattice: ConstantPropagationLattice = ConstantPropagationLattice()
  val statelattice: MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice] = MapLattice(valuelattice)

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

  private final val callerPreservedRegisters = Set("R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
    "R11", "R12", "R13", "R14", "R15", "R16", "R17", "R18", "R30")

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CFGPosition, s: statelattice.Element): statelattice.Element =
    n match
      case la: Assign =>
        s + (la.lhs -> eval(la.rhs, s))
      case c: Call => s ++ callerPreservedRegisters.filter(reg => s.keys.exists(_.name == reg)).map(n => Register(n, 64) -> statelattice.sublattice.top).toMap
      case _ => s



object IRSimpleValueAnalysis:

  class Solver(prog: Program) extends ILValueAnalysisMisc
    with IRIntraproceduralForwardDependencies 
    with Analysis[Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]]
    with SimplePushDownWorklistFixpointSolver[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]], MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice]]
    :
      /* Worklist initial set */
      //override val lattice: MapLattice[CFGPosition, statelattice.type] = MapLattice(statelattice)
      override val lattice: MapLattice[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]], MapLattice[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice]] = MapLattice(statelattice)

      override val domain : Set[CFGPosition] = computeDomain(IntraProcIRCursor, prog.procedures).toSet
      def transfer(n: CFGPosition, s: statelattice.Element): statelattice.Element = localTransfer(n, s)
