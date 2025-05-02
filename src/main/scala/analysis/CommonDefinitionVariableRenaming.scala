package analysis

import analysis.solvers.Var
import analysis.solvers.UnionFindSolver
import ir.*
import util.Logger

private var ssaCount: Int = 0

private def nextSSACount() = {
  ssaCount += 1
  ssaCount
}

/*
 *
 * This analysis attempts to create a variable renaming <name, integer> such that for a given (rhs) variable, all
 * variables with the same renaming have at least one reaching definition in common.
 *
 * This should mean that sequences of assignments such as:
 *
 * x = 1    renaming: (x -> 123)
 * x = 2    renaming: (x -> 124)
 * x = 3    renaming: (x -> 125)
 *
 * will get unique renamings which can be treated as distinct variables. On the other hand it does not provide any
 * guarantees of single-assignment or constantness of variables with in the presence of branching and loops.
 *
 * The returned value is of the form
 *
 * statement -> (renamings defined in statement, renamings used in statement)
 *
 * Pass 1: Map each definition to a number in reverse post order
 * Pass 2: Use reaching definitions results to unify definition indexes under their set of uses
 *
 */
def getCommonDefinitionVariableRenaming(
  p: Program,
  writesTo: Map[Procedure, Set[GlobalVar]]
): Map[CFGPosition, (Map[Variable, FlatEl[Int]], Map[Variable, FlatEl[Int]])] = {

  val RNASolver = RNAAnalysisSolver(p, false)
  val RNAResult: Map[CFGPosition, Set[Variable]] = RNASolver.analyze()

  // hack to get unreachable nodes in the visit order
  val unreachable = p.toSet.diff(p.preOrderIterator.toSet)
  val order = p.preOrderIterator.toList
  val toVisit = p.procedures.toList ++ order ++ unreachable

  val init = (Map[CFGPosition, Map[Variable, Int]](), Map[Variable, Int]())

  // pass 1; assign indexes to each definition
  val (definitionToIndex: Map[CFGPosition, Map[Variable, Int]], _) = toVisit.foldLeft(init) {
    (acc: (Map[CFGPosition, Map[Variable, Int]], Map[Variable, Int]), s: CFGPosition) =>

      val (st, x) = acc
      val nx = s match {
        case a: SingleAssign => x.updated(a.lhs, nextSSACount())
        case a: SimulAssign =>
          a.assignments.foldLeft(x) { case (s, (lhs, rhs)) =>
            s.updated(lhs, nextSSACount())
          }
        case p: Procedure =>
          val params = RNAResult(p).map(v => (v, nextSSACount())).toMap
          Logger.debug(s"${p.name} $params")
          x ++ params
        case d: DirectCall =>
          writesTo.get(d.target) match {
            case Some(registers) =>
              val result = registers.foldLeft(Map[Variable, Int]()) { (m, register) =>
                m + (register -> nextSSACount())
              }
              x ++ result
            case None =>
              x
          }
        case _: GoTo | _: MemoryStore | _: Assume | _: Assert | _: Block | _: NOP | _: IndirectCall | _: Unreachable |
            _: Return =>
          x
      }
      (st.updated(s, nx), nx)
  }

  // pass 2: unify indexes over their uses using reaching definitions (intraprocedurally so as to not unify across calls)
  val reachingDefinitionsAnalysisSolver = IntraprocReachingDefinitionsAnalysisSolver(p)
  val reachingDefs = reachingDefinitionsAnalysisSolver.analyze()

  case class SSAVar(v: Int) extends Var[SSAVar]

  val ufSolver = UnionFindSolver[SSAVar]()

  def unifyUsesOf(v: Variable, atPos: CFGPosition): Unit = {
    val d = getUse(v, atPos, reachingDefs)
    val vars = d.map(definitionToIndex(_)(v))
    for (v1 <- vars) {
      for (v2 <- vars) {
        if (v1 != v2) {
          ufSolver.unify(SSAVar(v1), SSAVar(v2))
        }
      }
    }
  }

  def unifyVarsUses(vs: Iterable[Variable], loc: CFGPosition): Unit = {
    vs.foreach(unifyUsesOf(_, loc))
  }

  toVisit.foreach {
    case a @ SimulAssign(assigns, _) => unifyVarsUses(assigns.flatMap(_._2.variables), a)
    case a: MemoryAssign => unifyVarsUses(a.rhs.variables, a)
    case l: MemoryLoad => unifyVarsUses(l.index.variables, l)
    case a: MemoryStore => unifyVarsUses(a.index.variables ++ a.value.variables, a)
    case a: Assert => unifyVarsUses(a.body.variables, a)
    case a: Assume => unifyVarsUses(a.body.variables, a)
    case a: IndirectCall => unifyVarsUses(a.target.variables, a)
    case b: Procedure => () /* formal params */
    case a: DirectCall => () /* actual params */
    case _: Return => () /* return params */
    case _: Block | _: Unreachable | _: GoTo | _: NOP => ()
  }

  // extract result
  // Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
  //      loc ->        (defining (var -> index)            reaching (var -> index))

  def getUseInd(v: Variable, pos: CFGPosition): Int = {
    val d = getUse(v, pos, reachingDefs).headOption.getOrElse {
      Logger.debug(s"${IRWalk.blockBegin(pos)}")
      IRWalk.procedure(pos)
    }
    val ind = definitionToIndex(d)(v)
    ufSolver.find(SSAVar(ind)).asInstanceOf[SSAVar].v
  }

  val defUse = definitionToIndex.map { (pos, defs) =>
    val definitions = defs.map((v, ind) => v -> FlatEl(ufSolver.find(SSAVar(ind)).asInstanceOf[SSAVar].v)).toMap
    val uses = pos match {
      case _: Command =>
        freeVarsPos(pos).map(v => v -> FlatEl(getUseInd(v, pos))).toMap
      case _ => Map()
    }
    pos -> (definitions, uses)
  }

  defUse
}
