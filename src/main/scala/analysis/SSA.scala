package analysis

import ir.*
import analysis.solvers.SimpleWorklistFixpointSolver
import util.Logger

type SSATuple =
  TupleLattice[MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]], MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]], Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]]

private var ssaCount: Int = 0

private def nextSSACount() = {
  ssaCount += 1
  ssaCount
}

trait SSA(program: Program, writesTo: Map[Procedure, Set[Register]], RNAResult: Map[CFGPosition, Set[Variable]]) {

  private val tupleLattice: TupleLattice[MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]], MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]], Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]] =
    TupleLattice(
      MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]](FlatLatticeWithDefault[Int](nextSSACount)),
      MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]](FlatLatticeWithDefault[Int](nextSSACount))
    )

  val lattice: MapLattice[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]), SSATuple] = MapLattice(
    tupleLattice
  )

  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])): (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]) =
    localTransfer(n, s)

  def localTransfer(
    n: CFGPosition,
    s: (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])
  ): (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]) = n match {
    case cmd: Command =>
      eval(cmd, s)
    case procedure: Procedure =>
      // get params from RNA
      // for every param assign a fresh SSA variable
      val params = RNAResult(procedure).map(v => (v, FlatEl(nextSSACount()))) // TODO: more advanced handling of params required
      (params.toMap, Map.empty)
    case _ => s
  }

  private def transformUses(vars: Set[Variable], s: (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])): (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]) = {
    vars.foldLeft((s(0), Map.empty[Variable, FlatElement[Int]])) {
      case ((state, acc), v) =>
        (state, acc + (v -> state(v)))
    }
  }

  def eval(cmd: Command, s: (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])):
    (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]) = cmd match {
    case assign: LocalAssign =>
      val rhs = assign.rhs.variables
      val lhs = assign.lhs
      val rhsUseDefs = rhs.foldLeft(Map.empty[Variable, FlatElement[Int]]) { (acc, v) =>
        acc + (v -> s(0)(v))
      }
      (s(0) + (lhs -> FlatEl(nextSSACount())), rhsUseDefs)
    case load: MemoryLoad =>
      val rhs = load.index.variables
      val lhs = load.lhs
      val rhsUseDefs = rhs.foldLeft(Map.empty[Variable, FlatElement[Int]]) { (acc, v) =>
        acc + (v -> s(0)(v))
      }
      (s(0) + (lhs -> FlatEl(nextSSACount())), rhsUseDefs)
    case assert: Assert =>
      transformUses(assert.body.variables, s)
    case store: MemoryStore =>
      transformUses(store.index.variables ++ store.value.variables, s)
    case assume: Assume =>
      transformUses(assume.body.variables, s)
    case indirectCall: IndirectCall =>
      transformUses(indirectCall.target.variables, s)
    case directCall: DirectCall =>
      writesTo.get(directCall.target) match {
        case Some(registers) =>
          val result = registers.foldLeft(Map[Variable, FlatElement[Int]]()) { (m, register) =>
            m + (register -> FlatEl(nextSSACount()))
          }
          (s(0) ++ result, s(1))
        case None =>
          s
      }
    case _ => s
  }
}

class IntraprocSSASolver(program: Program, writesTo: Map[Procedure, Set[Register]], RNAResult: Map[CFGPosition, Set[Variable]])
  extends SSA(program, writesTo, RNAResult)
    with IRIntraproceduralForwardDependencies
    with SimpleWorklistFixpointSolver[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]), SSATuple]

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
def getCommonDefinitionVariableRenaming(p: Program, writesTo: Map[Procedure, Set[Register]]) : Map[CFGPosition, (Map[Variable, FlatEl[Int]], Map[Variable, FlatEl[Int]])] = {

  val RNASolver = RNAAnalysisSolver(p, false)
  val RNAResult: Map[CFGPosition, Set[Variable]] = RNASolver.analyze()

  val init = (Map[CFGPosition, Map[Variable, Int]](), Map[Variable, Int]())

  // hack to get unreachable nodes in the visit order
  val unreachable = p.toSet.diff(p.preOrderIterator.toSet)
  val order = p.preOrderIterator.toList
  val toVisit = (p.procedures.toList ++ order ++ unreachable)

  // pass 1; assign indexes to each definition
  val definitionToIndex : Map[CFGPosition, Map[Variable, Int]]= toVisit.foldLeft(init)((acc: (Map[CFGPosition, Map[Variable, Int]], Map[Variable, Int]) , s: CFGPosition) => {

    val (st, x) = acc
    val nx = s match {
      case a: Assign => x.updated(a.lhs, nextSSACount())
      case p : Procedure =>
        val params = RNAResult(p).map(v => (v, nextSSACount())).toMap
        Logger.debug(s"${p.name} ${params}")
        x ++ params
      case d: DirectCall => writesTo.get(d.target) match {
        case Some(registers) =>
          val result = registers.foldLeft(Map[Variable, Int]()) { (m, register) =>
            m + (register -> nextSSACount())
          }
          x ++ result
        case None =>
          x
        }
      case _: GoTo | _: MemoryStore | _: Assume | _: Assert | _: Block | _: NOP | _: IndirectCall | _: Unreachable | _: Return => x
    }
    (st.updated(s, nx), nx)
  })._1

  // pass 2: unify indexes over their uses using reaching definitions (intraprocedurally so as to not unify across calls)
  val reachingDefinitionsAnalysisSolver = IntraprocReachingDefinitionsAnalysisSolver(p)
  val reachingDefs = reachingDefinitionsAnalysisSolver.analyze()
  case class SSAVar(v: Int) extends analysis.solvers.Var[SSAVar]
  val ufsolver = analysis.solvers.UnionFindSolver[SSAVar]()


  def unifyUsesOf(v: Variable, atPos: CFGPosition) : Unit = {
    val d = getUse(v, atPos, reachingDefs)
    val vars = d.map(definitionToIndex(_)(v))
    for (v1 <- vars) {
      for (v2 <- vars) {
        if (v1 != v2) {
          ufsolver.unify(SSAVar(v1), SSAVar(v2))
        }
      }
    }
  }

  def unifyVarsUses(vs: Iterable[Variable], loc: CFGPosition) : Unit = {
    vs.foreach(unifyUsesOf(_, loc))
  }

  toVisit.foreach((s: CFGPosition) => s match {
    case a: LocalAssign => unifyVarsUses(a.rhs.variables, s)
    case l: MemoryLoad => unifyVarsUses(l.index.variables, l)
    case a: MemoryStore => unifyVarsUses(a.index.variables ++ a.value.variables, a)
    case a: Assert => unifyVarsUses(a.body.variables, a)
    case a: Assume => unifyVarsUses(a.body.variables, a)
    case a: IndirectCall => unifyVarsUses(a.target.variables, a)
    case b: Procedure => () /* formal params */
    case a: DirectCall => () /* actual params */
    case _: Return => () /* return params */
    case _: Block | _: Unreachable |  _:GoTo | _: NOP => ()
  })

  // extract result
  // Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
  //      loc ->        (defining (var -> index)            reaching (var -> index))

  def getUseInd(v: Variable, pos: CFGPosition) : Int = {
    val d = getUse(v, pos, reachingDefs).headOption.getOrElse({Logger.debug(s"${IRWalk.blockBegin(pos)}"); IRWalk.procedure(pos)})
    val ind = definitionToIndex(d)(v)
    ufsolver.find(SSAVar(ind)).asInstanceOf[SSAVar].v
  }

  val defuse = definitionToIndex.map((pos, defs) => {
    (pos: CFGPosition) -> {
      val definitions = defs.map((v, ind) => v -> FlatEl(ufsolver.find(SSAVar(ind)).asInstanceOf[SSAVar].v)).toMap
      val uses = pos match {
        case c: Command =>
          freeVarsPos(pos).map(v => v -> FlatEl(getUseInd(v, pos))).toMap
        case _ => Map()
      }
      (definitions, uses)
    }
  })

  defuse
}
