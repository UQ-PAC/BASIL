package analysis

import boogie.{BExpr, TrueBLiteral}
import ir.*
import ir.transforms.{SCCCallgraphWorklistSolver, Transform, reversePostOrder, worklistSolver}
import util.ProcedureSummariesLogger as Logger

case class Condition(pred: Predicate, label: Option[String] = None)

case class ProcedureSummary(requires: List[Condition], ensures: List[Condition])

class InterprocSummaryGenerator(program: Program, parameterForm: Boolean = false) {
  import Predicate.*

  val relevantGlobals: Set[Variable] = if parameterForm then Set() else 0.to(31).map { n => Register(s"R$n", 64) }.toSet

  val varDepsSummaries = {
    util.StaticAnalysisLogger.debug("[!] Variable dependency summaries")
    val scc = stronglyConnectedComponents(CallGraph, List(program.mainProcedure))
    VariableDependencyAnalysis(program, scc, parameterForm).analyze()
  }

  private def getDependencies(procedure: Procedure): Map[Variable, Set[Variable]] = {
    varDepsSummaries.getOrElse(procedure, Map()).flatMap { (v, ls) =>
      ls match {
        case LatticeSet.FiniteSet(s) => Some((v, s))
        case LatticeSet.Bottom() => Some((v, Set()))
        case _ => None
      }
    }
  }

  def transfer(map: Procedure => ProcedureSummary, p: ProcedureSummary, procedure: Procedure): ProcedureSummary = {
    if procedure.blocks.isEmpty then return ProcedureSummary(List(), List())

    val curRequires = p.requires
    val curEnsures = p.ensures

    /* Gamma domain with reachability conditions
     *
     * Computes requires conditions for branch conditions to have low gammas.
     * At each branch condition, we compute a necessary gamma dependency for the
     * condition to be low, and a reachability predicate for the branch condition
     * (think nested in statements).
     */
    Logger.debug(s"Generating gamma with reachability preconditions for $procedure")
    val initialMustGammaDeps = LatticeMap.bottomMap(
      (relevantGlobals ++ procedure.formalInParam).map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap
    )
    val mustGammaDomain = MustGammaDomain(initialMustGammaDeps)
    val reachabilityDomain = ReachabilityConditions()
    reversePostOrder(procedure)
    val (_, mustGammaResults) = worklistSolver(mustGammaDomain).solveProc(procedure, false)
    val (beforeReachability, afterReachability) = worklistSolver(reachabilityDomain).solveProc(procedure, false)

    val mustGammasWithConditions = procedure.blocks
      .flatMap(b => {
        b.statements.flatMap(s => {
          s match {
            case a: Assume if a.checkSecurity => {
              val condition = reachabilityDomain.toPred(beforeReachability(a.parent))

              /* Compute a set of variables that this branch condition definitely depends on.
               * The MustGammaDomain gives as an invariant that Gamma_x => Join(S_u) for each
               * variable x in the branch condition. We require that the branch condition is
               * low, so Low => Join(S_u). Since S_u is a set of input variables, we require
               * at procedure entry that those variables are low.
               */
              a.body.variables
                .foldLeft(Some(Set()): Option[Set[GammaTerm]]) { (curSet, v) =>
                  for {
                    s <- curSet
                    r <- mustGammaResults(a.parent)(v).tryToSet
                  } yield (s ++ r.map(GammaTerm.Var(_)))
                }
                .map { gammas =>
                  {
                    Predicate
                      .bop(BoolIMPLIES, condition, Predicate.gammaLeq(GammaTerm.Join(gammas), GammaTerm.Low))
                      .simplify
                  }
                }
            }
            case _ => None
          }
        })
      })
      .map(_.simplify)
      .toList
      .map(Condition(_, Some("Reachability conditions")))

    // Predicate domain / mini wp
    Logger.debug(s"Generating mini wp preconditions for $procedure")
    val wpDomain = WpDualDomain(map)
    val (wpDomainResults, _) = worklistSolver(wpDomain).solveProc(procedure, true)

    val wp = procedure.entryBlock
      .flatMap(b => wpDomainResults.get(b))
      .toList
      .flatMap(p => wpDomain.toPred(p).simplify.split)
      .map(Condition(_, Some("Weakest precondition")))

    val requires = (curRequires ++ mustGammasWithConditions ++ wp).filter(_ != TrueBLiteral).distinct

    /* Forwards variable dependency
     *
     * Interprocedurally computes an overapproximation of, for each output variable,
     * the set of input variables whose join of gammas is the gamma of the output.
     * Since this is an overapproximation, we have that Gamma_out <= Join(S_o)
     */
    Logger.debug(s"Generating forwards dependency gamma postconditions for $procedure")
    val inVars = relevantGlobals ++ procedure.formalInParam
    val outVars = (relevantGlobals ++ procedure.formalOutParam).filter { v =>
      v match {
        case v: Global => procedure.modifies.contains(v)
        case v: LocalVar => procedure.formalOutParam.contains(v)
      }
    }

    val dependencyMap = getDependencies(procedure).filter { (variable, _) =>
      outVars.contains(variable)
    }

    val dependencyPreds = dependencyMap.toList
      .map { (variable, dependencies) =>
        {
          Predicate.gammaLeq(GammaTerm.Var(variable), GammaTerm.Join(dependencies.map(GammaTerm.OldVar(_)))).simplify
        }
      }
      .map(Condition(_, Some("variable dependency analysis")))

    /* Abstract interpretation predicates
     *
     * If a forwards abstract domain that is a may analysis can encode its state as a
     * predicate, it can be used to generate ensures clauses (by ensuring the state's
     * predicate at the end of the procedure).
     */
    Logger.debug(s"Generating forward abstract interpretation postconditions for $procedure")
    val returnBlock = IRWalk.lastInProc(procedure).map(_.parent)

    /* By computing the disjunctive completion of a product domain of a numerical and
     * gamma domain, we get some path sensitivity for gamma dependency. If the verifier
     * knows that a numerical invariant is not held, it can know that one of the disjuncts
     * will not hold.
     */
    val initialMayGammaDeps =
      LatticeMap.TopMap((relevantGlobals ++ procedure.formalInParam).map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val predAbsIntDomain = PredBoundedDisjunctiveCompletion(
      PredProductDomain(DoubleIntervalDomain(Some(procedure)), MayGammaDomain(initialMayGammaDeps)),
      10
    )
    val (beforeAbsInt, afterAbsInt) = worklistSolver(predAbsIntDomain, widen = true).solveProc(procedure)

    val absIntPreds = returnBlock
      .map(b =>
        afterAbsInt
          .get(b)
          .map(l => predAbsIntDomain.toPred(l).filterOut(outVars ++ inVars, Predicate.True).split.map(_.simplify))
      )
      .flatten
      .toList
      .flatten
      .map(Condition(_, Some("numerical analysis")))

    val ensures = (curEnsures ++ dependencyPreds ++ absIntPreds).filter(_ != True).distinct

    ProcedureSummary(requires, ensures)
  }

  def init(p: Procedure): ProcedureSummary = ProcedureSummary(List(), List())
}

/**
 * Generates summaries (requires and ensures clauses) for procedures that necessarily must hold (assuming a correct implementation).
 * This helps because the verifier cannot make any assumptions about procedures with no summaries.
 */
class SummaryGenerator(program: Program, parameterForm: Boolean = false) {
  val interprocGenerator = InterprocSummaryGenerator(program, parameterForm)
  val interprocResults: Map[Procedure, ProcedureSummary] =
    SCCCallgraphWorklistSolver(interprocGenerator.transfer, interprocGenerator.init).solve(program)

  /**
   * Generate requires clauses for a procedure.
   */
  def generateRequires(procedure: Procedure): List[BExpr] = {
    interprocResults
      .getOrElse(procedure, interprocGenerator.init(procedure))
      .requires
      .map(c => {
        val expr = c.pred.toBoogie.simplify
        expr.label = c.label
        expr
      })
      .filter(_ != TrueBLiteral)
  }

  /**
   * Generate ensures clauses for a procedure.
   */
  def generateEnsures(procedure: Procedure): List[BExpr] = {
    interprocResults
      .getOrElse(procedure, interprocGenerator.init(procedure))
      .ensures
      .map(c => {
        val expr = c.pred.toBoogie.simplify
        expr.label = c.label
        expr
      })
      .filter(_ != TrueBLiteral)
  }
}

def getGenerateProcedureSummariesTransform(simplified: Boolean): Transform =
  Transform(
    "GenerateProcedureSummaries",
    (ctx, man) => {
      val prog = ctx.program
      // Need to know modifies clauses to generate summaries, but this is probably out of place (fixme)
      val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
      prog.setModifies(specModifies)

      val summaryGenerator = SummaryGenerator(prog, simplified)
      for procedure <- prog.procedures if procedure != prog.mainProcedure do
        procedure.requires = summaryGenerator.generateRequires(procedure)
        procedure.ensures = summaryGenerator.generateEnsures(procedure)

      man.ClobberAll
    },
    notice = "Generating Procedure Summaries"
  )
