package analysis

import util.ProcedureSummariesLogger as Logger

import analysis.*
import ir.*
import boogie.*
import boogie.SpecGlobal
import ir.transforms.{AbstractDomain, BottomUpCallgraphWorklistSolver, ProcAbstractDomain, reversePostOrder, worklistSolver}

// TODO annotated preconditions and postconditions
// TODO don't convert to boogie until translating, so that we can use conditions in analyses

class InterprocSummaryGenerator(program: Program, parameterForm: Boolean = false) {
  import Predicate.*

  val relevantGlobals: Set[Variable] = if parameterForm then Set() else 0.to(31).map { n => Register(s"R$n", 64) }.toSet

  val varDepsSummaries = {
      util.StaticAnalysisLogger.debug("[!] Variable dependency summaries")
      val scc = stronglyConnectedComponents(CallGraph, List(program.mainProcedure))
      VariableDependencyAnalysis(program, scc, parameterForm).analyze()
  }

  private def getDependencies(procedure: Procedure): Map[Variable, Set[Variable]] = {
    varDepsSummaries.getOrElse(procedure, Map()).flatMap {
      (v, ls) => ls match {
        case LatticeSet.FiniteSet(s) => Some((v, s))
        case LatticeSet.Bottom() => Some((v, Set()))
        case _ => None
      }
    }
  }

  // TODO move these to the predicate file
  /**
   * Determines whether the term contains only variables in vars
   */
  def varsAllIn(b: BVTerm, vars: Set[Variable]): Boolean = {
    import BVTerm.*
    b match {
      case Lit(x) => true
      case Var(v) => vars.contains(v)
      case OldVar(v) => true
      case Uop(op, x) => varsAllIn(x, vars)
      case Bop(op, x, y) => varsAllIn(x, vars) && varsAllIn(y, vars)
      case Repeat(repeats, body) => varsAllIn(body, vars)
      case Extract(end, start, body) => varsAllIn(body, vars)
      case ZeroExtend(extension, body) => varsAllIn(body, vars)
      case SignExtend(extension, body) => varsAllIn(body, vars)
    }
  }

  /**
   * Determines whether the term contains only variables in vars
   */
  def varsAllIn(g: GammaTerm, vars: Set[Variable]): Boolean = {
    import GammaTerm.*
    g match {
      case Lit(x) => true
      case Var(v) => vars.contains(v)
      case OldVar(v) => true
      case Uop(op, x) => varsAllIn(x, vars)
      case Join(s) => s.forall(varsAllIn(_, vars))
    }
  }

  /**
   * Removes all parts of the predicate containing variables not in vars (replacing the subexpression with default).
   */
  private def filterPred(p: Predicate, vars: Set[Variable], default: Predicate): Predicate = {
    import Predicate.*
    p match {
      case Lit(x) => p
      case Not(x) => not(filterPred(x, vars, default))
      case Conj(s) => Conj(s.map(filterPred(_, vars, default)))
      case Disj(s) => Disj(s.map(filterPred(_, vars, default)))
      case BVCmp(op, x, y) => if varsAllIn(x, vars) && varsAllIn(y, vars) then p else default
      case GammaCmp(op, x, y) => if varsAllIn(x, vars) && varsAllIn(y, vars) then p else default
    }
  }


  def transfer(map: Procedure => (List[Predicate], List[Predicate]), p: (List[Predicate], List[Predicate]), procedure: Procedure): (List[Predicate], List[Predicate]) = {
    if procedure.blocks.isEmpty then return (List(), List())

    val (curRequires, curEnsures) = p

    /* Gamma domain with reachability conditions
     *
     * Computes requires conditions for branch conditions to have low gammas.
     * At each branch condition, we compute a necessary gamma dependency for the
     * condition to be low, and a reachability predicate for the branch condition
     * (think nested in statements).
     */
    Logger.debug(s"Generating gamma with reachability preconditions for $procedure")
    val initialMustGammaDeps = LatticeMap.BottomMap((relevantGlobals ++ procedure.formalInParam).map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val mustGammaDomain = MustGammaDomain(initialMustGammaDeps)
    val reachabilityDomain = ReachabilityConditions()
    reversePostOrder(procedure)
    val (_, mustGammaResults) = worklistSolver(mustGammaDomain).solveProc(procedure, false)
    val (beforeReachability, afterReachability) = worklistSolver(reachabilityDomain).solveProc(procedure, false)

    val mustGammasWithConditions = procedure.blocks.flatMap(b => {
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
            a.body.variables.foldLeft(Some(Set()): Option[Set[GammaTerm]]) {
              (curSet, v) => for {
                s <- curSet
                r <- mustGammaResults(a.parent)(v).tryToSet
              } yield (s ++ r.map(GammaTerm.Var(_)))
            }.map {
              gammas => {
                Predicate.bop(
                  BoolIMPLIES,
                  condition,
                  Predicate.gammaLeq(GammaTerm.Join(gammas), GammaTerm.Low)
                ).simplify
              }
            }
          }
          case _ => None
        }
      })
    }).map(_.simplify).toList

    // Predicate domain / mini wp
    Logger.debug(s"Generating mini wp preconditions for $procedure")
    val wpDomain = PredicateDomain(map)
    val (wpDomainResults, _) = worklistSolver(wpDomain).solveProc(procedure, true)

    val wpThing = procedure.entryBlock.flatMap(b => wpDomainResults.get(b))

    val requires = (curRequires ++ mustGammasWithConditions ++ wpThing).filter(_ != TrueBLiteral).distinct

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

    val dependencyPreds = dependencyMap.toList.map {
      (variable, dependencies) => {
        Predicate.gammaLeq(GammaTerm.Var(variable), GammaTerm.Join(dependencies.map(GammaTerm.OldVar(_))))
          .simplify
      }
    }

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
    val initialMayGammaDeps = LatticeMap.TopMap((relevantGlobals ++ procedure.formalInParam).map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val predAbsIntDomain = PredBoundedDisjunctiveCompletion(PredProductDomain(DoubleIntervalDomain(Some(procedure)), MayGammaDomain(initialMayGammaDeps)), 10)
    val (beforeAbsInt, afterAbsInt) = worklistSolver(predAbsIntDomain).solveProc(procedure)

    val absIntPreds = returnBlock.map(b => afterAbsInt.get(b).map(l => filterPred(predAbsIntDomain.toPred(l), outVars ++ inVars, Predicate.True).split.map(_.simplify))).flatten.toList.flatten

    val ensures = (curEnsures ++ dependencyPreds ++ absIntPreds).filter(_ != True).distinct

    (requires, ensures)
  }

  def init(p: Procedure): (List[Predicate], List[Predicate]) = (List(), List())
}

/**
 * Generates summaries (requires and ensures clauses) for procedures that necessarily must hold (assuming a correct implementation).
 * This helps because the verifier cannot make any assumptions about procedures with no summaries.
 */
class SummaryGenerator(
  program: Program,
  parameterForm: Boolean = false,
) {
  val interprocGenerator = InterprocSummaryGenerator(program, parameterForm)
  val interprocResults: Map[Procedure, (List[Predicate], List[Predicate])] = BottomUpCallgraphWorklistSolver(interprocGenerator.transfer, interprocGenerator.init).solve(program)

  /**
   * Generate requires clauses for a procedure.
   */
  def generateRequires(procedure: Procedure): List[BExpr] = {
    interprocResults.getOrElse(procedure, (List(), List()))._1.map(_.toBoogie.simplify).filter(_ != TrueBLiteral)
  }

  /**
   * Generate ensures clauses for a procedure.
   */
  def generateEnsures(procedure: Procedure): List[BExpr] = {
    interprocResults.getOrElse(procedure, (List(), List()))._2.map(_.toBoogie.simplify).filter(_ != TrueBLiteral)
  }
}
