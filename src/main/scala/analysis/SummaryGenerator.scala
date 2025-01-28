package analysis

import util.ProcedureSummariesLogger as Logger

import analysis.*
import ir.*
import boogie.*
import boogie.SpecGlobal
import ir.transforms.{AbstractDomain, reversePostOrder, worklistSolver}

/**
 * Generates summaries (requires and ensures clauses) for procedures that necessarily must hold (assuming a correct implementation).
 * This helps because the verifier cannot make any assumptions about procedures with no summaries.
 */
class SummaryGenerator(
    program: Program,
    varDepsSummaries: Map[Procedure, Map[Variable, LatticeSet[Variable]]],
) {
  // registers R19 to R29 and R31 must be preserved by a procedure call, so we only need summaries for R0-R18 and R30
  // TODO support R30? the analysis currently ignores it?
  val variables: Set[Variable] = 0.to(18).map { n =>
    Register(s"R$n", 64)
  }.toSet

  /**
   * Get a map of variables to variables which have tainted it in the procedure.
   */
  private def getTainters(procedure: Procedure): Map[Variable, Set[Variable]] = {
    varDepsSummaries.getOrElse(procedure, Map()).flatMap {
      (v, ls) => ls match {
        case LatticeSet.FiniteSet(s) => Some((v, s))
        case LatticeSet.Bottom() => Some((v, Set()))
        case _ => None
      }
    }
  }

  /**
   * Gets the set of gammas stored in a VarGammaMap, if possible
   */
  private def relevantGammas(gammaMap: VarGammaMap, v: Variable): Option[Set[Variable]] = {
    gammaMap(v) match {
      case LatticeSet.Top() => None // We can't know all of the variables, so we soundly say nothing
      case LatticeSet.Bottom() => Some(Set())
      case LatticeSet.FiniteSet(s) => Some(s)
      case LatticeSet.DiffSet(_) => None
    }
  }

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
      case Uop(op, x) => Uop(op, filterPred(x, vars, default))
      case Bop(op, x, y) => Bop(op, filterPred(x, vars, default), filterPred(y, vars, default))
      case BVCmp(op, x, y) => if varsAllIn(x, vars) && varsAllIn(y, vars) then p else default
      case GammaCmp(op, x, y) => if varsAllIn(x, vars) && varsAllIn(y, vars) then p else default
    }
  }

  /**
   * Generate requires clauses for a procedure. Currently we can generate the requirement that the variables that
   * influence the gamma of the first branch condition must be low.
   */
  def generateRequires(procedure: Procedure): List[BExpr] = {
    if procedure.blocks.isEmpty then return List()

    val predDomain = PredicateDomain()
    val initialState = LatticeMap.TopMap((variables.collect(_ match {
      case v: Variable => v
    }) ++ procedure.formalInParam).map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    val mustGammaDomain = MustGammaDomain(initialState)
    reversePostOrder(procedure)
    val (_, mustGammaResults) = worklistSolver(mustGammaDomain).solveProc(procedure, false)
    val (before, after) = worklistSolver(ReachabilityConditions()).solveProc(procedure, false)
    val (predDomainResults, _) = worklistSolver(predDomain).solveProc(procedure, true)
    Logger.debug(predDomainResults.map((a, b) => (a, eval.simplifyExprFixpoint(b.simplify.toBasil.get)._1.toBoogie)))

    val mustGammasWithConditions = procedure.blocks.flatMap(b => {
      b.statements.flatMap(s => {
        s match {
          case a: Assume if a.checkSecurity => {
            val condition = a.parent.prevBlocks.foldLeft(TrueBLiteral: BExpr)((p, b) =>
                BinaryBExpr(BoolAND, p, ReachabilityConditions().toPred(before(b)).toBoogie.simplify)).simplify
            a.body.variables.foldLeft(Some(Set()): Option[Set[BExpr]]) {
              (s, v) => {
                relevantGammas(mustGammaResults(a.parent), v).flatMap(r => s.map(s => s ++ r.map(_.toGamma)))
              }
            }.flatMap {
              gammas => {
                (if (gammas.size > 1) {
                  Some(gammas.tail.foldLeft(gammas.head)((ands: BExpr, next: BExpr) => BinaryBExpr(BoolAND, ands, next)))
                } else if (gammas.size == 1) {
                  Some(gammas.head)
                } else {
                  None
                }).map(prop => BinaryBExpr(BoolIMPLIES, condition, prop))
              }
            }
          }
          case _ => List()
        }
      })
    }).flatMap(p => {
      Logger.debug("Simplified " + p.toString() + " into " + p.simplify.toString())
      p.simplify match {
        case TrueBLiteral => None
        case p => Some[BExpr](p)
      }
    }).toSet.toList

    val wpThing = procedure.entryBlock.flatMap(b => predDomainResults.get(b).flatMap(p =>
        p.toBasil).map(p =>
          eval.simplifyCondFixpoint(p)._1.toBoogie
        )).toList

    mustGammasWithConditions ++ wpThing
  }

  /** Generate ensures clauses for a procedure. Currently, all generated ensures clauses are of the form (for example)
    * ensures Gamma_R2 || (Gamma_R2 == old(Gamma_y)) || (Gamma_R2 == old(Gamma_R1)) || (Gamma_R2 == old(Gamma_R2));
    * whenever this can be done soundly.
    */
  def generateEnsures(procedure: Procedure): List[BExpr] = {
    if procedure.blocks.isEmpty then return List()

    // We only need to make ensures clauses about the gammas of modified globals our output variables.
    val relevantVars = (variables ++ procedure.formalOutParam).filter { v =>
      v match {
        case v: Global => procedure.modifies.contains(v)
        case v: LocalVar => procedure.formalOutParam.contains(v)
      }
    }

    // TODO further explanation of this would help
    // Use rnaResults to find stack function arguments
    // TODO COMMENTS I think the reason I added this relevantVars.map thing is to make variables that aren't
    // tainted by anything report their gammas as low.
    val tainters = relevantVars.map {
      v => (v, Set())
    }.toMap ++ getTainters(procedure).filter { (variable, taints) =>
      relevantVars.contains(variable)
    }

    Logger.debug("For " + procedure.toString)
    Logger.debug(relevantVars)
    Logger.debug(tainters)
    Logger.debug(getTainters(procedure))
    Logger.debug(varDepsSummaries.get(procedure))

    val taintPreds = tainters.toList.flatMap {
      (variable, taints) => {
        val varGamma = variable.toGamma
        if taints.isEmpty then {
          // If the variable is tainted by nothing, it must have been set to some constant value, meaning
          // its gamma is low. Note that if a variable was unchanged, it would be tainted by itself.
          Some(varGamma)
        } else {
          // Else, we must consider the variables which have tainted this variable. We can soundly assert
          // that this variable's gamma must be equal to one of its tainter's gammas, or it is set to
          // low.
          //
          // In a correct procedure, we never have L(x) = true and Gamma_x = false, so we can safely
          // assume L(x) || Gamma_x == Gamma_x. This means that the gamma of any expression is equal
          // to the conjunction of the gammas of the variables and the loads in the expression.
          val equations = taints.map { tainter =>
            BinaryBExpr(BoolEQ, varGamma, Old(tainter.toGamma))
          }

          if equations.nonEmpty then {
            Some(equations.foldLeft(varGamma: BExpr) { (expr, eq) =>
              BinaryBExpr(BoolOR, expr, eq)
            })
          } else {
            None
          }
      }
    }

    val returnBlock = IRWalk.lastInProc(procedure).map(_.parent)

    val initialState = LatticeMap.TopMap((procedure.formalInParam.toSet: Set[Variable]).map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)

    val predDomain = PredDisjunctiveCompletion(PredProductDomain(DoubleIntervalDomain(), MayGammaDomain(initialState)))
    //val predDomain = PredDisjunctiveCompletion(SignedIntervalDomain())
    //val predDomain = PredDisjunctiveCompletion(UnsignedIntervalDomain())
    //val predDomain = PredDisjunctiveCompletion(DoubleIntervalDomain())
    //val predDomain = PredDisjunctiveCompletion(MayGammaDomain(initialState))
    val (before, after) = worklistSolver(predDomain).solveProc(procedure)
    val outVars: Set[Variable] = procedure.formalOutParam.toSet ++ procedure.formalInParam

    val absIntPreds = returnBlock.map(b => after.get(b).map(l => filterPred(predDomain.toPred(l), outVars, Predicate.Lit(TrueLiteral)).split.map(_.simplify.toBoogie.simplify))).flatten.toList.flatten

    taintPreds ++ absIntPreds
  }
}
