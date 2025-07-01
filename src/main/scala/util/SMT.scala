package util.SMT

import analysis.{BVTerm, GammaTerm, Predicate}
import ir.*
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.api.{BitvectorFormula, BooleanFormula, FormulaManager}

import scala.jdk.CollectionConverters.SetHasAsJava

enum SatResult {
  case SAT
  case UNSAT
  case Unknown(s: String)
}

class FormulaConverter(formulaManager: FormulaManager) {
  val bitvectorFormulaManager = formulaManager.getBitvectorFormulaManager()
  val booleanFormulaManager = formulaManager.getBooleanFormulaManager()

  def convertBoolLit(lit: BoolLit): BooleanFormula = {
    lit match {
      case FalseLiteral => booleanFormulaManager.makeFalse()
      case TrueLiteral => booleanFormulaManager.makeTrue()
    }
  }

  def convertBVUnOp(op: BVUnOp, a: BitvectorFormula): BitvectorFormula = {
    op match {
      case BVNOT => bitvectorFormulaManager.not(a)
      case BVNEG => bitvectorFormulaManager.negate(a)
    }
  }

  def convertBVBinOp(op: BVBinOp, a: BitvectorFormula, b: BitvectorFormula): BitvectorFormula = {
    op match {
      case BVAND => bitvectorFormulaManager.and(a, b)
      case BVOR => bitvectorFormulaManager.or(a, b)
      case BVADD => bitvectorFormulaManager.add(a, b)
      case BVMUL => bitvectorFormulaManager.multiply(a, b)
      case BVUDIV => bitvectorFormulaManager.divide(a, b, false)
      case BVUREM => bitvectorFormulaManager.remainder(a, b, false)
      case BVSHL => bitvectorFormulaManager.shiftLeft(a, b)
      case BVLSHR => bitvectorFormulaManager.shiftRight(a, b, false)
      case BVNAND => bitvectorFormulaManager.not(bitvectorFormulaManager.and(a, b))
      case BVNOR => bitvectorFormulaManager.not(bitvectorFormulaManager.or(a, b))
      case BVXOR => bitvectorFormulaManager.xor(a, b)
      case BVXNOR => bitvectorFormulaManager.not(bitvectorFormulaManager.xor(a, b))
      case BVCOMP => ???
      case BVSUB => bitvectorFormulaManager.subtract(a, b)
      case BVSDIV => bitvectorFormulaManager.divide(a, b, true)
      case BVSREM => bitvectorFormulaManager.remainder(a, b, true)
      case BVSMOD => bitvectorFormulaManager.smodulo(a, b)
      case BVASHR => bitvectorFormulaManager.shiftRight(a, b, true)
      case BVCONCAT => bitvectorFormulaManager.concat(a, b)
      case _ => throw Exception("Comparison bitvector operation used as a binary operator")
    }
  }

  def convertBVCmpOp(op: BVCmpOp | PolyCmp, a: BitvectorFormula, b: BitvectorFormula): BooleanFormula = {
    op match {
      case BVULT => bitvectorFormulaManager.lessThan(a, b, false)
      case BVULE => bitvectorFormulaManager.lessOrEquals(a, b, false)
      case BVUGT => bitvectorFormulaManager.greaterThan(a, b, false)
      case BVUGE => bitvectorFormulaManager.greaterOrEquals(a, b, false)
      case BVSLT => bitvectorFormulaManager.lessThan(a, b, true)
      case BVSLE => bitvectorFormulaManager.lessOrEquals(a, b, true)
      case BVSGT => bitvectorFormulaManager.greaterThan(a, b, true)
      case BVSGE => bitvectorFormulaManager.greaterOrEquals(a, b, true)
      case EQ => bitvectorFormulaManager.equal(a, b)
      case NEQ => booleanFormulaManager.not(bitvectorFormulaManager.equal(a, b))
    }
  }

  // Conversion for analysis predicates

  def convertBVVar(irType: IRType, name: String): BitvectorFormula = irType match {
    case BitVecType(size) => bitvectorFormulaManager.makeVariable(size, name)
    case _ => throw Exception("Non bitvector used in bitvector expression")
  }

  def convertGammaVar(name: String): BooleanFormula = booleanFormulaManager.makeVariable(s"Gamma_$name")

  def convertBVTerm(e: BVTerm): BitvectorFormula = {
    import BVTerm.*
    e match {
      case Lit(x) => bitvectorFormulaManager.makeBitvector(x.size, x.value.bigInteger)
      case Var(v) => convertBVVar(v.irType, v.name)
      case OldVar(v) => convertBVVar(v.irType, s"old(${v.name})")
      case Uop(op, x) => convertBVUnOp(op, convertBVTerm(x))
      case Bop(op, x, y) => convertBVBinOp(op, convertBVTerm(x), convertBVTerm(y))
      case Extract(end, start, body) => bitvectorFormulaManager.extract(convertBVTerm(body), end, start)
      case Repeat(repeats, body) => ???
      case ZeroExtend(extension, body) => bitvectorFormulaManager.extend(convertBVTerm(body), extension, false)
      case SignExtend(extension, body) => bitvectorFormulaManager.extend(convertBVTerm(body), extension, true)
    }
  }

  def convertGammaTerm(e: GammaTerm): BooleanFormula = {
    import GammaTerm.*
    e match {
      case Lit(x) => convertBoolLit(x)
      case Var(v) => convertGammaVar(v.name)
      case OldVar(v) => convertGammaVar(s"old(${v.name})")
      case Uop(op, x) =>
        op match {
          case BoolNOT => booleanFormulaManager.not(convertGammaTerm(x))
          case BoolToBV1 => ???
        }
      case Join(s) => booleanFormulaManager.and(s.map(convertGammaTerm(_)).asJava)
    }
  }

  def convertPredicate(e: Predicate): BooleanFormula = {
    import Predicate.*
    e match {
      case Lit(x) => convertBoolLit(x)
      case Not(x) => booleanFormulaManager.not(convertPredicate(x))
      case Conj(s) => booleanFormulaManager.and(s.map(convertPredicate(_)).asJava)
      case Disj(s) => booleanFormulaManager.and(s.map(convertPredicate(_)).asJava)
      case BVCmp(op, x, y) => convertBVCmpOp(op, convertBVTerm(x), convertBVTerm(y))
      case GammaCmp(op, x, y) =>
        op match {
          case BoolIMPLIES => booleanFormulaManager.implication(convertGammaTerm(x), convertGammaTerm(y))
          case EQ => booleanFormulaManager.equivalence(convertGammaTerm(x), convertGammaTerm(y))
          case NEQ =>
            booleanFormulaManager.not(booleanFormulaManager.equivalence(convertGammaTerm(x), convertGammaTerm(y)))
        }
    }
  }
}

/** Make sure to close the solver when you are done!
 */
class SMTSolver {
  val solverContext = SolverContextFactory.createSolverContext(SolverContextFactory.Solvers.PRINCESS)
  val formulaConverter = FormulaConverter(solverContext.getFormulaManager())

  def satisfiable(p: Predicate): SatResult = {
    val f = formulaConverter.convertPredicate(p)
    try {
      val env = solverContext.newProverEnvironment()
      env.addConstraint(f)
      if env.isUnsat() then SatResult.UNSAT else SatResult.SAT
    } catch { _ =>
      SatResult.Unknown("")
    }
  }

  def close() = {
    solverContext.close()
  }

}
