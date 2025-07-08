package util.SMT

import analysis.{BVTerm, GammaTerm, Predicate}
import ir.*
import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.LogManager
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.api.{BitvectorFormula, BooleanFormula, FormulaManager}

import scala.jdk.CollectionConverters.SetHasAsJava

enum SatResult {
  case SAT
  case UNSAT
  case Unknown(s: String)
}

/** Make sure to close the solver when you are done!
 */
class SMTSolver(var timeoutMillis: Option[Int] = None) {
  val shutdownManager = ShutdownManager.create()

  val solverContext = {
    val config = Configuration.defaultConfiguration()
    val logger = LogManager.createNullLogManager()
    val shutdown = shutdownManager.getNotifier()
    SolverContextFactory.createSolverContext(config, logger, shutdown, SolverContextFactory.Solvers.Z3)
  }

  val formulaConverter = FormulaConverter(solverContext.getFormulaManager())

  def satisfiable(f: BooleanFormula): SatResult = {
    // To handle timeouts, we must create a thread that sends a shutdown request after an amount of milliseconds
    val thread = timeoutMillis.map(m => {
      new Thread(new Runnable() {
        def run() = {
          try {
            Thread.sleep(m)
            shutdownManager.requestShutdown("Timeout")
          } catch { _ => {} }
        }
      })
    })

    try {
      val env = solverContext.newProverEnvironment()
      env.push(f)
      thread.map(_.start)
      val res = if env.isUnsat() then SatResult.UNSAT else SatResult.SAT
      env.close()
      res
    } catch { _ =>
      SatResult.Unknown("")
    } finally {
      thread.map(t => {
        t.interrupt()
        t.join()
      })
    }
  }

  def satisfiable(p: Predicate): SatResult = {
    satisfiable(formulaConverter.convertPredicate(p))
  }

  /** Run the solver on a predicate given as an SMT2 string
   */
  def smt2Satisfiable(s: String): SatResult = {
    satisfiable(solverContext.getFormulaManager().parse(s))
  }

  def close() = {
    solverContext.close()
  }

}

class FormulaConverter(formulaManager: FormulaManager) {
  lazy val bitvectorFormulaManager = formulaManager.getBitvectorFormulaManager()
  lazy val booleanFormulaManager = formulaManager.getBooleanFormulaManager()

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

  // Convert IR expressions

  def convertBoolExpr(e: Expr): BooleanFormula = {
    assert(e.getType == BoolType)
    e match {
      case TrueLiteral => booleanFormulaManager.makeTrue()
      case FalseLiteral => booleanFormulaManager.makeFalse()
      case BinaryExpr(op, arg, arg2) =>
        op match {
          case op: BoolBinOp => ???
          case op: PolyCmp => ???
          case op: BVCmpOp => convertBVCmpOp(op, convertBVExpr(arg), convertBVExpr(arg2))
          case _ => ???
        }
      case UnaryExpr(op, arg) =>
        op match {
          case op: BoolUnOp => ???
          case _ => ???
        }
      case v: Variable => ???
      case r: OldExpr => ???
      case _ => throw Exception("Non boolean expression was attempted to be converted")
    }
  }

  def convertBVExpr(e: Expr): BitvectorFormula = {
    assert(e.getType.isInstanceOf[BitVecType])
    e match {
      case BitVecLiteral(value, size) => bitvectorFormulaManager.makeBitvector(size, value.bigInteger)
      case Extract(end, start, arg) => bitvectorFormulaManager.extract(convertBVExpr(arg), end, start)
      case Repeat(repeats, arg) => ???
      case ZeroExtend(bits, arg) => bitvectorFormulaManager.extend(convertBVExpr(arg), bits, false)
      case SignExtend(bits, arg) => bitvectorFormulaManager.extend(convertBVExpr(arg), bits, true)
      case BinaryExpr(op, arg, arg2) =>
        op match {
          case op: BVBinOp => convertBVBinOp(op, convertBVExpr(arg), convertBVExpr(arg2))
          case _ => throw Exception("Non bitvector operation was attempted to be converted")
        }
      case UnaryExpr(op, arg) =>
        op match {
          case op: BVUnOp => convertBVUnOp(op, convertBVExpr(arg))
          case BoolToBV1 => ???
          case _ => throw Exception("Non bitvector operation was attempted to be converted")
        }
      case v: Variable => convertBVVar(v.irType, v.name)
      case r: OldExpr => ???
      case _ => throw Exception("Non bitvector expression was attempted to be converted")
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
