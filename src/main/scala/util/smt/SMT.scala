package util.SMT

import analysis.{BVTerm, GammaTerm, Predicate}
import ir.*
import org.sosy_lab.common.ShutdownManager
import org.sosy_lab.common.configuration.Configuration
import org.sosy_lab.common.log.LogManager
import org.sosy_lab.java_smt.SolverContextFactory
import org.sosy_lab.java_smt.api.{
  BitvectorFormula,
  BooleanFormula,
  Evaluator,
  Formula,
  FormulaManager,
  FormulaType,
  ProverEnvironment,
  SolverContext
}

import scala.jdk.CollectionConverters.{SeqHasAsJava, SetHasAsJava}

// TODO
// support moving up and down a proof context stack
// support other solvers

/** The result of an SMT query */
enum SatResult {
  case SAT(model: Option[Model])
  case UNSAT
  case Unknown(s: String)
}

enum Solver {
  case Z3
  case CVC5
}

/** A wrapper around an SMT solver.
 *
 *  (!!) It is very important (!!) to close the solver with [[close]] once you are done with it to prevent memory leaks!
 *
 *  This class contains an SMT solver context. This means that ideally, you should create a single solver, and make many
 *  queries to it. Queries can be either BASIL [[Expr]]s of type [[BoolType]] or [[Predicate]]s. Alternatively one can
 *  pass a string representation of an SMT2 query using [[smt2Sat]].
 *
 *  A solver wide default timeout can optionally be given with the [[defaultTimeoutMillis]] variable, but this can be
 *  overwritten by calling sat methods with an additional parameter.
 *
 *  Models can be obtained by requesting for them in smt query method calls.
 */
class SMTSolver(var defaultTimeoutMillis: Option[Int] = None, solver: Solver = Solver.Z3) {

  /** Create solver with timeout
   *
   *  @param defaultTimeoutMillis milliseconds of timeout
   */
  def this(defaultTimeoutMillis: Int) = this(Some(defaultTimeoutMillis))

  val shutdownManager = ShutdownManager.create()

  val solverContext: SolverContext = {
    val builder = Configuration.builder()
    builder.copyFrom(Configuration.defaultConfiguration())
    (solver, defaultTimeoutMillis) match {
      case (Solver.CVC5, Some(tl)) =>
        builder.setOption("solver.cvc5.furtherOptions", s"tlimit-per=${tl}")
      case _ => ()
    }
    val logger = LogManager.createNullLogManager()
    val shutdown = shutdownManager.getNotifier()
    SolverContextFactory.createSolverContext(
      builder.build(),
      logger,
      shutdown,
      solver match {
        case Solver.Z3 => SolverContextFactory.Solvers.Z3
        case Solver.CVC5 => SolverContextFactory.Solvers.CVC5
      }
    )
  }

  val formulaConverter = FormulaConverter(solverContext.getFormulaManager())

  def getProver(obtainModel: Boolean = false): SMTProver = {
    val prover =
      if obtainModel
      then solverContext.newProverEnvironment(SolverContext.ProverOptions.GENERATE_MODELS)
      else solverContext.newProverEnvironment()

    SMTProver(solverContext, shutdownManager, formulaConverter, prover)
  }

  private def sat(f: BooleanFormula, timeoutMillis: Option[Int], obtainModel: Boolean = false): SatResult = {
    val env = getProver(obtainModel)
    env.addConstraint(f)
    val r = env.checkSat(timeoutMillis.orElse(defaultTimeoutMillis), obtainModel)
    env.close()
    r
  }

  /** Run solver on a [[Predicate]] */
  def predSat(p: Predicate, timeoutMillis: Option[Int] = None, obtainModel: Boolean = false): SatResult = {
    sat(formulaConverter.convertPredicate(p), timeoutMillis.orElse(defaultTimeoutMillis), obtainModel)
  }

  /** Run solver on a boolean typed BASIL [[Expr]] */
  def exprSat(p: Expr, timeoutMillis: Option[Int] = None, obtainModel: Boolean = false): SatResult = {
    sat(formulaConverter.convertBoolExpr(p), timeoutMillis.orElse(defaultTimeoutMillis), obtainModel)
  }

  /** Run solver on a predicate given as an SMT2 string */
  def smt2Sat(s: String, timeoutMillis: Option[Int] = None, obtainModel: Boolean = false): SatResult = {
    sat(solverContext.getFormulaManager().parse(s), timeoutMillis.orElse(defaultTimeoutMillis), obtainModel)
  }

  /** Close the solver to prevent a memory leak when done. */
  def close() = {
    solverContext.close()
  }

}

class SMTEvaluator(formulaConverter: FormulaConverter, eval: Evaluator) {

  def evalExpr(e: Expr): Option[Literal] = {
    e.getType match {
      case BoolType =>
        evalBoolExpr(e).map {
          case true => TrueLiteral
          case false => FalseLiteral
        }
      case _: BitVecType => evalBVExpr(e)
      case _ => throw Exception(s"Model eval not supported for expr : ${e.getType}")
    }
  }

  def evalBoolExpr(e: Expr): Option[Boolean] = {
    Option(eval.evaluate(formulaConverter.convertBoolExpr(e)))
  }

  def evalBVExpr(e: Expr): Option[BitVecLiteral] = {
    val width = e.getType match {
      case BitVecType(s) => s
      case _ => throw Exception("not a bv formula")
    }
    Option(eval.evaluate(formulaConverter.convertBVExpr(e))).map(v => BitVecLiteral(v, width))
  }

}

class SMTProver(
  val solverContext: SolverContext,
  val shutdownManager: ShutdownManager,
  val formulaConverter: FormulaConverter,
  val prover: ProverEnvironment
) {

  def addConstraint(e: BooleanFormula) = {
    prover.addConstraint(e)
  }

  def addConstraint(e: Expr) = {
    prover.addConstraint(formulaConverter.convertBoolExpr(e))
  }

  def addConstraint(e: Predicate) = {
    prover.addConstraint(formulaConverter.convertPredicate(e))
  }

  def close() = {
    prover.close()
  }

  def checkSat(timeoutMillis: Option[Int] = None, obtainModel: Boolean = false): SatResult = {
    // To handle timeouts, we must create a thread that sends a shutdown request after an amount of milliseconds
    val thread = timeoutMillis.map(m => {
      new Thread(new Runnable() {
        override def run() = {
          try {
            Thread.sleep(m)
            shutdownManager.requestShutdown("Timeout")
          } catch { e => { println(s"$e") } }
        }
      })
    })

    try {
      thread.map(_.start)
      val res =
        if prover.isUnsat() then SatResult.UNSAT
        else
          SatResult.SAT(obtainModel match {
            case true => Some(Model(prover.getModel))
            case false => None
          })
      res
    } catch { e =>
      SatResult.Unknown(e.toString())
    } finally {
      thread.map(t => {
        t.interrupt()
        t.join()
      })
    }
  }

  def getEvaluator() = {
    SMTEvaluator(formulaConverter, prover.getEvaluator())
  }

}

class FormulaConverter(formulaManager: FormulaManager) {
  lazy val ufFormulaMAnager = formulaManager.getUFManager()
  lazy val bitvectorFormulaManager = formulaManager.getBitvectorFormulaManager()
  lazy val booleanFormulaManager = formulaManager.getBooleanFormulaManager()
  lazy val integerFormulaManager = formulaManager.getIntegerFormulaManager()

  def convertBoolLit(lit: BoolLit): BooleanFormula = {
    lit match {
      case FalseLiteral => booleanFormulaManager.makeFalse()
      case TrueLiteral => booleanFormulaManager.makeTrue()
    }
  }

  def convertBoolBinOp(op: BoolBinOp, a: BooleanFormula, b: BooleanFormula): BooleanFormula = {
    op match {
      case BoolAND => booleanFormulaManager.and(a, b)
      case BoolOR => booleanFormulaManager.or(a, b)
      case BoolIMPLIES => booleanFormulaManager.implication(a, b)
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
      case BVCOMP =>
        booleanFormulaManager.ifThenElse(
          bitvectorFormulaManager.equal(a, b),
          bitvectorFormulaManager.makeBitvector(1, 1),
          bitvectorFormulaManager.makeBitvector(1, 0)
        )
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

  def convertExpr(e: Expr): Formula = {
    e.getType match {
      case BoolType => convertBoolExpr(e)
      case _: BitVecType => convertBVExpr(e)
      case IntType =>
        e match {
          case IntLiteral(v) =>
            bitvectorFormulaManager.makeBitvector(v.bitLength + 2, v.bigInteger)
          case _ => throw Exception(s"integer formulas are not supported ${e.getType}: $e")
        }
      case _ => throw Exception(s"unsupported expr type ${e.getType}: $e")
    }

  }

  def convertBoolExpr(e: Expr): BooleanFormula = {
    assert(e.getType == BoolType)
    e match {
      case TrueLiteral => booleanFormulaManager.makeTrue()
      case FalseLiteral => booleanFormulaManager.makeFalse()
      case AssocExpr(BoolAND, args) => booleanFormulaManager.and(args.map(convertBoolExpr).asJava)
      case AssocExpr(BoolOR, args) => booleanFormulaManager.or(args.map(convertBoolExpr).asJava)
      case BinaryExpr(op, arg, arg2) =>
        op match {
          case op: BoolBinOp => convertBoolBinOp(op, convertBoolExpr(arg), convertBoolExpr(arg2))
          case op: PolyCmp =>
            arg.getType match {
              case _: BitVecType => convertBVCmpOp(op, convertBVExpr(arg), convertBVExpr(arg2))
              case BoolType =>
                op match {
                  case EQ => booleanFormulaManager.equivalence(convertBoolExpr(arg), convertBoolExpr(arg2))
                  case NEQ =>
                    booleanFormulaManager.not(
                      booleanFormulaManager.equivalence(convertBoolExpr(arg), convertBoolExpr(arg2))
                    )
                }
              case _ => ???
            }
          case op: BVCmpOp => convertBVCmpOp(op, convertBVExpr(arg), convertBVExpr(arg2))
          case _ => ???
        }
      case UnaryExpr(op, arg) =>
        op match {
          case op: BoolUnOp =>
            op match {
              case BoolNOT => booleanFormulaManager.not(convertBoolExpr(arg))
              case BoolToBV1 => throw Exception("Attempt to convert a bitvector expression to a bool formula")
            }
          case _ => ???
        }
      case v: Variable => booleanFormulaManager.makeVariable(v.name)
      case r: OldExpr => ???
      case e => throw Exception(s"Non boolean expression was attempted to be converted: ${e.getClass.getSimpleName()}")
    }
  }

  def convertBVExpr(e: Expr): BitvectorFormula = {
    assert(e.getType.isInstanceOf[BitVecType])
    e match {
      case BitVecLiteral(value, size) => bitvectorFormulaManager.makeBitvector(size, value.bigInteger)
      case Extract(end, start, arg) => bitvectorFormulaManager.extract(convertBVExpr(arg), end - 1, start)
      case Repeat(repeats, arg) => {
        val x = convertBVExpr(arg)
        (1 until repeats).foldLeft(x)((f, _) => bitvectorFormulaManager.concat(f, x))
      }
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
          case BoolToBV1 =>
            booleanFormulaManager.ifThenElse(
              convertBoolExpr(arg),
              bitvectorFormulaManager.makeBitvector(1, 1),
              bitvectorFormulaManager.makeBitvector(1, 0)
            )
          case _ => throw Exception("Non bitvector operation was attempted to be converted")
        }
      case v: Variable => convertBVVar(v.irType, v.name)
      case r: OldExpr => ???
      case FApplyExpr(n, p, rt @ BitVecType(sz), _) =>
        val t: FormulaType[BitvectorFormula] = FormulaType.getBitvectorTypeWithSize(sz)
        ufFormulaMAnager.declareAndCallUF(n, t, p.map(convertExpr).toList.asJava)
      case e => throw Exception(s"Non bitvector expression was attempted to be converted: $e")
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
      case Extract(end, start, body) => bitvectorFormulaManager.extract(convertBVTerm(body), end - 1, start)
      case Repeat(repeats, body) => {
        val x = convertBVTerm(body)
        (1 until repeats).foldLeft(x)((f, _) => bitvectorFormulaManager.concat(f, x))
      }
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
          case BoolToBV1 => throw Exception("Attempt to convert a bitvector expression to a bool formula")
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
