import ir.*
import ir.eval.*
import org.scalatest.*
import org.scalatest.funsuite.*
import specification.*
import util.{Logger, LogLevel}
import org.scalatest.funsuite.AnyFunSuite
import test_util.BASILTest
import util.{BASILResult, StaticAnalysisConfig, IRContext}
import translating.PrettyPrinter.*

trait TestValueDomainWithInterpreter[T] {

  /**
   * Construct a Basil IR expression which evaluates to true iff 
   * the concrete expression `concrete` is contained in the abstract value `absval`.
   */
  def valueInAbstractValue(absval: T, concrete: Expr): Expr

  case class CheckResult(name: String, breakpoint: BreakPoint, testExpr: Expr, evaluatedTestExpr: Expr)

  enum Heuristic:
    case AllVarsInAbstract
    case VarsLiveInBlock

  case class InterpreterTestResult(stopCondition: ExecutionContinuation, checks: List[CheckResult]) {
    def getFailures = {
      // require nonzero number of checks to ensure test is not vacuous
      var noChecks = if (checks.isEmpty) then Seq("no checks hit") else Seq()
      val termination =
        if (!normalTermination(stopCondition)) then Seq(s"Stopped with error condition: ${stopCondition}") else Seq()
      termination ++ noChecks ++ checksFailed
    }

    def checksFailed = {
      // collected breakpoints evaluate to true
      checks.flatMap(b => {
        val loc = b.breakpoint.location match {
          case BreakPointLoc.CMD(c) => c.parent.label
          case _ => ??? /* not used here */
        }
        if (b.evaluatedTestExpr != TrueLiteral) then Seq(s"${b.name} @ $loc") else Seq()
      })
    }
  }

  def runTestInterpreter(
    ictx: IRContext,
    testResultBefore: Map[Block, Map[Variable, T]],
    testVars: Heuristic = Heuristic.AllVarsInAbstract
  ): InterpreterTestResult = {

    val breaks: List[BreakPoint] = ictx.program.collect {
      // convert analysis result to a list of breakpoints, each which evaluates an expression describing
      // the invariant inferred by the analysis (the assignment of registers) at a corresponding program point
      case (block: Block) if (testResultBefore.contains(block)) => {
        val result = testResultBefore(block)
        // only create assertion for variables relevant to the block and defined in the abstract state,
        // on the assumption they are likely to be defined in the concrete state
        val vars = (testVars match {
          case Heuristic.AllVarsInAbstract => result.keySet
          case Heuristic.VarsLiveInBlock => freeVarsPos(block).filter(result.contains)
        }).map(v => (v, result(v)))

        val expectedPredicates: List[(String, Expr)] = vars.toList.flatMap(r => {
          val (variable, value) = r
          val assertion = valueInAbstractValue(value, variable)
          Seq((s"${variable.name} ⊆ ${value}", assertion))
        })
        BreakPoint(
          location = BreakPointLoc.CMD(IRWalk.firstInBlock(block)),
          BreakPointAction(saveState = false, evalExprs = expectedPredicates)
        )
      }
    }.toList

    // run the interpreter evaluating the analysis result at each command with a breakpoint
    val interpretResult = interpretWithBreakPoints(ictx, breaks.toList, NormalInterpreter, InterpreterState())

    val breakres: List[(BreakPoint, _, List[(String, Expr, Expr)])] = interpretResult._2
    val checkResults = breakres.flatMap { case (bp, _, l) =>
      l.map { case (name, test, evaled) =>
        CheckResult(name, bp, test, evaled)
      }
    }.toList

    InterpreterTestResult(interpretResult._1.nextCmd, checkResults)
  }

}
