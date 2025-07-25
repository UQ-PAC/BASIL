package test_util

import ir.*
import ir.eval.*
import translating.PrettyPrinter.*
import util.IRContext
import util.functional.State

trait TestValueDomainWithInterpreter[T] {

  /**
   * Construct a Basil IR expression which evaluates to true iff 
   * the concrete expression `concrete` is contained in the abstract value `absval`.
   */
  def valueInAbstractValue(absval: T, concrete: Expr): Expr

  def abstractEvalSoundnessProperty(evaluate: Expr => T)(expr: Expr): (Boolean, String) = {
    // val expr = BinaryExpr(op, BitVecLiteral(lhs, size), BitVecLiteral(rhs, size))
    val abs: T = evaluate(expr)
    val concrete = ir.eval.evaluateExpr(expr).getOrElse(throw Exception(s"Failed to eval expr : $expr"))
    val test = valueInAbstractValue(abs, concrete)
    val result = ir.eval.evaluateExpr(test).get
    (result == TrueLiteral, s"${pp_expr(concrete)} ∈ $abs test (${pp_expr(test)}) evaluated to $result")
  }

  case class CheckResult(
    name: String,
    breakpoint: BreakPoint,
    testExpr: Expr,
    variable: Expr,
    variableValue: Option[Expr],
    evaluatedTestExpr: Option[Expr]
  )

  enum Heuristic:
    case AllVarsInAbstract
    case VarsLiveInBlock

  case class InterpreterTestResult(stopCondition: ExecutionContinuation, checks: List[CheckResult]) {
    def getFailures: Seq[String] = {
      // require nonzero number of checks to ensure test is not vacuous
      var noChecks = if (checks.filter(_.evaluatedTestExpr.isDefined).isEmpty) then Seq("no checks hit") else Seq()
      val termination =
        if (!normalTermination(stopCondition)) then Seq(s"Stopped with error condition: ${stopCondition}") else Seq()
      termination ++ noChecks ++ checksFailed
    }

    def checksFailed: Seq[StackFrameID] = {
      // collected breakpoints evaluate to true
      checks.flatMap(b => {
        val loc = b.breakpoint.location match {
          case BreakPointLoc.CMD(c) => c.parent.label
          case _ => ??? /* not used here */
        }
        if (b.evaluatedTestExpr.contains(FalseLiteral)) then Seq(s"${b.name} @ $loc :: ${b.evaluatedTestExpr}")
        else Seq()
      })
    }

    def checksPassed: Seq[StackFrameID] = {
      // collected breakpoints evaluate to true
      checks.flatMap(b => {
        val loc = b.breakpoint.location match {
          case BreakPointLoc.CMD(c) => c.parent.label
          case _ => ??? /* not used here */
        }
        if (b.evaluatedTestExpr.contains(TrueLiteral)) then Seq(s"${b.name} @ $loc :: ${b.evaluatedTestExpr}")
        else Seq()
      })
    }

    def toDotLabels: Map[Block, String] = {
      checks
        .map(b => {
          val loc = b.breakpoint.location match {
            case BreakPointLoc.CMD(c) => c.parent
            case _ => ??? /* not used here */
          }
          (
            loc,
            s"${b.name} (${pp_expr(b.variable)} = ${b.variableValue.map(pp_expr).getOrElse("eval error")} ) = ${b.evaluatedTestExpr.map(pp_expr).getOrElse("eval error")}"
          )
        })
        .groupBy(_(0))
        .map((g, v) => g -> v.map(_(1)).mkString("\n"))
        .toMap
    }
  }

  def runTestInterpreter(
    ictx: IRContext,
    testResultBefore: Map[Block, Map[Variable, T]],
    testResultAfter: Map[Block, Map[Variable, T]] = Map(),
    testVars: Heuristic = Heuristic.AllVarsInAbstract,
    callProcedure: Option[Procedure] = None,
    callParams: Option[Iterable[(LocalVar, Literal)]] = None
  ): InterpreterTestResult = {

    def makeBreakpoint(result: Map[Variable, T], block: Block, location: BreakPointLoc) = {
        // only create assertion for variables relevant to the block and defined in the abstract state,
        // on the assumption they are likely to be defined in the concrete state
        val vars = (testVars match {
          case Heuristic.AllVarsInAbstract => result.keySet
          case Heuristic.VarsLiveInBlock => freeVarsPos(block).filter(result.contains)
        }).map(v => (v, result(v)))

        val expectedPredicates: List[(String, Expr)] = vars.toList.flatMap(r => {
          val (variable, value) = r
          val assertion = valueInAbstractValue(value, variable)
          Seq((s"${variable.name}", variable), (s"${variable.name} ∈ ${value}", assertion))
        })
        BreakPoint(
          location = location,
          BreakPointAction(saveState = false, evalExprs = expectedPredicates)
        )
    }

    // convert analysis result to a list of breakpoints, each which evaluates an expression describing
    // the invariant inferred by the analysis (the assignment of registers) at a corresponding program point
    val breaks: List[BreakPoint] = ictx.program.collect {
      case (block: Block) if (testResultBefore.contains(block)) => {
        val result = testResultBefore(block)
        makeBreakpoint(result, block, BreakPointLoc.CMD(IRWalk.firstInBlock(block)))
      }
    }.toList ++ ictx.program.collect {
      case (block: Block) if (testResultAfter.contains(block)) => {
        val result = testResultAfter(block)
        makeBreakpoint(result, block, BreakPointLoc.CMD(IRWalk.lastInBlock(block)))
      }
    }

    // run the interpreter evaluating the analysis result at each command with a breakpoint

    val startProc = callProcedure.getOrElse(ictx.program.mainProcedure)
    val startParams = callParams.getOrElse(InterpFuns.mainDefaultFunctionArguments(startProc))
    val innerInitState = InterpFuns.initProgState(NormalInterpreter)(ictx, InterpreterState())
    val interp = LayerInterpreter(NormalInterpreter, RememberBreakpoints(NormalInterpreter, breaks.toList))
    val initState = (innerInitState, List())
    val interpretResult = State.execute(initState, InterpFuns.callProcedure(interp)(startProc, startParams))

    val breakres: List[(BreakPoint, _, List[(String, Expr, Option[Expr])])] = interpretResult(1)
    val checkResults = breakres.flatMap { case (bp, _, evaledExprs) =>
      evaledExprs.grouped(2).map(_.toList).map {
        case List((_, variable, varValue), (name, test, evaled)) =>
          CheckResult(name, bp, test, variable, varValue, evaled)
        case _ => ???
      }
    }.toList

    InterpreterTestResult(interpretResult(0).nextCmd, checkResults)
  }
}
