import analysis.*
import ir.*
import ir.dsl.*
import ir.eval.BitVectorEval.nat2bv
import ir.transforms.{reversePostOrder, worklistSolver}
import org.scalatest.funsuite.AnyFunSuite
import test_util.{CaptureOutput, TestValueDomainWithInterpreter}
import util.{LogLevel, Logger}
import translating.PrettyPrinter.*

@test_util.tags.UnitTest
class IntervalAnalysisTests extends AnyFunSuite, CaptureOutput {}

trait InterpretIntervalAnalysisTests(signed: Boolean)
    extends AnyFunSuite
    with CaptureOutput
    with TestValueDomainWithInterpreter[Interval] {

  Logger.setLevel(LogLevel.ERROR)

  def valueInAbstractValue(absval: Interval, concrete: Expr): Expr =
    absval match {
      case Interval.Top => TrueLiteral
      case Interval.Bottom => FalseLiteral
      case Interval.ConcreteInterval(lower, upper, width) if signed =>
        BinaryExpr(
          BoolAND,
          BinaryExpr(BVSLE, signedInt2bv(width, lower), concrete),
          BinaryExpr(BVSLE, concrete, signedInt2bv(width, upper))
        )
      case Interval.ConcreteInterval(lower, upper, width) /* if !signed */ =>
        BinaryExpr(
          BoolAND,
          BinaryExpr(BVULE, nat2bv(width, lower), concrete),
          BinaryExpr(BVULE, concrete, nat2bv(width, upper))
        )
    }

  def toMap(m: Map[Block, LatticeMap[Variable, Interval]]): Map[Block, Map[Variable, Interval]] =
    m.map((b, lm) => {
      b -> (lm match {
        case LatticeMap.Top() => Map().withDefault(_ => Interval.Top): Map[Variable, Interval]
        case LatticeMap.TopMap(m) => m.withDefault(_ => Interval.Top): Map[Variable, Interval]
        case LatticeMap.Bottom() => Map().withDefault(_ => Interval.Bottom): Map[Variable, Interval]
        case LatticeMap.BottomMap(m) => m.withDefault(_ => Interval.Bottom): Map[Variable, Interval]
      })
    })

  def testProgram(program: Program, params: Option[Iterable[(LocalVar, Literal)]]) = {
    val context = util.IRLoading.load(program)
    val domain = if signed then SignedIntervalDomain() else UnsignedIntervalDomain()
    val procedure = program.nameToProcedure("f_1892")
    reversePostOrder(procedure)
    val (beforeResults, afterResults) = worklistSolver(domain).solveProc(procedure)
    val res = runTestInterpreter(context, toMap(beforeResults), toMap(afterResults), testVars = Heuristic.VarsLiveInBlock, callProcedure = Some(procedure), callParams = params)
    assert(res.checksPassed.nonEmpty)
    assert(
      res.checksFailed.isEmpty,
      "\n\n" + pp_prog_with_analysis_results(res.toDotLabels, Map(), context.program, x => x.toString) + "\n\n"
    )

  }

  test("Operations") {
    val program = prog(
      proc("f_1892",
        Seq(
          "R0_in" -> BitVecType(64),
          "R1_in" -> BitVecType(64),
          "R2_in" -> BitVecType(64),
        ),
        Seq(
          "R0_out" -> BitVecType(64),
          "R1_out" -> BitVecType(64)
        ),
        block("f_entry",
          LocalAssign(LocalVar("Stack_n8_0", BitVecType(64), 0), LocalVar("R0_in", BitVecType(64), 0), Some("1896_0")),
          LocalAssign(LocalVar("Stack_n16_n8", BitVecType(64), 0), LocalVar("R1_in", BitVecType(64), 0), Some("1900_0")),
          LocalAssign(LocalVar("Stack_n24_n16", BitVecType(64), 0), LocalVar("R2_in", BitVecType(64), 0), Some("1904_0")),
          LocalAssign(LocalVar("Stack_n8_0", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64), Some("1908_0")),
          LocalAssign(LocalVar("Stack_n16_n8", BitVecType(64), 0), BitVecLiteral(BigInt("18446744073709551606"), 64), Some("1916_0")),
          LocalAssign(LocalVar("load15", BitVecType(64), 1), LocalVar("Stack_n8_0", BitVecType(64), 0), Some("1920_0_0")),
          LocalAssign(LocalVar("load16", BitVecType(64), 1), LocalVar("Stack_n24_n16", BitVecType(64), 0), Some("1924_0_0")),
          LocalAssign(LocalVar("R0", BitVecType(64), 4), BinaryExpr(BVADD, LocalVar("load15", BitVecType(64), 1), LocalVar("load16", BitVecType(64), 1)), Some("1928_0")),
          LocalAssign(LocalVar("Stack_n16_n8", BitVecType(64), 0), LocalVar("R0", BitVecType(64), 4), Some("1932_0")),
          LocalAssign(LocalVar("load17", BitVecType(64), 1), LocalVar("Stack_n24_n16", BitVecType(64), 0), Some("1936_0_0")),
          goto("phi_3", "phi_4")
        ),
        block("f_return",
          LocalAssign(LocalVar("load18", BitVecType(64), 1), LocalVar("Stack_n16_n8", BitVecType(64), 0), Some("1980_0_0")),
          Assert(TrueLiteral, Some("R31 preserved across calls"), None),
          ret(
            "R0_out" -> LocalVar("load18", BitVecType(64), 1),
            "R1_out" -> LocalVar("R1", BitVecType(64), 9)
          )
        ),
        block("f_4",
          goto("f_return")
        ),
        block("phi_3",
          Assume(BinaryExpr(BVSLE, LocalVar("load17", BitVecType(64), 1), BitVecLiteral(BigInt("0"), 64)), None, None, true),
          LocalAssign(LocalVar("R1", BitVecType(64), 9), LocalVar("load15", BitVecType(64), 1), Some("phiback")),
          goto("f_4")
        ),
        block("phi_4",
          Assume(BinaryExpr(BVSGT, LocalVar("load17", BitVecType(64), 1), BitVecLiteral(BigInt("0"), 64)), None, None, true),
          LocalAssign(LocalVar("load11", BitVecType(64), 1), LocalVar("Stack_n16_n8", BitVecType(64), 0), Some("1948_0_0")),
          LocalAssign(LocalVar("load12", BitVecType(64), 1), LocalVar("Stack_n8_0", BitVecType(64), 0), Some("1952_0_0")),
          LocalAssign(LocalVar("R0", BitVecType(64), 7), BinaryExpr(BVADD, LocalVar("load11", BitVecType(64), 1), LocalVar("load12", BitVecType(64), 1)), Some("1956_0")),
          LocalAssign(LocalVar("Stack_n24_n16", BitVecType(64), 0), LocalVar("R0", BitVecType(64), 7), Some("1960_0")),
          LocalAssign(LocalVar("load13", BitVecType(64), 1), LocalVar("Stack_n16_n8", BitVecType(64), 0), Some("1964_0_0")),
          LocalAssign(LocalVar("load14", BitVecType(64), 1), LocalVar("Stack_n8_0", BitVecType(64), 0), Some("1968_0_0")),
          LocalAssign(LocalVar("R0", BitVecType(64), 9), BinaryExpr(BVADD, LocalVar("load13", BitVecType(64), 1), LocalVar("load14", BitVecType(64), 1)), Some("1972_0")),
          LocalAssign(LocalVar("Stack_n16_n8", BitVecType(64), 0), LocalVar("R0", BitVecType(64), 9), Some("1976_0")),
          LocalAssign(LocalVar("R1", BitVecType(64), 9), LocalVar("load13", BitVecType(64), 1), Some("phiback")),
          goto("f_4")
        )
      )
    )

    def params(r0: BigInt, r1: BigInt, r2: BigInt): Option[Iterable[(LocalVar, Literal)]] = Some(List(
        LocalVar("R0_in", BitVecType(64), 0) -> BitVecLiteral(r0, 64),
        LocalVar("R1_in", BitVecType(64), 0) -> BitVecLiteral(r1, 64),
        LocalVar("R2_in", BitVecType(64), 0) -> BitVecLiteral(r2, 64),
      ))

    testProgram(program, params(0, 0, 0))
  }
}

@test_util.tags.UnitTest
class SignedInterpretIntervalAnalysisTests extends InterpretIntervalAnalysisTests(true)

@test_util.tags.UnitTest
class UnsignedInterpretIntervalAnalysisTests extends InterpretIntervalAnalysisTests(false)
