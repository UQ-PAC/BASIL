import analysis.Interval.ConcreteInterval
import analysis.{TNum, TNumIntervalReducedProduct}
import ir.*
import ir.dsl.*
import ir.transforms.{reversePostOrder, worklistSolver}
import org.scalatest.funsuite.AnyFunSuiteLike

@test_util.tags.UnitTest
class TNumIntervalReducedProductTest extends AnyFunSuiteLike {

  val domain = TNumIntervalReducedProduct()
  val interval = ConcreteInterval(6, 10, 4)
  val tnum = TNum(BitVecLiteral(0, 4), BitVecLiteral(9, 4))

  val kbitsProg = prog(
    proc(
      "knownBitsExample_4196164",
      Seq("R0_in" -> BitVecType(64), "R1_in" -> BitVecType(64)),
      Seq("R0_out" -> BitVecType(64), "R2_out" -> BitVecType(64), "R3_out" -> BitVecType(64)),
      block(
        "lknownBitsExample",
        LocalAssign(
          LocalVar("R2", BitVecType(64), 2),
          BinaryExpr(
            BVOR,
            BinaryExpr(BVAND, LocalVar("R0_in", BitVecType(64), 0), BitVecLiteral(BigInt("18374966859414961920"), 64)),
            BitVecLiteral(BigInt("18446744069414584320"), 64)
          ),
          Some("%0000023e")
        ),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 3),
          BinaryExpr(
            BVOR,
            BinaryExpr(BVAND, LocalVar("R0_in", BitVecType(64), 0), BitVecLiteral(BigInt("18374966859414961920"), 64)),
            BitVecLiteral(BigInt("71777218305454335"), 64)
          ),
          Some("%00000257")
        ),
        goto("lknownBitsExample_phi_lknownBitsExample_goto_l00000271", "lknownBitsExample_goto_l0000026d")
      ),
      block(
        "l00000274",
        LocalAssign(
          LocalVar("R0", BitVecType(64), 6),
          ZeroExtend(
            4,
            BinaryExpr(
              BVSHL,
              ZeroExtend(8, Extract(60, 8, LocalVar("R2", BitVecType(64), 9))),
              BitVecLiteral(BigInt("8"), 60)
            )
          ),
          Some("%0000027e")
        ),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 7),
          BinaryExpr(BVOR, LocalVar("R0", BitVecType(64), 6), BitVecLiteral(BigInt("15"), 64)),
          Some("%00000284")
        ),
        goto("l00000274_phi_l00000274_goto_l00000299", "l00000274_goto_l0000029d")
      ),
      block(
        "l000002a0",
        Assert(TrueLiteral, Some("is returning to caller-set R30"), None),
        goto("knownBitsExample_4196164_basil_return")
      ),
      block(
        "lknownBitsExample_goto_l0000026d",
        Assume(BinaryExpr(EQ, LocalVar("R1_in", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64)), None, None, true),
        LocalAssign(LocalVar("R2", BitVecType(64), 9), LocalVar("R0", BitVecType(64), 3), Some("phiback")),
        goto("l00000274")
      ),
      block(
        "l00000274_goto_l0000029d",
        Assume(
          BinaryExpr(EQ, Extract(16, 0, LocalVar("R2", BitVecType(64), 9)), BitVecLiteral(BigInt("0"), 16)),
          None,
          None,
          true
        ),
        LocalAssign(LocalVar("R0", BitVecType(64), 14), LocalVar("R2", BitVecType(64), 9), Some("phiback")),
        goto("l000002a0")
      ),
      block(
        "knownBitsExample_4196164_basil_return",
        ret(
          "R0_out" -> LocalVar("R0", BitVecType(64), 14),
          "R2_out" -> LocalVar("R2", BitVecType(64), 9),
          "R3_out" -> BitVecLiteral(BigInt("71777218305454335"), 64)
        )
      ),
      block(
        "lknownBitsExample_phi_lknownBitsExample_goto_l00000271",
        Assume(
          UnaryExpr(BoolNOT, BinaryExpr(EQ, LocalVar("R1_in", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64))),
          None,
          None,
          true
        ),
        LocalAssign(LocalVar("R2", BitVecType(64), 9), LocalVar("R2", BitVecType(64), 2), Some("phiback")),
        goto("l00000274")
      ),
      block(
        "l00000274_phi_l00000274_goto_l00000299",
        Assume(
          UnaryExpr(
            BoolNOT,
            BinaryExpr(EQ, Extract(16, 0, LocalVar("R2", BitVecType(64), 9)), BitVecLiteral(BigInt("0"), 16))
          ),
          None,
          None,
          true
        ),
        LocalAssign(LocalVar("R0", BitVecType(64), 14), LocalVar("R0", BitVecType(64), 7), Some("phiback")),
        goto("l000002a0")
      )
    )
  )
  val littleProc = kbitsProg.nameToProcedure("knownBitsExample_4196164")

  test("Refine lower bound - simple example") {
    val expected = 8
    assert(expected == domain.refineLowerBound(6, tnum))
  }

  test("Refine lower bound - 0") {
    val expected = 0
    assert(expected == domain.refineLowerBound(0, tnum))
  }

  test("Refine upper bound - simple example") {
    val expected = 9
    assert(expected == domain.refineUpperBound(10, tnum))
  }

  test("Refine upper bound - 0") {
    val expected = 0
    assert(expected == domain.refineUpperBound(0, tnum))
  }

  test("Refine tnum - simple example") {
    val expected = TNum(BitVecLiteral(8, 4), BitVecLiteral(1, 4))
    assert(expected == domain.refineTnum(8, 9, tnum))
  }

  test("Refine tnum - 0") {
    val expected = TNum(BitVecLiteral(0, 4), BitVecLiteral(0, 4))
    assert(expected == domain.refineTnum(0, 0, tnum))
  }

  test("Run on some code!") {
    val domain = TNumIntervalReducedProduct()
    val solver = worklistSolver(domain)
    reversePostOrder(littleProc)
    print(solver.solveProc(littleProc, backwards = false))
    assert(true)
  }

}
