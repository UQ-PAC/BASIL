import analysis.{StronglyLiveBitsEdgeFunctionLattice, StronglyLiveBitsLattice, StronglyLiveBitsAnalysis,
  TwoElement, TwoElementBottom, TwoElementTop}
import ir.BitVecType
import org.scalatest.funsuite.AnyFunSuite
import test_util.{BASILTest, CaptureOutput}
import ir.{dsl, *}
import ir.dsl.*

class StronglyLiveBitAnalysisTests extends AnyFunSuite, CaptureOutput, BASILTest{

  val lattice = StronglyLiveBitsLattice()
  val bot: TwoElement = TwoElementBottom
  val top: TwoElement = TwoElementTop

  val edgeFuncLat = StronglyLiveBitsEdgeFunctionLattice(lattice)

  // Lattice Tests
  test("Shifting Empty map - should return Empty map") {
    val result = lattice.shiftMap(Set(1), Map().withDefaultValue(bot))
    assert(result == Map().withDefaultValue(bot))
  }

  test("Shifting a Map with 1 element to the left by one - should return shifted map") {
    val result = lattice.shiftMap(Set(1), Map(1->top).withDefaultValue(bot))
    assert(result == Map(2->top).withDefaultValue(bot))
  }

  test("Shifting a Map with a set of two elements - should return a set of two elements") {
    val result = lattice.shiftMap(Set(1, 2), Map(1->top).withDefaultValue(bot))
    assert(result == Map(2->top, 3->top).withDefaultValue(bot))
  }

  // End lattice tests
  // Start EdgeFunction Tests

  test("The composition of an empty constant edge with empty constant edge - should return an empty constant edge") {
    val result = edgeFuncLat.ConstEdge(Map().withDefaultValue(bot)).composeWith(
      edgeFuncLat.ConstEdge(Map().withDefaultValue(bot)))
    assert(result == edgeFuncLat.ConstEdge(Map().withDefaultValue(bot)))
  }

  // Actual tests


  def basicIntraCase(): Unit = {
    val program = prog(
      proc("main",
        block(
          "main",
          LocalAssign(LocalVar("R1", BitVecType(64), 0), BitVecLiteral(BigInt("7"), 32)),
          LocalAssign(LocalVar("R0", BitVecType(64), 2), ZeroExtend(32, Extract(32, 0, LocalVar("R1", BitVecType(64), 0)))),
          goto("main_return")
        ),
        block(
          "main_return",
          ret("R0_out" -> LocalVar("R0", BitVecType(64), 2))
        ))
    )
    val liveBitsAnalysisResults = StronglyLiveBitsAnalysis(program).analyze()
    println(liveBitsAnalysisResults(program.mainProcedure))

    assert(liveBitsAnalysisResults(program.mainProcedure) == Map(R0 -> Map()))
  }
  test("Basic Intra Live Bits"){
    basicIntraCase()
 }
}
