import analysis.data_structure_analysis.{IntervalDSA, generateConstraints, getSymbolicValues}
import boogie.SpecGlobal
import ir.*
import ir.dsl.{block, proc, prog, ret}
import ir.{BitVecLiteral, Endian, MemoryLoad, Register, SharedMemory}
import org.scalatest.funsuite.AnyFunSuite
import specification.Specification
import test_util.BASILTest.writeToFile
import util.DSAAnalysis.Norm
import util.RunUtils.{loadAndTranslate, staticAnalysis}
import util.{
  BASILConfig,
  BASILResult,
  BoogieGeneratorConfig,
  DSAAnalysis,
  DSAConfig,
  ILLoadingConfig,
  IRContext,
  RunUtils,
  StaticAnalysisConfig,
  StaticAnalysisContext
}

@test_util.tags.UnitTest
class IntervalDSATest extends AnyFunSuite {
  def runAnalysis(program: Program): StaticAnalysisContext = {
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val emptySpec = Specification(Set(), Set(), Map(), List(), List(), List(), Set())
    val emptyContext = IRContext(List(), Set(), Set(), Set(), Map(), emptySpec, program)
    RunUtils.staticAnalysis(StaticAnalysisConfig(), emptyContext)
  }

  def runTest(path: String): BASILResult = {
    RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(inputFile = path + ".adt", relfFile = path + ".relf"),
        simplify = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = Some(DSAConfig(Set(Norm)))
      )
    )
  }

  def runTest(context: IRContext): BASILResult = {
    RunUtils.loadAndTranslate(
      BASILConfig(
        context = Some(context),
        loading = ILLoadingConfig(inputFile = "", relfFile = ""),
        simplify = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = Some(DSAConfig(Set(Norm)))
      )
    )
  }

  def programToContext(
    program: Program,
    globals: Set[SpecGlobal] = Set.empty,
    globalOffsets: Map[BigInt, BigInt] = Map.empty
  ): IRContext = {
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val spec = Specification(Set(), globals, Map(), List(), List(), List(), Set())
    IRContext(List(), Set(), globals, Set(), globalOffsets, spec, program)
  }

  test("jumptable main") {
    val results = runTest("src/test/indirect_calls/jumptable/clang/jumptable")

    val dsg = results.dsa.get.topDown(results.ir.program.mainProcedure)
    dsg.localCorrectness()
  }

  test("Global Dereference") {
    val mem = SharedMemory("mem", 64, 8)
    val V0 = Register("V0", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val yAddress = BitVecLiteral(3000, 64)
    val xPointer = BitVecLiteral(1010, 64)
    val yPointer = BitVecLiteral(1018, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value, yPointer.value -> yAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val y = SpecGlobal("y", 64, None, yAddress.value)
    val globals = Set(x, y)

    val load = MemoryLoad(V0, mem, xPointer, Endian.LittleEndian, 64, Some("001"))

    val program = prog(proc("main", block("block", load, ret)))

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val context = programToContext(program, globals, globalOffsets)
    val basilResult = runTest(context)
    val main = basilResult.ir.program.mainProcedure

    val result =  runTest(context)

    val dsg = result.dsa.get.local(main)
    val xPointerCells = dsg.exprToCells(xPointer)
    assert(xPointerCells.size == 1)
    val xPointerCell = xPointerCells.head
    val xAddressCells = dsg.exprToCells(xAddress)
    assert(xAddressCells.size == 1)
    val xAddressCell = xAddressCells.head
    assert(xPointerCell.getPointee.equiv(xAddressCell))
  }

}
