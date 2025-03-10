import analysis.data_structure_analysis.{IntervalDSA, generateConstraints, getSymbolicValues}
import boogie.SpecGlobal
import ir.*
import ir.dsl.{block, proc, prog, ret}
import ir.{BitVecLiteral, Endian, MemoryLoad, Register, SharedMemory}
import org.scalatest.funsuite.AnyFunSuite
import specification.Specification
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
        dsaConfig = Some(DSAConfig(Set.empty))
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
        dsaConfig = Some(DSAConfig(Set.empty))
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

  test("Global Assignment") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val xPointer = BitVecLiteral(1000, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val globals = Set(x)

    val load = MemoryLoad(R0, mem, xPointer, Endian.LittleEndian, 64, Some("001"))

    val program = prog(proc("main", block("block", load, ret)))

    val context = programToContext(program, globals, globalOffsets)
    val basilResult = runTest(context)
    val main = basilResult.ir.program.mainProcedure

    val localGraph = IntervalDSA.getLocal(main, context, getSymbolicValues(main), generateConstraints(main))

    val test = 1
  }

}
