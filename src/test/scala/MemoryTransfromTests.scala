import analysis.data_structure_analysis.{Global, Interval}
import boogie.SpecGlobal
import ir.*
import org.scalatest.funsuite.AnyFunSuite
import specification.Specification
import util.DSAAnalysis.Norm
import util.{
  BASILConfig,
  BASILResult,
  BoogieGeneratorConfig,
  DSAConfig,
  ILLoadingConfig,
  IRContext,
  RunUtils,
  StaticAnalysisConfig,
  StaticAnalysisContext
}

@test_util.tags.UnitTest
class MemoryTransfromTests extends AnyFunSuite {
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
        dsaConfig = Some(DSAConfig(Set(Norm))),
        memoryTransform = true
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
        dsaConfig = Some(DSAConfig(Set(Norm))),
        memoryTransform = true
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

  test("global assignment") {
    val results = runTest("src/test/memory_transform/clasloc/clang/clasloc")

    val source = results.ir.program.nameToProcedure("source")
    val memoryAssigns = source.collect { case ma: MemoryAssign =>
      ma
    }
    assert(memoryAssigns.size == 1, "Expected Assignment to Z")
    val memoryAssign = memoryAssigns.head
    val global = memoryAssign.lhs
    val z = results.ir.globals
      .collectFirst { case SpecGlobal("z", size, arraySize, address) =>
        (Global, Interval(address.toInt, address.toInt + (size / 8)))
      }
      .get
      .toString()
    assert(global.name.contains(z), s"Expected variable to be named $z")
  }

}
