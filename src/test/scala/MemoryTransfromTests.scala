import analysis.data_structure_analysis.{Global, Interval}
import boogie.SpecGlobal
import ir.*
import ir.Endian.{BigEndian, LittleEndian}
import ir.dsl.{block, directCall, goto, proc, prog, ret}
import org.scalatest.funsuite.AnyFunSuite
import specification.Specification
import util.*
import util.DSAAnalysis.Norm

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
    val memoryAssigns = source.collect { case ma: MemoryAssign => ma }
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

  test("multi proc global assignment") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val xPointer = BitVecLiteral(1000, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val globals = Set(x)

    val store1 = MemoryStore(mem, xAddress, BitVecLiteral(10000, 64), Endian.LittleEndian, 64, Some("001"))
    val store2 = MemoryStore(mem, xAddress, BitVecLiteral(40000, 64), Endian.LittleEndian, 64, Some("002"))

    val program = prog(
      proc(
        "main",
        block("start", goto("caller1", "caller2")),
        block("caller1", directCall("func1"), goto("end")),
        block("caller2", directCall("func2"), goto("end")),
        block("end", ret)
      ),
      proc("func1", block("block", store1, ret)),
      proc("func2", block("block", store2, ret))
    )

    val context = programToContext(program, globals, globalOffsets)

    val results = runTest(context)

    val memoryAssigns = results.ir.program.collect { case ma: MemoryAssign => ma }
    assert(memoryAssigns.size == 2)
    assert(memoryAssigns.map(_.lhs).toSet.size == 1, "global assignments across procs must be replace by same reference")
  }

  test("multi global assignment") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val yAddress = BitVecLiteral(3000, 64)
    val xPointer = BitVecLiteral(1000, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val y = SpecGlobal("y", 64, None, yAddress.value)
    val globals = Set(x, y)

    val store1 = MemoryStore(mem, xAddress, BitVecLiteral(10000, 64), Endian.LittleEndian, 64, Some("001"))
    val store2 = MemoryStore(mem, yAddress, BitVecLiteral(40000, 64), Endian.LittleEndian, 64, Some("002"))

    val program = prog(
      proc(
        "main",
        block("start", goto("caller1", "caller2")),
        block("caller1", directCall("func1"), goto("end")),
        block("caller2", directCall("func2"), goto("end")),
        block("end", ret)
      ),
      proc("func1", block("block", store1, ret)),
      proc("func2", block("block", store2, ret))
    )

    val context = programToContext(program, globals, globalOffsets)

    val results = runTest(context)

    val memoryAssigns = results.ir.program.collect { case ma: MemoryAssign => ma }
    assert(memoryAssigns.size == 2)
    assert(memoryAssigns.map(_.lhs).toSet.size == 2, "global assignments across procs must be replace by different references")
  }

  test("Local Assignment") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R31 = Register("R31", 64)
    val irType = BitVecType(64)

    val program = prog(
      proc("main", Set(("R0", irType)), Set(("R0", irType)),
        block("b",
          MemoryStore(mem, R31, R0, LittleEndian, 64, Some("01")),
          MemoryLoad(R0, mem, R31, LittleEndian, 64, Some("02")),
          ret(("R0", R0))
        )
      )
    )

    val context = programToContext(program)
    val results = runTest(context)

    val lassigns = results.ir.program.collect {case la: LocalAssign => la}
    assert(lassigns.size == 2)
    println(lassigns)
    val read = lassigns.collectFirst {case read @ LocalAssign(lhs, rhs, label) if lhs.name.startsWith("R0") => read}.get
    val write = lassigns.filterNot(_ == read).head
    assert(read.rhs == write.lhs)
  }

  test("Heap Assignment") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val R31 = Register("R31", 64)
    val m = MemoryLoad(R0, mem, R0, Endian.LittleEndian, 64, Some("load"))

    val irType = BitVecType(64)

    val program = prog(
      proc("main", Set(("R0", irType), ("R1", irType)), Set(("R1", irType)),
        block("call",
          directCall(Set(("R0", R0)), "malloc", Set(("R0", R0))),
          goto("b")
        ),
        block("b",
          MemoryStore(mem, R0, R1, LittleEndian, 64, Some("01")),
          MemoryLoad(R0, mem, R0, LittleEndian, 64, Some("02")),
          ret(("R1", R0))
        )
      ),
     proc(
        "malloc", // fake malloc
        Set(("R0", BitVecType(64))),
        Set(("R0", BitVecType(64))),
        block("malloc_b", m, ret(("R0", R0)))
      )
    )

    val context = programToContext(program)
    val results = runTest(context)


    val load = results.ir.program.collect {case la: MemoryLoad => la}.head
    val store = results.ir.program.collect {case s: MemoryStore => s}.head
    assert(load.mem == store.mem)
  }

  test("Unified Global/Stack") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R31 = Register("R31", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val yAddress = BitVecLiteral(3000, 64)
    val xPointer = BitVecLiteral(1000, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val y = SpecGlobal("y", 64, None, yAddress.value)
    val globals = Set(x, y)
    val irType = BitVecType(64)

    val program = prog(
      proc("main", Set(("R0", irType)), Set(("R0", irType)),
        block("b",
          MemoryStore(mem, xAddress, R0, Endian.LittleEndian, 64, Some("00")),
          MemoryStore(mem, R31, R0, LittleEndian, 64, Some("01")),
          MemoryLoad(R0, mem, R31, LittleEndian, 64, Some("02")),
          ret(("R0", R0))
        )
      )
    )

    val context = programToContext(program)
    val results = runTest(context)

    val load = results.ir.program.collect { case la: MemoryLoad => la}
    val stores = results.ir.program.collect {case i: MemoryStore => i}
    assert(load.nonEmpty, "multiple local pointers, shouldn't be converted to scalar variable")
    assert(stores.nonEmpty, "multiple local pointers, shouldn't be converted to scalar variable")
  }
}
