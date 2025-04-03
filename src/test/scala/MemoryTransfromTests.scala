import analysis.data_structure_analysis.{Global, Interval}
import boogie.SpecGlobal
import ir.*
import ir.Endian.{BigEndian, LittleEndian}
import ir.dsl.{block, directCall, goto, proc, prog, ret}
import org.scalatest.Ignore
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
    assert(
      memoryAssigns.map(_.lhs).toSet.size == 1,
      "global assignments across procs must be replace by same reference"
    )
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
    assert(
      memoryAssigns.map(_.lhs).toSet.size == 2,
      "global assignments across procs must be replace by different references"
    )
  }

  test("Local Assignment") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R31 = Register("R31", 64)
    val irType = BitVecType(64)

    val program = prog(
      proc(
        "main",
        Set(("R0", irType)),
        Set(("R0", irType)),
        block(
          "b",
          MemoryStore(mem, R31, R0, LittleEndian, 64, Some("01")),
          MemoryLoad(R0, mem, R31, LittleEndian, 64, Some("02")),
          ret(("R0", R0))
        )
      )
    )

    val context = programToContext(program)
    val results = runTest(context)

    val lassigns = results.ir.program.collect { case la: LocalAssign => la }
    assert(lassigns.size == 2)
    println(lassigns)
    val read = lassigns.collectFirst {
      case read @ LocalAssign(lhs, rhs, label) if lhs.name.startsWith("R0") => read
    }.get
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
      proc(
        "main",
        Set(("R0", irType), ("R1", irType)),
        Set(("R1", irType)),
        block("call", directCall(Set(("R0", R0)), "malloc", Set(("R0", R0))), goto("b")),
        block(
          "b",
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

    val load = results.ir.program.collect { case la: MemoryLoad => la }.head
    val store = results.ir.program.collect { case s: MemoryStore => s }.head
    assert(load.mem == store.mem)
    assert(load.mem != mem)
  }

  test("Multi pointers") {
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

    // R31 and x point to same region due to R0 being stored in both
    // Therefore stores and loads to either shouldn't be converted to scalar assignment
    val program = prog(
      proc(
        "main",
        Set(("R0", irType)),
        Set(("R0", irType)),
        block(
          "b",
          MemoryStore(mem, xAddress, R0, Endian.LittleEndian, 64, Some("00")),
          MemoryStore(mem, R31, R0, LittleEndian, 64, Some("01")),
          MemoryLoad(R0, mem, R31, LittleEndian, 64, Some("02")),
          ret(("R0", R0))
        )
      )
    )

    val context = programToContext(program)
    val results = runTest(context)

    val load = results.ir.program.collect { case la: MemoryLoad => la }.head
    val globalStore = results.ir.program.collect { case i: MemoryStore if i.index == xAddress => i }.head
    val stackStore = results.ir.program.collect { case i: MemoryStore if i.index.isInstanceOf[LocalVar] => i }.head
    assert(mem != load.mem, "memory should be transformed for the load")
    assert(globalStore.mem != mem, "memory should be transformed for the global mem Store")
    assert(stackStore.mem == load.mem, "load and store should have same regions for the same position on stack")

  }

  test("Multi pointer interprocedural") {
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

    // same as Multi Pointer but multiple pointers are split across procedures
    val program = prog(
      proc(
        "main",
        Set(("R0", irType)),
        Set(("R0", irType)),
        block(
          "m",
          MemoryStore(mem, xAddress, R0, Endian.LittleEndian, 64, Some("00")),
          directCall(Set(("R0", R0)), "callee", Set(("R0", R0))),
          ret(("R0", R0))
        )
      ),
      proc(
        "callee",
        Set(("R0", irType)),
        Set(("R0", irType)),
        block(
          "b",
          MemoryStore(mem, R31, R0, LittleEndian, 64, Some("01")),
          MemoryLoad(R0, mem, R31, LittleEndian, 64, Some("02")),
          ret(("R0", R0))
        )
      )
    )

    val context = programToContext(program)
    val results = runTest(context)
    val load = results.ir.program.collect { case la: MemoryLoad => la }.head
    val globalStore = results.ir.program.collect { case i: MemoryStore if i.index == xAddress => i }.head
    val stackStore = results.ir.program.collect { case i: MemoryStore if i.index.isInstanceOf[LocalVar] => i }.head
    assert(mem != load.mem, "memory should be transformed for the load")
    assert(globalStore.mem != mem, "memory should be transformed for the global mem Store")
    assert(stackStore.mem == load.mem, "load and store should have same regions for the same position on stack")
  }

  test("Mixed pointers") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val R2 = Register("R2", 64)
    val R31 = Register("R31", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val yAddress = BitVecLiteral(3000, 64)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val y = SpecGlobal("y", 64, None, yAddress.value)
    val globals = Set(x, y)
    val irType = BitVecType(64)

    // some regions have unique pointers (Callee R31 + 10, Global y) and other don't
    val program = prog(
      proc(
        "main",
        Set(("R0", irType), ("R1", irType), ("R2", irType)),
        Set(("R0", irType)),
        block(
          "m",
          MemoryStore(mem, yAddress, R2, LittleEndian, 64, Some("03")),
          MemoryStore(mem, xAddress, R0, Endian.LittleEndian, 64, Some("00")),
          directCall(Set(("R0", R0)), "callee", Set(("R0", R0), ("R1", R1))),
          ret(("R0", R0))
        )
      ),
      proc(
        "callee",
        Set(("R0", irType), ("R1", irType)),
        Set(("R0", irType)),
        block(
          "b",
          MemoryStore(mem, BinaryExpr(BVADD, R31, BitVecLiteral(10, 64)), R1, LittleEndian, 64, Some("04")),
          MemoryStore(mem, R31, R0, LittleEndian, 64, Some("01")),
          MemoryLoad(R0, mem, R31, LittleEndian, 64, Some("02")),
          ret(("R0", R0))
        )
      )
    )

    val context = programToContext(program, globals)
    val results = runTest(context)
    val loads = results.ir.program.collect { case la: MemoryLoad => la }
    val globalStores = results.ir.program.collect { case i: MemoryStore if i.index.isInstanceOf[BitVecLiteral] => i }
    val stackStores = results.ir.program.collect { case i: MemoryStore if i.index.isInstanceOf[LocalVar] => i }
    val ma = results.ir.program.collect { case i: MemoryAssign => i }
    val sca = results.ir.program.collect { case i: LocalAssign if i.lhs.name.startsWith("(Stack") => i }

    assert(loads.size == 1)
    assert(globalStores.size == 1)
    assert(stackStores.size == 1)
    assert(ma.size == 1)
    assert(sca.size == 1)
  }


  test("Multi region mixed pointer") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val R2 = Register("R2", 64)
    val R31 = Register("R31", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val yAddress = BitVecLiteral(3000, 64)
    val zAddress = BitVecLiteral(4000, 64)
    val gAddress = BitVecLiteral(5000, 64)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val y = SpecGlobal("y", 64, None, yAddress.value)
    val z = SpecGlobal("z", 64, None, zAddress.value)
    val g = SpecGlobal("g", 64, None, gAddress.value)
    val globals = Set(x, y, z, g)
    val irType = BitVecType(64)

    // some regions have unique pointers (Callee R31 + 10, Global y) and other don't
    val program = prog(
      proc(
        "main",
        Set(("R0", irType), ("R1", irType), ("R2", irType)),
        Set(("R0", irType)),
        block(
          "m",
          LocalAssign(R0, zAddress),
          MemoryStore(mem, xAddress, R0, Endian.LittleEndian, 64, Some("00")),
          MemoryStore(mem, yAddress, gAddress, LittleEndian, 64, Some("01")),
          directCall(Set(("R0", R0)), "callee", Set(("R0", R0), ("R1", R1))),
          ret(("R0", R0))
        )
      ),

      proc(
        "callee",
        Set(("R0", irType), ("R1", irType)),
        Set(("R0", irType)),
        block(
          "b",
          MemoryStore(mem, R31, R0, LittleEndian, 64, Some("02")),
          MemoryStore(mem, BinaryExpr(BVADD, R31, BitVecLiteral(10, 64)), gAddress, LittleEndian, 64, Some("03")),
          MemoryStore(mem, BinaryExpr(BVADD, R31, BitVecLiteral(20, 64)), R1, LittleEndian, 64, Some("04")),
          MemoryLoad(R0, mem, R31, LittleEndian, 64, Some("05")),
          ret(("R0", R0))
        )
      )
    )

    val context = programToContext(program,globals)
    val results = runTest(context)
  }
}
