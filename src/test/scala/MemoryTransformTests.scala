import boogie.SpecGlobal
import ir.*
import ir.Endian.LittleEndian
import ir.dsl.{block, directCall, goto, indirectCall, proc, prog, ret}
import org.scalatest.funsuite.AnyFunSuite
import test_util.BASILTest.programToContext
import test_util.{BASILTest, CaptureOutput}
import util.*

@test_util.tags.UnitTest
class MemoryTransformTests extends AnyFunSuite with CaptureOutput {

  def runTest(relativePath: String): BASILResult = {
    val path = s"${BASILTest.rootDirectory}/$relativePath"
    RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(inputFile = path + ".adt", relfFile = Some(path + ".relf")),
        simplify = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = Some(DSConfig()),
        memoryTransform = true
      )
    )
  }

  def runTest(context: IRContext): BASILResult = {
    RunUtils.loadAndTranslate(
      BASILConfig(
        context = Some(context),
        loading = ILLoadingConfig(inputFile = "", relfFile = None),
        simplify = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = Some(DSConfig()),
        memoryTransform = true
      )
    )
  }

  test("global assignment") {
    val results = runTest("src/test/memory_transform/clasloc/clang/clasloc")
    val source = results.ir.program.nameToProcedure("source")
    val memoryAssigns = source.collect { case ma: MemoryAssign => ma }
    assert(memoryAssigns.size == 1, "Expected Assignment to Z")
    val memoryAssign = memoryAssigns.head
    val global = memoryAssign.lhs
    val z = results.ir.globals.collectFirst { case g @ SpecGlobal("z", size, arraySize, address) => g }.get

    assert(global.name == s"Global_${z.address}_${z.address + (z.size / 8)}", s"Expected variable to be named $z")
  }

  test("multi proc global assignment") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val xPointer = BitVecLiteral(1000, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val globals = Set(x)

    val store1 = MemoryStore(mem, xAddress, BitVecLiteral(10, 64), Endian.LittleEndian, 64, Some("001"))
    val store2 = MemoryStore(mem, xAddress, BitVecLiteral(20, 64), Endian.LittleEndian, 64, Some("002"))

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
      proc("main", Set(("R0", irType)), Set(("R0", irType)))(
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
      case read @ LocalAssign(lhs, rhs, label) if lhs.name.contains("R0") => read
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
      proc("main", Set(("R0", irType), ("R1", irType)), Set(("R1", irType)))(
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
        Set(("R0", BitVecType(64)))
      )(block("malloc_b", m, ret(("R0", R0))))
    )

    program.nameToProcedure("malloc").isExternal = Some(true)
    val context = programToContext(program)
    val results = runTest(context)

    val load = results.ir.program.collect { case la: MemoryLoad => la }.head
    val store = results.ir.program.collect { case s: MemoryStore => s }.head
    assert(load.mem == store.mem)
    assert(load.mem != mem)
  }

  test("unified global/stack") {
    val mem = SharedMemory("mem", 64, 8)
    val R31 = Register("R31", 64)
    val R0 = Register("R0", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val yAddress = BitVecLiteral(3000, 64)
    val zAddress = BitVecLiteral(4000, 64)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val y = SpecGlobal("y", 64, None, yAddress.value)
    val z = SpecGlobal("z", 64, None, zAddress.value)
    val globals = Set(x, y, z)
    val irType = BitVecType(64)

    val program = prog(
      proc(
        "main",
        block(
          "b",
          MemoryStore(mem, xAddress, yAddress, Endian.LittleEndian, 64, Some("00")),
          MemoryStore(mem, xAddress, R31, LittleEndian, 64, Some("01")),
          MemoryLoad(R0, mem, xAddress, LittleEndian, 64, Some("02")),
          MemoryStore(mem, zAddress, R0, LittleEndian, 64, Some("03")),
          goto("k")
        ),
        block("h", goto("k")),
        block("k", ret),
        // indirect prevents unreachable blocks from being pruned.
        block("dummy", indirectCall(R0))
      )
    )

    val context = programToContext(program, globals)
    translating.PrettyPrinter.pp_prog(context.program)
    val results = runTest(context)

    val loads = results.ir.program.collect { case la: MemoryLoad => la }
    val stores = results.ir.program.collect { case m: MemoryStore => m }
    assert(stores.size == 3)
    assert(loads.size == 1)
    val load = loads.head
    val newMem = load.mem
    assert(newMem != mem)
    assert(stores.filter(_.index == xAddress).forall(_.mem == newMem))
    assert(stores.filter(_.index == zAddress).forall(i => i.mem != mem && i.mem != newMem))

  }

  test("escaped in-param") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val globals = Set(x)
    val irType = BitVecType(64)

    // same as Multi Pointer but multiple pointers are split across procedures
    val program = prog(
      proc("main", Set(("R0", irType)), Set(("R1", irType)))(
        block(
          "m",
          MemoryStore(mem, R0, xAddress, Endian.LittleEndian, 64, Some("00")),
          MemoryLoad(R1, mem, R0, LittleEndian, 64, Some("01")),
          ret(("R1", R1))
        )
      )
    )

    val context = programToContext(program)
    val results = runTest(context)

    val load = results.ir.program.collect { case la: MemoryLoad => la }.head
    val store = results.ir.program.collect { case m: MemoryStore => m }.head
    assert(load.mem == mem)
    assert(store.mem == mem)
  }

  test("escaped out-param") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val globals = Set(x)
    val irType = BitVecType(64)

    // same as Multi Pointer but multiple pointers are split across procedures
    val program = prog(
      proc("main", Set(("R0", irType)), Set(("R1", irType)))(
        block("a", directCall(Set(("R1", R0)), "callee", Set(("R0", R0))), goto("b")),
        block(
          "b",
          MemoryStore(mem, R1, xAddress, LittleEndian, 64, Some("00")),
          MemoryLoad(R1, mem, R1, LittleEndian, 64, Some("01")),
          ret(("R1", R0))
        )
      ),
      proc("callee", Set(("R0", irType)), Set(("R1", irType)))(
        block("c", MemoryLoad(R1, mem, R0, LittleEndian, 64, Some("01")), ret(("R1", R0)))
      )
    )

    val context = programToContext(program)
    val results = runTest(context)

    val load = results.ir.program.mainProcedure.collect { case la: MemoryLoad => la }.head
    val store = results.ir.program.mainProcedure.collect { case m: MemoryStore => m }.head
    assert(load.mem == mem)
    assert(store.mem == mem)
  }

  test("Multi regions") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val globals = Set(x)
    val irType = BitVecType(64)

    // same as Multi Pointer but multiple pointers are split across procedures
    val program = prog(
      proc("main", Set(("R0", irType)), Set())(
        block("a", directCall(Set(("R0", R0)), "malloc", Set(("R0", R0)), Some("heap1")), goto("b")),
        block(
          "b",
          MemoryStore(mem, R0, xAddress, LittleEndian, 64, Some("01")),
          directCall(Set(("R0", R0)), "malloc", Set(("R0", R0)), Some("heap2")),
          goto("c")
        ),
        block("c", MemoryStore(mem, R0, xAddress, LittleEndian, 64, Some("02")), ret)
      ),
      proc("malloc", Set(("R0", BitVecType(64))), Set(("R0", BitVecType(64))))(
        block("malloc_b", MemoryLoad(R0, mem, R0, LittleEndian, 64, None), ret(("R0", R0)))
      )
    )

    program.nameToProcedure("malloc").isExternal = Some(true)
    val context = programToContext(program)
    val results = runTest(context)

    val stores = results.ir.program.mainProcedure.collect { case m: MemoryStore => m }
    assert(stores.size == 2)
    assert(stores.forall(_.mem != mem))
    assert(stores.map(_.mem).toSet.size == 2)
  }

  test("global escape") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val R2 = Register("R2", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val globals = Set(x)
    val irType = BitVecType(64)

    // same as Multi Pointer but multiple pointers are split across procedures
    val program = prog(
      proc("main", Set(("R0", irType)), Set())(
        block("a", directCall(Set(("R0", R0)), "malloc", Set(("R0", R0)), Some("heap1")), goto("b", "c")),
        block("b", MemoryStore(mem, xAddress, R0, LittleEndian, 64, Some("01")), directCall("callee1"), goto("d")),
        block("c", MemoryStore(mem, xAddress, R0, LittleEndian, 64, Some("02")), directCall("callee2"), goto("d")),
        block("d", ret)
      ),
      proc("malloc", Set(("R0", BitVecType(64))), Set(("R0", BitVecType(64))))(
        block("malloc_b", MemoryLoad(R0, mem, R0, LittleEndian, 64, None), ret(("R0", R0)))
      ),
      proc(
        "callee1",
        block(
          "c1b",
          MemoryLoad(R1, mem, xAddress, LittleEndian, 64, Some("03")),
          MemoryStore(mem, R1, BitVecLiteral(10, 64), LittleEndian, 64, Some("04")),
          LocalAssign(R1, BitVecLiteral(30, 64), Some("07")),
          ret
        )
      ),
      proc(
        "callee2",
        block(
          "c2b",
          MemoryLoad(R1, mem, xAddress, LittleEndian, 64, Some("05")),
          MemoryStore(mem, R1, BitVecLiteral(20, 64), LittleEndian, 64, Some("06")),
          LocalAssign(R1, BitVecLiteral(30, 64), Some("08")),
          ret
        )
      )
    )

    program.nameToProcedure("malloc").isExternal = Some(true)
    val context = programToContext(program, globals)
    val results = runTest(context)

    val mainStores = results.ir.program.mainProcedure.collect { case m: MemoryAssign => m }
    val loads = results.ir.program.collect { case l @ LocalAssign(_, r: GlobalVar, _) => l }
    assert(mainStores.map(_.lhs).toSet.size == 1)
    assert(loads.map(_.rhs).toSet.size == 1)
    assert(loads.map(_.rhs).toSet.head == mainStores.map(_.lhs).toSet.head)
    val callee1Store = results.ir.program.nameToProcedure("callee1").collect { case m: MemoryStore => m }.head
    val callee2Store = results.ir.program.nameToProcedure("callee2").collect { case m: MemoryStore => m }.head
    assert(callee2Store.mem == callee1Store.mem)
    assert(callee1Store.mem != mem)

  }

}
