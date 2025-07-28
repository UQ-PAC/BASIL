import analysis.data_structure_analysis.*
import analysis.data_structure_analysis.given
import boogie.SpecGlobal
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import specification.Specification
import test_util.{CaptureOutput, programToContext}
import util.*

@test_util.tags.UnitTest
class SVATest extends AnyFunSuite with CaptureOutput {

  def runAnalysis(program: Program): StaticAnalysisContext = {
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val emptySpec = Specification(Set(), Set(), Map(), List(), List(), List(), Set())
    val emptyContext = IRContext(List(), Set(), Set(), Set(), Map(), emptySpec, program)
    RunUtils.staticAnalysis(StaticAnalysisConfig(), emptyContext)
  }

  def runTest(context: IRContext): BASILResult = {
    RunUtils.loadAndTranslate(
      BASILConfig(
        context = Some(context),
        loading = ILLoadingConfig(inputFile = "", relfFile = None),
        simplify = SimplifyMode.Simplify,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = None // Some(DSAConfig(Set.empty))
      )
    )
  }

  test("malloc-OSet") {
    mallocTest[OSet]
  }

  test("malloc-Interval") {
    mallocTest[DSInterval]
  }

  def mallocTest[T <: Offsets](using domain: SymValSetDomain[T])(using oDomain: OffsetDomain[T]): Unit = {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val globalOffsets: Map[BigInt, BigInt] = Map.empty
    val globals: Set[SpecGlobal] = Set.empty

    val load = MemoryLoad(R0, mem, R0, Endian.LittleEndian, 64, Some("001"))
    val use = LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(10, 64)), Some("use"))
    val use1 = LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(10, 64)), Some("use1"))

    val program = prog(
      proc(
        "main",
        block("call", directCall("malloc"), goto("block1", "block2")),
        block("block1", use, goto("return")),
        block("block2", use1, goto("return")),
        block("return", ret)
      ),
      proc(
        "malloc", // fake malloc
        block("malloc_b", load, ret)
      )
    )

    val context = programToContext(program, globals, globalOffsets)
    val results = runTest(context)
    val mainProc = results.ir.program.mainProcedure
    val glbs = globalIntervals(context)
    val sva = getSymbolicValues(context, mainProc, glbs)
    val r0SVA = SymValues.getSorted(sva, "R0")
    val inParam = r0SVA.firstKey // TODO look into why there is an inParam
    assert(r0SVA(inParam) == domain.init(Par(mainProc, inParam)), "input param not set correctly")

    val mallocValSet = r0SVA.collectFirst {
      case (variable: LocalVar, valueSet: SymValSet[T])
          if valueSet.state.keys.exists(_.isInstanceOf[Heap]) && valueSet.state.size == 1 =>
        valueSet
    }.get // expect exactly 1 value set matching the case
    assert(mallocValSet.state.head(1).toOffsets == Set(0), "incorrect offset for malloc symbolic value")

    val outPram = r0SVA.lastKey
    assert(r0SVA(outPram) == SymValSet.transform(mallocValSet, i => i + 10), "should be malloc symValueSet oplus 10")
  }

  test("call-OSet") {
    callTest[OSet]
  }

  test("call-Interval") {
    callTest[DSInterval]
  }
  def callTest[T <: Offsets](using domain: SymValSetDomain[T])(using oDomain: OffsetDomain[T]): Unit = {
    val mem = SharedMemory("mem", 64, 8)
    val regName = "R0"
    val R0 = Register(regName, 64)
    val globalOffsets: Map[BigInt, BigInt] = Map.empty
    val globals: Set[SpecGlobal] = Set.empty

    val init = LocalAssign(R0, BitVecLiteral(20, 64))
    val load = MemoryLoad(R0, mem, R0, Endian.LittleEndian, 64, Some("001"))
    val assign = LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(10, 64)), Some("assign"))

    val program = prog(
      proc("main", block("call", init, directCall("callee"), goto("block")), block("block", assign, ret)),
      proc(
        "callee", // fake malloc
        block("callee_b", load, ret)
      )
    )

    val context = programToContext(program, globals, globalOffsets)
    val results = runTest(context)
    val main = program.mainProcedure
    val glbs = globalIntervals(context)
    val sva = getSymbolicValues[T](context, main, glbs) //  results.dsa.get.sva(mainProc)
    val r0SVA = SymValues.getSorted(sva, regName)

    val returnedValSet = r0SVA.collectFirst {
      case (variable: LocalVar, valueSet: SymValSet[T])
          if valueSet.state.keys.exists(_.isInstanceOf[Ret]) && valueSet.state.size == 1 =>
        valueSet
    }.get // expect exactly 1 value set matching the case
    assert(returnedValSet.state.head(1).toOffsets == Set(0), "incorrect offset for returned symbolic value")

    val outPram = r0SVA.lastKey
    assert(r0SVA(outPram) == SymValSet.transform(returnedValSet, i => i + 10), "should be return symValueSet oplus 10")
  }

  test("procEntry-OSet") {
    procEntry[OSet]
  }

  test("procEntry-Interval") {
    procEntry[DSInterval]
  }
  def procEntry[T <: Offsets](using domain: SymValSetDomain[T])(using oDomain: OffsetDomain[T]): Unit = {
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val R2 = Register("R2", 64)
    val R3 = Register("R3", 64)
    val bv64 = BitVecType(64)
    val globalOffsets: Map[BigInt, BigInt] = Map.empty
    val globals: Set[SpecGlobal] = Set.empty

    val assign1 = LocalAssign(R2, R0, Some("01"))
    val assign2 = LocalAssign(R3, R1, Some("02"))
//    val assign3 = LocalAssign(R0, BinaryExpr(BVADD, R2, BitVecLiteral(10, 64)), Some("assign"))

    val program = prog(proc("main", block("block", assign1, assign2, ret)))

    val context = programToContext(program, globals, globalOffsets)
    val main = program.mainProcedure
    val glbs = globalIntervals(context)

    runTest(context)
    val sva = getSymbolicValues[T](context, main, glbs) //  results.dsa.get.sva(mainProc)

    val R0in = LocalVar("R0_in", bv64)
    val R1in = LocalVar("R1_in", bv64)

    val R2out = LocalVar("R2_out", bv64)
    val R3out = LocalVar("R3_out", bv64)

    assert(main.formalInParam.contains(R0in), "Expected R0 as an input parameter")
    assert(main.formalInParam.contains(R1in), "Expected R1 as an input parameter")

    val domain = SymValSetDomain[T]()
    assert(sva(R0in) == domain.init(Par(main, R0in)), "Incorrect SymbolicValueSet for R0_in")
    assert(sva(R1in) == domain.init(Par(main, R1in)), "Incorrect SymbolicValueSet for R1_in")

    assert(sva(R0in) == sva(R2out), "Expected input param to propagate to output")
    assert(sva(R1in) == sva(R3out), "Expected input param to propagate to output")

  }

  test("reassignment-OSet") {
    reassignment[OSet]
  }

  test("reassignment-Interval") {
    reassignment[DSInterval]
  }
  def reassignment[T <: Offsets](using domain: SymValSetDomain[T])(using oDomain: OffsetDomain[T]): Unit = {
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val bv64 = BitVecType(64)
    val globalOffsets: Map[BigInt, BigInt] = Map.empty
    val globals: Set[SpecGlobal] = Set.empty

    val assign1 = LocalAssign(R1, R0, Some("01"))
    val assign2 = LocalAssign(R0, BinaryExpr(BVADD, R1, BitVecLiteral(10, 64)), Some("assign"))

    val program = prog(proc("main", block("block", assign1, assign2, ret)))

    val context = programToContext(program, globals, globalOffsets)
    val main = program.mainProcedure
    val glbs = globalIntervals(context)
    runTest(context)
    val sva = getSymbolicValues[T](context, main, glbs) //  results.dsa.get.sva(mainProc)

    val R0in = LocalVar("R0_in", bv64)

    val R0out = LocalVar("R0_out", bv64)
    val R1out = LocalVar("R1_out", bv64)

    assert(main.formalInParam.contains(R0in), "Expected R0 as an input parameter")

    val domain = SymValSetDomain[T]()
    assert(sva(R0in) == domain.init(Par(main, R0in)), "Incorrect SymbolicValueSet for R0_in")

    assert(sva(R0in) == sva(R1out), "Expected input param to propagate to output")
    assert(
      sva(R0out) == SymValSet.transform(sva(R0in), i => i + 10),
      "Expected updated input param to propagate to output"
    )
  }

  test("branch-OSet") {
    branch[OSet]
  }

  test("branch-Interval") {
    branch[DSInterval]
  }

  def branch[T <: Offsets](using domain: SymValSetDomain[T])(using oDomain: OffsetDomain[T]): Unit = {
    val R0 = Register("R0", 64)
    val bv64 = BitVecType(64)
    val globalOffsets: Map[BigInt, BigInt] = Map.empty
    val globals: Set[SpecGlobal] = Set.empty

    val assign1 = LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(10, 64)), Some("01"))
    val assign2 = LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(45, 64)), Some("02"))
    val assign3 = LocalAssign(R0, R0, Some("03"))

    val program = prog(
      proc(
        "main",
        block("split", goto(List("return", "branch1", "branch2"))),
        block("branch1", assign1, goto("return")),
        block("branch2", assign2, goto("return")),
        block(
          "return",
          assign3, // needed so that dead code won't kill the branch assignments
          ret
        )
      )
    )
    val R0in = LocalVar("R0_in", bv64)

    val context = programToContext(program, globals, globalOffsets)
    val glbs = globalIntervals(context)
    val main = program.mainProcedure
    runTest(context)
    val sva = getSymbolicValues[T](context, main, glbs) //  results.dsa.get.sva(mainProc)

    val domain = SymValSetDomain[T]()
    val (_, lastValSet) = SymValues.getSorted(sva, "R0").last

    assert(sva(R0in) == domain.init(Par(main, R0in)), "Incorrect SymbolicValueSet for R0_in")
    println(lastValSet)
    assert(
      lastValSet == domain.join(
        domain.join(sva(R0in), SymValSet.transform(sva(R0in), i => i + 10)),
        SymValSet.transform(sva(R0in), i => i + 45)
      ),
      "Incorrect set of offsets"
    )
  }

  test("loop-OSet") {
    loop[OSet]
  }

  test("loop-Interval") {
    loop[DSInterval]
  }
  def loop[T <: Offsets](using domain: SymValSetDomain[T])(using oDomain: OffsetDomain[T]): Unit = {
    val R0 = Register("R0", 64)
    val bv64 = BitVecType(64)
    val globalOffsets: Map[BigInt, BigInt] = Map.empty
    val globals: Set[SpecGlobal] = Set.empty

    val assign1 = LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(10, 64)), Some("01"))
    val assign2 = LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(45, 64)), Some("assign"))

    val program = prog(
      proc(
        "main",
        block("intro", goto("head")),
        block("head", goto("body", "return")),
        block("body", assign1, goto(List("head", "return"))),
        block("return", ret)
      )
    )

    val R0in = LocalVar("R0_in", bv64)

    val context = programToContext(program, globals, globalOffsets)
    val glbs = globalIntervals(context)
    val main = context.program.mainProcedure
    runTest(context)
    val sva = getSymbolicValues[T](context, main, glbs) //  results.dsa.get.sva(mainProc)
    val R0last = SymValues.getSorted(sva, "R0").lastKey

    val domain = SymValSetDomain[T]()
    assert(sva(R0in) == domain.init(Par(main, R0in)), "Incorrect SymbolicValueSet for R0_in")
    assert(sva(R0last) == SymValSet(sva(R0in).state.view.mapValues(_ => oDomain.top).toMap), "Incorrect set of offsets")
  }

  def load[T <: Offsets](using domain: SymValSetDomain[T])(using oDomain: OffsetDomain[T]): Unit = {
    val mem = SharedMemory("mem", 64, 8)
    val regName = "R0"
    val R0 = Register(regName, 64)
    val xAddress = BitVecLiteral(2000, 64)
    val xPointer = BitVecLiteral(1000, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val globals = Set(x)

    val load = MemoryLoad(R0, mem, xPointer, Endian.LittleEndian, 64, Some("001"))
    val assign = LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(10, 64)))

    val program = prog(proc("main", block("block", load, assign, ret)))

    val context = programToContext(program, globals, globalOffsets)
    val glbs = globalIntervals(context)

    val procedure: Procedure = program.mainProcedure
    runTest(context)
    val sva = getSymbolicValues[T](context, procedure, glbs) //  results.dsa.get.sva(mainProc)
    val r0SVA = SymValues.getSorted(sva, regName)

    val loadValSet = r0SVA.collectFirst {
      case (variable: LocalVar, valueSet: SymValSet[T])
          if valueSet.state.contains(Loaded(load)) && valueSet.state.size == 1 =>
        valueSet
    }.get // expect exactly 1 value set matching the case
    assert(loadValSet.state(Loaded(load)).toOffsets == Set(0), "incorrect offset for loaded symbolic value")

    val outPram = r0SVA.lastKey
    assert(r0SVA(outPram) == SymValSet.transform(loadValSet, i => i + 10), "should be load symValueSet oplus 10")
  }
}
