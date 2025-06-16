import analysis.data_structure_analysis.DSAPhase.{Local, TD}
import analysis.data_structure_analysis.{
  DSAPhase,
  DSInterval,
  Global,
  Heap,
  IntervalDSA,
  IntervalGraph,
  Par,
  Ret,
  Stack,
  SymBase,
  generateConstraints,
  getSymbolicValues,
  given
}
import boogie.SpecGlobal
import ir.*
import ir.Endian.LittleEndian
import ir.dsl.{block, directCall, goto, proc, prog, ret}
import ir.{BitVecLiteral, Endian, MemoryLoad, Register, SharedMemory}
import org.scalatest.funsuite.AnyFunSuite
import specification.Specification
import util.*
import analysis.data_structure_analysis
import test_util.{BASILTest, CaptureOutput}

object IntervalDSATestData {
  val mem = SharedMemory("mem", 64, 8)
  val xAddress = BitVecLiteral(2000, 64)
  val yAddress = BitVecLiteral(3000, 64)
  val zAddress = BitVecLiteral(4000, 64)
  val kAddress = BitVecLiteral(5000, 64)
  val eight = BitVecLiteral(8, 64)
  val x = SpecGlobal("x", 128, None, xAddress.value)
  val y = SpecGlobal("y", 128, None, yAddress.value)
  val z = SpecGlobal("z", 64, None, zAddress.value)
  val k = SpecGlobal("k", 64, None, kAddress.value)
  val R0 = Register("R0", 64)
  val R1 = Register("R1", 64)
  val R2 = Register("R2", 64)
  val bv64 = BitVecType(64)

  def recursion: IRContext = {
    val program =
      prog(
        proc(
          "main",
          Set(("R0", bv64)),
          Set(("R0", bv64)),
          block(
            "en",
            LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(1, 64)), Some("01")),
            directCall(Set(("R0", R0)), "main", Set(("R0", R0))),
            ret(("R0", R0))
          )
        )
      )
    programToContext(program, Set.empty)
  }

  def recursionWithIndirection: IRContext = {
    val program =
      prog(
        proc(
          "main",
          Set(("R0", bv64)),
          Set(("R0", bv64)),
          block(
            "en",
            MemoryLoad(R1, mem, R0, LittleEndian, 64),
            LocalAssign(R2, BinaryExpr(BVADD, R1, BitVecLiteral(8, 64)), Some("01")),
            MemoryStore(mem, R0, R2, LittleEndian, 64),
            directCall(Set(("R0", R0)), "main", Set(("R0", R0))),
            ret(("R0", R0))
          )
        )
      )
    programToContext(program, Set.empty)
  }

  def mutualRecursion: IRContext = {
    val program = prog(
      proc(
        "main",
        Set(("R0", bv64)),
        Set(("R0", bv64)),
        block(
          "en",
          LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(1, 64)), Some("01")),
          directCall(Set(("R0", R0)), "callee", Set(("R0", R0))),
          ret(("R0", R0))
        )
      ),
      proc(
        "callee",
        Set(("R0", bv64)),
        Set(("R0", bv64)),
        block(
          "en",
          LocalAssign(R0, BinaryExpr(BVADD, R0, BitVecLiteral(1, 64)), Some("01")),
          directCall(Set(("R0", R0)), "main", Set(("R0", R0))),
          ret(("R0", R0))
        )
      )
    )
    programToContext(program, Set.empty)
  }

  def globalBranch: IRContext = {
    val globals = Set(x, y, z, k)
    val program =
      prog(
        proc(
          "main",
          block("entry", goto("a", "b")),
          block("a", LocalAssign(R0, xAddress, Some("01")), goto("c")),
          block("b", LocalAssign(R0, yAddress, Some("02")), goto("c")),
          block("c", MemoryStore(mem, zAddress, R0, LittleEndian, 64, Some("03")), ret)
        )
      )

    programToContext(program, globals)
  }

  def globalBranchIndirectUse: IRContext = {
    val globals = Set(x, y, z, k)
    val program =
      prog(
        proc(
          "main",
          block("entry", goto("a", "b")),
          block("a", LocalAssign(R0, xAddress, Some("01")), goto("c")),
          block("b", LocalAssign(R0, yAddress, Some("02")), goto("c")),
          block(
            "c",
            MemoryStore(mem, zAddress, R0, LittleEndian, 64, Some("03")),
            MemoryLoad(R0, mem, zAddress, LittleEndian, 64, Some("04")),
            MemoryStore(mem, kAddress, BinaryExpr(BVADD, R0, eight), LittleEndian, 64, Some("05")),
            ret
          )
        )
      )
    programToContext(program, globals)
  }
}

def runAnalysis(program: Program): StaticAnalysisContext = {
  cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
  transforms.addReturnBlocks(program)
  cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

  val emptySpec = Specification(Set(), Set(), Map(), List(), List(), List(), Set())
  val emptyContext = IRContext(List(), Set(), Set(), Set(), Map(), emptySpec, program)
  RunUtils.staticAnalysis(StaticAnalysisConfig(), emptyContext)
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

def globalsToLiteral(ctx: IRContext) = {
  ctx.globals.map(g => (g.name, BitVecLiteral(g.address, 64))).toMap
    ++ (ctx.funcEntries.map(f => (f.name, BitVecLiteral(f.address.toInt, 64))).toMap)
}

@test_util.tags.AnalysisSystemTest3
class IntervalDSATest extends AnyFunSuite with test_util.CaptureOutput {

  def runTest(relativePath: String, main: Option[String] = None, config: DSConfig = DSConfig()): BASILResult = {
    val path = s"${BASILTest.rootDirectory}/$relativePath"
    RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = path + ".gts",
          relfFile = path + ".relf",
          trimEarly = main.isDefined,
          mainProcedureName = main.getOrElse("main")
        ),
        simplify = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = Some(config)
      )
    )
  }

  def runTestPrg(context: IRContext, config: DSConfig = DSConfig()): BASILResult = {
    RunUtils.loadAndTranslate(
      BASILConfig(
        context = Some(context),
        loading = ILLoadingConfig(inputFile = "", relfFile = ""),
        simplify = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = Some(config)
      )
    )
  }

  test("recursion") {
    val result = runTestPrg(IntervalDSATestData.recursion)
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    assert(dsg.exprToCells(dsg.proc.formalInParam.head).forall(_.node.isCollapsed))
  }

  test("recursion eq cells") {
    val result = runTestPrg(IntervalDSATestData.recursion, DSConfig(TD, eqClasses = true))
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    assert(dsg.exprToCells(dsg.proc.formalInParam.head).forall(_.node.isCollapsed))
  }

  test("mutual recursion") {
    val result = runTestPrg(IntervalDSATestData.mutualRecursion)
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    val dsgCallee = result.dsa.get.topDown(result.ir.program.nameToProcedure("callee"))
    assert(dsg == dsgCallee)
    assert(dsg.exprToCells(dsg.proc.formalInParam.head).forall(_.node.isCollapsed))
  }

  test("recursion with indirection") {
    val result = runTestPrg(IntervalDSATestData.recursionWithIndirection)
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    assert(dsg.exprToCells(dsg.proc.formalInParam.head).forall(_.getPointee.node.isCollapsed))
  }

  // Eq classes counter example
  ignore("recursion with indirection eq cells") {
    val result = runTestPrg(IntervalDSATestData.recursionWithIndirection, DSConfig(TD, eqClasses = true))
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    val t = (dsg.exprToCells(dsg.proc.formalInParam.head).flatMap(i => dsg.cellToEq(i.getPointee)))
    assert(dsg.exprToCells(dsg.proc.formalInParam.head).forall(_.getPointee.node.isCollapsed))
  }

  test("global branch") {
    val result = runTestPrg(IntervalDSATestData.globalBranch)
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    val globals = globalsToLiteral(result.ir).values.flatMap(i => dsg.exprToCells(i)).map(dsg.get).toSet
    assert(globals.size == 1)
    assert(globals.head.node.isCollapsed)
    assert(globals.head.getPointee.equiv(globals.head))
  }

  test("global branch split globals") {
    val result = runTestPrg(IntervalDSATestData.globalBranch, DSConfig(TD, true, true))
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    val globals = globalsToLiteral(result.ir)
    val z = dsg.exprToCells(globals("z")).map(dsg.find).head
    val x = dsg.exprToCells(globals("x")).map(dsg.find).head
    val y = dsg.exprToCells(globals("y")).map(dsg.find).head
    val x8 = dsg.exprToCells(BinaryExpr(BVADD, globals("x"), BitVecLiteral(8, 64))).map(dsg.find).head
    val y8 = dsg.exprToCells(BinaryExpr(BVADD, globals("y"), BitVecLiteral(8, 64))).map(dsg.find).head
    assert(!z.equiv(x))
    assert(z.getPointee.equiv(x))
    assert(y.equiv(x))
    assert(!x8.equiv(x))
    assert(x8.equiv(y8))
  }

  test("global branch eq classes") {
    val result = runTestPrg(IntervalDSATestData.globalBranch, DSConfig(TD, eqClasses = true))
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    val globals = globalsToLiteral(result.ir)
    val z = dsg.exprToCells(globals("z")).map(dsg.find).head
    val x = dsg.exprToCells(globals("x")).map(dsg.find)
    val y = dsg.exprToCells(globals("y")).map(dsg.find)
    val x8 = dsg.exprToCells(BinaryExpr(BVADD, globals("x"), BitVecLiteral(8, 64))).map(dsg.find).head
    val y8 = dsg.exprToCells(BinaryExpr(BVADD, globals("y"), BitVecLiteral(8, 64))).map(dsg.find).head
    assert(x.forall(c => !c.equiv(z)))
    assert(x.forall(c => z.getPointee.equiv(c)))
    assert(x == y)
    assert(x.size == 2)
    assert(x.forall(c => !c.equiv(x8)))
    assert(x.forall(c => !c.equiv(y8)))
    assert(!x8.equiv(y8)) // don't expect them to be unified
    val join = dsg.proc.formalOutParam.head
    assert(dsg.exprToCells(join) == x)
    assert(dsg.exprToCells(BinaryExpr(BVADD, join, BitVecLiteral(8, 64))) == Set(x8, y8))
  }

  test("global branch Indirect Use eq classes") {
    val result = runTestPrg(IntervalDSATestData.globalBranchIndirectUse, DSConfig(TD, eqClasses = true))
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    val globals = globalsToLiteral(result.ir)
    val z = dsg.exprToCells(globals("z")).map(dsg.find).head
    val x = dsg.exprToCells(globals("x")).map(dsg.find)
    val y = dsg.exprToCells(globals("y")).map(dsg.find)
    val x8 = dsg.exprToCells(BinaryExpr(BVADD, globals("x"), BitVecLiteral(8, 64))).map(dsg.find).head
    val y8 = dsg.exprToCells(BinaryExpr(BVADD, globals("y"), BitVecLiteral(8, 64))).map(dsg.find).head
    assert(x.forall(c => !c.equiv(z)))
    assert(x.forall(c => z.getPointee.equiv(c)))
    assert(x == y)
    assert(x.size == 2)
    assert(x.forall(c => !c.equiv(x8)))
    assert(x.forall(c => !c.equiv(y8)))
    assert(x8.equiv(y8))
  }

  test("global branch split globals and eq classes") {
    val result = runTestPrg(IntervalDSATestData.globalBranch, DSConfig(TD, true, true, true))
    val dsg = result.dsa.get.topDown(result.ir.program.mainProcedure)
    val globals = globalsToLiteral(result.ir)
    val z = dsg.exprToCells(globals("z")).map(dsg.find).head
    val x = dsg.exprToCells(globals("x")).map(dsg.find).head
    val y = dsg.exprToCells(globals("y")).map(dsg.find).head
    val x8 = dsg.exprToCells(BinaryExpr(BVADD, globals("x"), BitVecLiteral(8, 64))).map(dsg.find).head
    val y8 = dsg.exprToCells(BinaryExpr(BVADD, globals("y"), BitVecLiteral(8, 64))).map(dsg.find).head
    assert(!z.equiv(x))
    assert(z.getPointee.equiv(x))
    assert(y.equiv(x))
    assert(!x8.equiv(x))
    assert(x8.equiv(y8))
  }

  test("Global Dereference") {
    val mem = SharedMemory("mem", 64, 8)
    val V0 = Register("V0", 64)
    val xAddress = BitVecLiteral(2000, 64)
    val yAddress = BitVecLiteral(3000, 64)
    val xPointer = BitVecLiteral(1000, 64)
    val yPointer = BitVecLiteral(1008, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value, yPointer.value -> yAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val y = SpecGlobal("y", 64, None, yAddress.value)
    val globals = Set(x, y)

    val load = MemoryLoad(V0, mem, xPointer, Endian.LittleEndian, 64, Some("001"))

    val program = prog(proc("main", block("block", load, ret)))

    val context = programToContext(program, globals, globalOffsets)
    val result = runTestPrg(context)

    val dsg = result.dsa.get.local(result.ir.program.mainProcedure)
    val xPointerCells = dsg.exprToCells(xPointer)
    assert(xPointerCells.size == 1)
    val xPointerCell = xPointerCells.head
    val xAddressCells = dsg.exprToCells(xAddress)
    assert(xAddressCells.size == 1)
    val xAddressCell = xAddressCells.head
    assert(xPointerCell.getPointee.equiv(xAddressCell))
  }

  test("alias") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val R2 = Register("R2", 64)

    val xAddress = BitVecLiteral(2000, 64)
    val yAddress = BitVecLiteral(3000, 64)
    val xPointer = BitVecLiteral(1010, 64)
    val yPointer = BitVecLiteral(1018, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value, yPointer.value -> yAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val y = SpecGlobal("y", 64, None, yAddress.value)
    val globals = Set(x, y)

    val irType = BitVecType(64)

    val program = prog(
      proc(
        "main",
        Set(("R0", irType)),
        Set(("R0", irType), ("R1", irType)),
        block(
          "en",
          LocalAssign(R1, R0, Some("01")),
          directCall(Set(("R0", R0), ("R1", R1)), "callee", Set(("R0", R0), ("R1", R1), ("R2", R2))),
          ret(("R0", R0), ("R1", R1))
        )
      ),
      proc(
        "callee",
        Set(("R0", irType), ("R1", irType), ("R2", irType)),
        Set(("R0", irType), ("R1", irType)),
        block(
          "calleeEn",
          MemoryStore(mem, R0, xAddress, LittleEndian, 64, Some("02")),
          MemoryStore(mem, R1, R2, LittleEndian, 64, Some("03")),
          ret(("R0", R0), ("R1", R1))
        )
      )
    )

    val context = programToContext(program, globals, globalOffsets)
    val basilResult = runTestPrg(context)
    val main = basilResult.ir.program.mainProcedure
    val callee = basilResult.ir.program.nameToProcedure("callee")
    val dsgMain = basilResult.dsa.get.topDown(main)
    val dsgCallee = basilResult.dsa.get.topDown(callee)
    val in = dsgMain.get(dsgMain.nodes(Par(main, LocalVar("R0_in", irType))).get(0))
    val calleein1 = dsgCallee.get(dsgCallee.nodes(Par(callee, LocalVar("R0_in", irType))).get(0))
    val calleein2 = dsgCallee.get(dsgCallee.nodes(Par(callee, LocalVar("R1_in", irType))).get(0))

    assert(calleein1 == calleein2)
    assert(in.interval == calleein1.interval)
    assert(in.node.bases == calleein1.node.bases)
  }

  /**
   * checks dsa-context sensitivity
   * two heap regions are expected to be distinct in caller main
   * but the corresponding ret regions from the caller are expected to be unified in the callee
   */
  test("disjoint in caller") {
    val mem = SharedMemory("mem", 64, 8)
    val R0 = Register("R0", 64)
    val R1 = Register("R1", 64)
    val R2 = Register("R2", 64)
    val R3 = Register("R3", 64)
    val r1Assign = LocalAssign(R1, R0, Some("01"))
    val r2Assign = LocalAssign(R2, R0, Some("02"))
    val load = MemoryLoad(R0, mem, R0, Endian.LittleEndian, 64, Some("load"))
    val store = MemoryStore(mem, R0, R0, LittleEndian, 64, Some("store"))
    val store1 = MemoryStore(mem, R3, R1, LittleEndian, 64, Some("store1"))
    val store2 = MemoryStore(mem, R3, R2, LittleEndian, 64, Some("store2"))
    val mallocCall = directCall(Set(("R0", R0)), "malloc", Set(("R0", R0)))

    val program =
      prog(
        proc(
          "main",
          Set(("R0", BitVecType(64))),
          Set(("R0", BitVecType(64)), ("R1", BitVecType(64)), ("R2", BitVecType(64))),
          block("entry", mallocCall, goto("b1", "b2")),
          block(
            "b1",
            LocalAssign(R1, R0, Some("01")),
            directCall(Set(("R0", R0)), "wmalloc", Set(("R0", R0))),
            goto("exit")
          ),
          block(
            "b2",
            LocalAssign(R2, R0, Some("02")),
            directCall(Set(("R0", R0)), "wmalloc", Set(("R0", R0))),
            goto("exit")
          ),
          block(
            "exit",
            LocalAssign(R1, R0, Some("03")),
            LocalAssign(R2, R0, Some("04")),
            ret(("R0", R0), ("R1", R0), ("R2", R0))
          )
        ),
        proc(
          "wmalloc",
          Set(("R0", BitVecType(64))),
          Set(("R0", BitVecType(64))),
          block("en", mallocCall, ret(("R0", R0)))
        ),
        proc(
          "malloc", // fake malloc
          Set(("R0", BitVecType(64))),
          Set(("R0", BitVecType(64))),
          block("malloc_b", load, ret(("R0", R0)))
        )
      )

    val context = programToContext(program, Set.empty, Map.empty)
    val malloc = context.program.nameToProcedure("malloc")
    malloc.isExternal = Some(true)
    val results = runTestPrg(context)
    val mainDSG = results.dsa.get.topDown(results.ir.program.mainProcedure)

    val wmallocDSG = results.dsa.get.topDown(results.ir.program.nameToProcedure("wmalloc"))

    val mainHeap =
      mainDSG.nodes.filter((base, _) => base.isInstanceOf[Ret]).values.map(mainDSG.find).map(_.bases.keySet)
    assert(mainHeap.forall(s => s.exists(base => base.isInstanceOf[Heap])))
    val wmallocHeap =
      wmallocDSG.nodes.filter((base, _) => base.isInstanceOf[Heap]).values.map(wmallocDSG.find).map(_.bases.keySet)
    assert(wmallocHeap.size == 1)
    assert(wmallocHeap.head.exists(base => base.isInstanceOf[Heap]))
    assert(mainHeap != wmallocHeap)
    assert(mainHeap.flatten.toSet == wmallocHeap.flatten.toSet)
  }

  test("overlapping access") {
    val results = runTest("src/test/indirect_calls/jumptable/clang/jumptable")

    // the dsg of the main procedure after the local phase
    val program = results.ir.program
    val dsg = results.dsa.get.local(program.mainProcedure)

    val globals = globalsToLiteral(results.ir)
    val add_two = globals("add_two")
    val add_six = globals("add_six")
    val sub_sev = globals("sub_seven")

    assert(dsg.exprToCells(add_two).map(dsg.get) == dsg.exprToCells(add_six).map(dsg.get))
    assert(dsg.exprToCells(add_two).size == 1)
    assert(dsg.exprToCells(add_two).head.node.isCollapsed)
  }

  test("overlapping access split") {
    val results = runTest("src/test/indirect_calls/jumptable/clang/jumptable", None, DSConfig(DSAPhase.TD, true))

    // the dsg of the main procedure after the local phase
    val program = results.ir.program
    val dsg = results.dsa.get.topDown(program.mainProcedure)

    val globals = globalsToLiteral(results.ir)
    val add_two = globals("add_two")
    val add_six = globals("add_six")
    val sub_sev = globals("sub_seven")

    assert(dsg.exprToCells(add_two).map(dsg.get) == dsg.exprToCells(add_six).map(dsg.get))
    assert(dsg.exprToCells(add_two).size == 1)
    assert(!dsg.exprToCells(add_two).head.node.isCollapsed)
  }

  test("http_parse_basic") {
    val path = "examples/cntlm-noduk/cntlm-noduk"
    val res = runTest(path, Some("http_parse_basic"))

    val proc = res.ir.program.mainProcedure
    val dsg = res.dsa.get.topDown(res.ir.program.mainProcedure)
    assert(dsg.glIntervals.size == 1)
    assert(IntervalDSA.checksStackMaintained(dsg))
    assert(!IntervalDSA.checksGlobalsMaintained(dsg))
  }

  test("cntlm local globals") {
    val path = "examples/cntlm-noduk/cntlm-noduk"
    val res = runTest(path, None, DSConfig(Local))

    val stackCollapsed = Set(
      "main",
      "md5_process_block",
      "md4_process_block",
      "gl_des_is_weak_key",
      "tunnel",
      "direct_request",
      "forward_request",
      "tunnel_add",
      "magic_auth_detect"
    )
    val globalCollapsed = Set("to_base64", "printmem", "gl_des_ecb_crypt", "from_base64", "des_key_schedule", "scanmem")
    val locals = res.dsa.get.local
    assert(locals.values.forall(_.glIntervals.size == 1))

    assert(
      locals.values.filterNot(g => stackCollapsed.contains(g.proc.procName)).forall(IntervalDSA.checksStackMaintained)
    )

    assert(
      locals.values
        .filterNot(g => globalCollapsed.contains(g.proc.procName))
        .forall(IntervalDSA.checksGlobalsMaintained)
    )

    assert(
      !locals.values.filter(g => stackCollapsed.contains(g.proc.procName)).exists(IntervalDSA.checksStackMaintained)
    )

    assert(
      !locals.values.filter(g => globalCollapsed.contains(g.proc.procName)).exists(IntervalDSA.checksGlobalsMaintained)
    )
  }

  test("cntlm split globals") {
    val path = "examples/cntlm-noduk/cntlm-noduk"
    val res = runTest(path, None, DSConfig(TD, true, true))
  }

  test("cntlm eq cells") {
    val path = "examples/cntlm-noduk/cntlm-noduk"
    val res = runTest(path, None, DSConfig(TD, eqClasses = true))
  }

  test("www_authenticate") {
    val path = "examples/cntlm-noduk/cntlm-noduk"
    val res = runTest(path, Some("www_authenticate"))

    val proc = res.ir.program.mainProcedure

    val dsg = res.dsa.get.topDown(res.ir.program.mainProcedure)
    assert(dsg.glIntervals.size == 1)
    assert(!IntervalDSA.checksStackMaintained(dsg))
    assert(!IntervalDSA.checksGlobalsMaintained(dsg))
  }

  test("hmac_md5") {
    val path = "examples/cntlm-noduk/cntlm-noduk"
    val res = runTest(path, Some("hmac_md5"))
    val proc = res.ir.program.mainProcedure
    val dsg = res.dsa.get.topDown(res.ir.program.mainProcedure)
    assert(dsg.glIntervals.size == 1)
    assert(!IntervalDSA.checksStackMaintained(dsg))
    assert(!IntervalDSA.checksGlobalsMaintained(dsg))
  }

}
