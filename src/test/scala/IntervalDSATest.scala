import analysis.data_structure_analysis.{
  Heap,
  Interval,
  IntervalDSA,
  Par,
  Ret,
  Stack,
  SymBase,
  generateConstraints,
  getSymbolicValues
}
import boogie.SpecGlobal
import ir.*
import ir.Endian.{BigEndian, LittleEndian}
import ir.dsl.{block, directCall, goto, proc, prog, ret}
import ir.{BitVecLiteral, Endian, MemoryLoad, Register, SharedMemory}
import org.scalatest.funsuite.AnyFunSuite
import specification.Specification
import test_util.BASILTest.writeToFile
import util.*
import util.DSAAnalysis.Norm
import analysis.data_structure_analysis.given
import translating.PrettyPrinter.pp_proc

@test_util.tags.UnitTest
class IntervalDSATest extends AnyFunSuite with test_util.CaptureOutput {
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

  def globalsToLiteral(ctx: IRContext) = {
    ctx.globals.map(g => (g.name, BitVecLiteral(g.address, 64))).toMap
      ++ (ctx.funcEntries.map(f => (f.name, BitVecLiteral(f.address.toInt, 64))).toMap)
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

    val result = runTest(context)

    val dsg = result.dsa.get.local(main)
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

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val context = programToContext(program, globals, globalOffsets)
    val basilResult = runTest(context)
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

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val context = programToContext(program, Set.empty, Map.empty)
    val malloc = context.program.nameToProcedure("malloc")
    malloc.isExternal = Some(true)
    val results = runTest(context)
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

  test("stack interproc") {
    val results = runTest("src/test/dsa/stack_interproc_overlapping/stack_interproc_overlapping")
    val program = results.ir.program

    // Local Callee
    val dsgCallee = results.dsa.get.local(program.nameToProcedure("set_fields"))
    val inParam = dsgCallee.exprToCells(LocalVar("R0_in", BitVecType(64))).map(dsgCallee.get).head
    val stackNode = dsgCallee.find(dsgCallee.nodes(Stack(program.nameToProcedure("set_fields"))))
    // stack should point to in parameter
    assert(stackNode.cells.filter(_.hasPointee).exists(_.getPointee.equiv(inParam)))
    // two in param cells both of size 8 starting at 0 and 16
    assert(inParam.node.cells.forall(_.interval.size.get == 8))
    assert(inParam.node.cells.size == 2)
    assert(inParam.node.cells.map(_.interval.start.get).toSet == Set(0, 16))

    // local caller
    val dsgCaller = results.dsa.get.local(program.mainProcedure)
    val stack32 = dsgCaller.nodes(Stack(program.mainProcedure)).get(-32)
    val stack48 = dsgCaller.nodes(Stack(program.mainProcedure)).get(-48)
    assert(stack32 != stack48)

    // top down caller
    val dsg = results.dsa.get.topDown(program.mainProcedure)
    val stack32td = dsg.nodes(Stack(program.mainProcedure)).get(-32)
    val stack48td = dsg.nodes(Stack(program.mainProcedure)).get(-48)
    assert(stack48td != stack32td)
  }

  test("http_parse_basic") {
    val path = "examples/cntlm-o3/cntlm-noduk"
    val res = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = path + ".adt",
          relfFile = path + ".relf",
          mainProcedureName = "http_parse_basic",
          trimEarly = true,
//          procedureTrimDepth = 1,
        ),
        simplify = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = Some(DSAConfig(Set(Norm)))
      )
    )

    val proc = res.ir.program.mainProcedure
//    val dsg = IntervalDSA.getLocal(proc, res.ir, getSymbolicValues(proc), generateConstraints(proc))
    val dsg = res.dsa.get.topDown(res.ir.program.mainProcedure)
    assert(!dsg.find(dsg.nodes(Stack(res.ir.program.mainProcedure))).isCollapsed)
  }


  test("md5_process_block") {
    val path = "examples/cntlm-o3/cntlm-noduk"
    val res = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = path + ".adt",
          relfFile = path + ".relf",
          mainProcedureName = "md5_process_block",
          trimEarly = true
        ),
        simplify = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = Some(DSAConfig(Set(Norm)))
      )
    )

    val proc = res.ir.program.mainProcedure
    val dsg = res.dsa.get.topDown(res.ir.program.mainProcedure)
  }

  test("acl_check") {
    val path = "examples/cntlm-o3/cntlm-noduk"
    val res = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = path + ".adt",
          relfFile = path + ".relf",
          mainProcedureName = "acl_check",
          trimEarly = true
        ),
        simplify = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = Some(DSAConfig(Set(Norm)))
      )
    )

    val proc = res.ir.program.mainProcedure
    val dsg = res.dsa.get.topDown(res.ir.program.mainProcedure)
    assert(!dsg.find(dsg.nodes(Stack(res.ir.program.mainProcedure))).isCollapsed)
    writeToFile(dsg.toDot, "helper.dot")
  }
}
