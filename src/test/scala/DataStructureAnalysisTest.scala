import analysis.data_structure_analysis.*
import boogie.SpecGlobal
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import specification.Specification
import test_util.{BASILTest, CaptureOutput}
import util.{
  BASILConfig,
  BASILResult,
  BoogieGeneratorConfig,
  ILLoadingConfig,
  IRContext,
  RunUtils,
  StaticAnalysisConfig,
  StaticAnalysisContext
}

/** This is the test suite for testing DSA functionality The tests follow a general pattern of running BASIL analyses on
  * a test program and then asserting properties about the Data Structure Graph (DSG) of the function produced at
  * different levels
  *
  * DSA has three phases. BASILRESULT.analysis.get.local is the set of graphs from the end of the local phase
  * BASILRESULT.analysis.get.bu is the set of graphs from the end of the bottom-up phase BASILRESULT.analysis.get.td is
  * the set of graphs from the end of the top-down phase
  */
@test_util.tags.UnitTest
class DataStructureAnalysisTest extends AnyFunSuite with CaptureOutput {

  def runAnalysis(program: Program): StaticAnalysisContext = {
    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val emptySpec = Specification(Set(), Set(), Map(), List(), List(), List(), Set())
    val emptyContext = IRContext(List(), Set(), Set(), Set(), Map(), emptySpec, program)
    RunUtils.staticAnalysis(StaticAnalysisConfig(), emptyContext)
  }

  def runTest(relativePath: String): BASILResult = {
    val path = s"${BASILTest.rootDirectory}/$relativePath"

    val result = RunUtils.loadAndTranslate(
      BASILConfig(
        loading =
          ILLoadingConfig(inputFile = path + ".adt", relfFile = Some(path + ".relf"), specFile = None, dumpIL = None),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out"
      )
    )
    result
  }

  test("overlapping access") {
    val results = runTest("src/test/indirect_calls/jumptable/clang/jumptable")

    // the dsg of the main procedure after the local phase
    val program = results.ir.program
    val dsg = results.analysis.get.localDSA(program.mainProcedure)

    val addtwo_range = dsg.getGlobal("add_two").get // procedures("add_two");
    val addsix_range = dsg.getGlobal("add_six").get // procedures("add_six");
    val subseven_range = dsg.getGlobal("sub_seven").get // procedures("sub_seven");

    // dsg.formals(R29) is the slice representing formal R29
    val R29formal = dsg.adjust(dsg.formals(R29))

    // cells representing the stack at various offsets
    val stack64 = dsg.get(dsg.stackMapping(64).cells(0)) // R31 + 0x40
    val stack72 = dsg.get(dsg.stackMapping(72).cells(0)) //  R31 + 0x40 + 8
    val stack16 = dsg.get(dsg.stackMapping(16).cells(0)) //  R31 + 40
    val stack32 = dsg.get(dsg.stackMapping(32).cells(0)) //  R31 + 32
    val stack24 = dsg.get(dsg.stackMapping(16).addCell(8, 0)) //  R31 + 24 and Malloc merged together with R31 + 16

    assert(dsg.adjust(stack64.getPointee).equals(R29formal)) // R31 points to the frame pointer
    assert(dsg.adjust(stack72.getPointee).equals(dsg.adjust(dsg.formals(R30)))) // R31 + 8 points to the link register

    // overlapping access
    assert(dsg.adjust(stack16.getPointee).equals(dsg.get(dsg.globalMapping(addtwo_range).node.cells(0))))
    assert(dsg.adjust(stack24.getPointee).equals(dsg.get(dsg.globalMapping(addsix_range).node.cells(0))))
    assert(stack24 == stack16)

//    assert(!dsg.get(dsg.globalMapping(addtwo_range).node.cells(0)).equals(dsg.get(dsg.globalMapping(addsix_range).node.cells(0))))
    assert(
      dsg
        .get(dsg.globalMapping(addtwo_range).node.cells(0))
        .equals(dsg.get(dsg.globalMapping(addsix_range).node.cells(0)))
    )
    assert(
      dsg
        .get(dsg.globalMapping(addtwo_range).node.cells(0))
        .node
        .get
        .equals(dsg.get(dsg.globalMapping(addsix_range).node.cells(0)).node.get)
    )

    assert(dsg.get(dsg.globalMapping(addtwo_range).node.cells(0)).offset.equals(0))
    assert(dsg.get(dsg.globalMapping(addsix_range).node.cells(0)).offset.equals(0))
//    assert(dsg.get(dsg.globalMapping(addsix_range).node.cells(0)).offset.equals(8))

    // locate all memory loads with R31 within their index expression, where
    // the loaded value is immediately assigned into a register.
    // the returned list is the label and destination register of the local assignment.
    val loadsWithStackOffset = program.mainProcedure.preOrderIterator
      .sliding(2)
      .map(_.toSeq)
      .collect {
        case Seq(
              MemoryLoad(memresult, _, memexpr, _, _, _),
              LocalAssign(Register(assignedreg, _), assignedval, Some(assignlabel))
            ) if memexpr.variables.contains(R31) && memresult == assignedval =>
          (assignlabel, assignedreg)
      }
      .toSeq
    assert(
      dsg
        .adjust(dsg.SSAVar.tupled(loadsWithStackOffset(0)))
        .equals(dsg.get(dsg.globalMapping(addtwo_range).node.cells(0)))
    )
    assert(
      dsg
        .adjust(dsg.SSAVar.tupled(loadsWithStackOffset(1)))
        .equals(dsg.get(dsg.globalMapping(addsix_range).node.cells(0)))
    )

    assert(dsg.adjust(stack32.getPointee).equals(dsg.get(dsg.globalMapping(subseven_range).node.cells(0))))

  }

  test("stack interproc overlapping") {
    val results = runTest("src/test/dsa/stack_interproc_overlapping/stack_interproc_overlapping")

    // the dsg of the main procedure after the all phases
    val program = results.ir.program

    // Local Callee
    val dsgCallee = results.analysis.get.localDSA(program.nameToProcedure("set_fields"))
    // dsg.formals(R29) is the slice representing formal R29

    val stack8Pointee = dsgCallee.adjust(dsgCallee.get(dsgCallee.stackMapping(8).cells(0)).getPointee)
    val R0formal = dsgCallee.adjust(dsgCallee.formals(R0))
    val paramNode = R0formal.node.get

    assert(stack8Pointee.equals(R0formal))
    assert(paramNode.cells.size == 2)
    assert(R0formal.largestAccessedSize == 8)
    assert(paramNode.cells(0) == R0formal)
    assert(paramNode.cells(16).largestAccessedSize == 8)

    // Local Caller
    val dsgCaller = results.analysis.get.localDSA(program.mainProcedure)
    val stack32 = dsgCaller.get(dsgCaller.stackMapping(32).cells(0))
    val stack48 = dsgCaller.get(dsgCaller.stackMapping(48).cells(0))

    assert(stack32.node.get != stack48.node.get)

    // topdown Caller
    val dsg = results.analysis.get.bottomUpDSA(program.mainProcedure)
    val stack32Final = dsg.get(dsg.stackMapping(32).cells(0))
    val stack32FinalNode = stack32Final.node.get
    val stack48Final = dsg.get(dsg.stackMapping(48).cells(0))

//    assert(stack0Final.largestAccessedSize == 24)
    assert(stack32Final.node.get == stack48Final.node.get)
    assert(dsg.get(stack32Final.node.get.cells(0)) == stack32Final)
    assert(dsg.get(stack32Final.node.get.cells(16)) == stack48Final)

  }

  test("global interproc overlapping") {
    val results = runTest("src/test/dsa/global_interproc_overlapping/global_interproc_overlapping")

    // the dsg of the main procedure after the local phase
    val program = results.ir.program

    // Local Caller
    val dsgCaller = results.analysis.get.localDSA(program.mainProcedure)
    val global = dsgCaller.getGlobal("global").get

    assert(dsgCaller.find(dsgCaller.globalMapping(global).node).node.cells.size == 1)
    assert(dsgCaller.get(dsgCaller.globalMapping(global).node.cells(0)).largestAccessedSize == 8)

//    // topdown Caller
    val dsg = results.analysis.get.topDownDSA(program.mainProcedure)
    assert(dsg.find(dsg.globalMapping(global).node).node.cells.size == 3)
    assert(dsg.find(dsg.globalMapping(global).node).node.cells(0).largestAccessedSize == 8)
    assert(dsg.find(dsg.globalMapping(global).node).node.cells(8).largestAccessedSize == 8)
    assert(dsg.find(dsg.globalMapping(global).node).node.cells(16).largestAccessedSize == 8)

  }

  test("indirect overlapping") {
    val results = runTest("src/test/dsa/indirect_overlapping/indirect_overlapping")

    val program = results.ir.program
    val dsg = results.analysis.get.localDSA(program.mainProcedure)

    val stack16 = dsg.get(dsg.stackMapping(16).cells(0))
    val stack32 = dsg.get(dsg.stackMapping(32).cells(0))
    assert(!dsg.stackMapping.keys.toSet.contains(24))

    val node = stack16.node.get
    assert(node.cells.size == 3)
    assert(node.cells(0).largestAccessedSize == 8)
    assert(node.cells(8).largestAccessedSize == 8)
    assert(node.cells(16).largestAccessedSize == 8)
    assert(node.cells(16) == stack32)
  }

  // Local DSA tests
  /*
  TODO - rewrite this test with a new input that is more suitable than the removed example
  test("basic pointer") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/basicpointer/basicpointer.adt",
          relfFile = "examples/basicpointer/basicpointer.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program

    // the dsg of the main procedure after the local phase
    val dsg = results.analysis.get.localDSA(program.mainProcedure)

    // dsg.formals(R29) is the slice representing formal R29
    val R29formal = dsg.adjust(dsg.formals(R29))

    // cells representing the stack at various offsets
    val stack0 = dsg.get(dsg.stackMapping(0).cells(0)) // R31
    val stack8 = dsg.get(dsg.stackMapping(8).cells(0)) //  R31 + 8
    val stack40 = dsg.get(dsg.stackMapping(40).cells(0))//  R31 + 40
    val stack32 = dsg.get(dsg.stackMapping(32).cells(0)) //  R31 + 32
    val stack24 = dsg.get(dsg.stackMapping(24).cells(0)) //  R31 + 24 and Malloc

    assert(dsg.adjust(stack0.getPointee).equals(R29formal)) // R31 points to the frame pointer
    assert(dsg.adjust(stack8.getPointee).equals(dsg.adjust(dsg.formals(R30)))) // R31 + 8 points to the link register

    /*
    Position 0000044F: tmp1 := R31 + 24 // Ev(tmp1) = new Node(R31 + 24).0
    implicit normalisation: tmp2 := R31 + 32 // Ev(tmp2) = new Node(R31 + 32).0
    Position 00000457: *tmp2 := tmp1 // merge(Ev(tmp1), E(Ev(tmp2)))
    Therefore, Node(R31 + 32).0.pointee is merged with Node(R31 + 24).0, making E(Node(R31 + 32).0) == Node(R31 + 24).0
    at position 00000446 Ev(R0) == Malloc_Node == E(Node(R31 + 24).0) we have
    mem := mem with [R31 + 0x20, el]:u64 <- R0
   *(R31 + 32) := R0
    merge(Ev(R0), E(Ev(R31+ 32))
    == merge(E(Node(R31 + 24).0), E(Node(R31 + 32).0))
    == merge(E(Node(R31 + 24).0), Node(R31 + 24).0)
    which merges make the stack + 24 point to itself
   */

    // R31 + 32 points to R31 + 24, later set to point to heap but it should point to both (
    assert(dsg.adjust(stack32.getPointee).equals(stack24))
    assert(stack24.node.get.collapsed) // 00000497 collapses stack24 concatenation is currently unhandled, any objects referenced in an unhandled operation are collapsed
    assert(dsg.adjust(stack24.getPointee).equals(stack24)) // 00000466, R31 + 32 and R31 + 24 pointees are merged

    // __stack_chk_guard's pointee is also pointed to by stack40
    assert(dsg.get(dsg.adjust(stack40.getPointee)).equals(dsg.get(dsg.adjust(dsg.get(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69600)).node.cells(0).getPointee)).getPointee))))

  }
   */

  // this function asserts universal properties about global objects in Jumptable2  example
  def assertJumptable2Globals(dsg: Graph): Unit = {
    // global mappings
    val addsix = dsg.getGlobal("add_six").get
    val addtwo = dsg.getGlobal("add_two").get
    val subseven = dsg.getGlobal("sub_seven").get
    val main = dsg.getGlobal("main").get

    val x = dsg.getGlobal("x").get
    val x_relocated = dsg.getGlobal("x", 8, 1).get // all relocations are pointing to an address, therefore size 8
    val main_relocated = dsg.getGlobal("main", 8, 1).get
    val jumptable = dsg.getGlobal("add_two", 24, 1).get // jumptable points to add_two and has size 24
    val jumptable_relocated = dsg.getGlobal("add_two", 8, 2).get

    // jump_table relocation
    assert(
      dsg
        .adjust(dsg.globalMapping(jumptable_relocated).node.cells(0).getPointee)
        .equals(dsg.get(dsg.globalMapping(jumptable).node.cells(0)))
    )
    // add_two relocation
    assert(
      dsg
        .adjust(dsg.globalMapping(jumptable).node.cells(0).getPointee)
        .equals(dsg.get(dsg.globalMapping(addtwo).node.cells(0)))
    )
    // add_six relocation
    assert(
      dsg
        .adjust(dsg.globalMapping(jumptable).node.cells(8).getPointee)
        .equals(dsg.get(dsg.globalMapping(addsix).node.cells(0)))
    )
    // sub_seven relocation
    assert(
      dsg
        .adjust(dsg.globalMapping(jumptable).node.cells(16).getPointee)
        .equals(dsg.get(dsg.globalMapping(subseven).node.cells(0)))
    )
    // main relocation
    assert(
      dsg
        .adjust(dsg.globalMapping(main_relocated).node.cells(0).getPointee)
        .equals(dsg.get(dsg.globalMapping(main).node.cells(0)))
    )
    // x relocation
    assert(
      dsg
        .adjust(dsg.globalMapping(x_relocated).node.cells(0).getPointee)
        .equals(dsg.get(dsg.globalMapping(x).node.cells(0)))
    )
  }

  test("local jumptable2 callees") {
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")

    val program = results.ir.program
    // test that all three callees have the same local graph
    val callees = Set("sub_seven", "add_two", "add_six")
    val procs = program.nameToProcedure

    callees.foreach { callee =>
      val dsg = results.analysis.get.localDSA(procs(callee))
      val x = dsg.getGlobal("x").get
      assert(dsg.stackMapping.isEmpty) // stack is not used in either callee
      assertJumptable2Globals(dsg) // globals should be the same everywhere unused in callees
      // x should point to a collapsed object, in all 3 functions
      // all three load value of x
      // the analysis doesn't know if x is a pointer or not therefore assumes it is for soundness
      // arbitrary pointer is used in arithmetic causing collapse
      assert(dsg.adjust(dsg.get(dsg.globalMapping(x).node.cells(0)).getPointee).node.get.collapsed)
    }
  }

  test("local jumptable2 main") {
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")

    val program = results.ir.program
    val dsg = results.analysis.get.localDSA(program.mainProcedure)
    val x = dsg.getGlobal("x").get
    val stack0 = dsg.get(dsg.stackMapping(0).cells(0))
    val stack8 = dsg.get(dsg.stackMapping(8).cells(0))
    val stack16 = dsg.get(dsg.stackMapping(16).cells(0))
    val stack28 = dsg.get(dsg.stackMapping(28).cells(0))
    assert(dsg.adjust(stack0.getPointee).equals(dsg.adjust(dsg.formals(R29))))
    assert(dsg.adjust(stack8.getPointee).equals(dsg.adjust(dsg.formals(R30))))
    assert(dsg.adjust(stack16.getPointee).equals(dsg.adjust(dsg.formals(R1)))) // input args
    assert(dsg.adjust(stack28.getPointee).equals(dsg.adjust(dsg.formals(R0)))) // input args

    // initial global mappings
    assertJumptable2Globals(dsg)

    // x should not be collapsed in the main function's local graph
    assert(!dsg.get(dsg.globalMapping(x).node.cells(0)).getPointee.node.collapsed)
  }

  test("unsafe pointer arithmetic") {
    val results = runTest("src/test/dsa/unsafe_pointer_arithmetic/unsafe_pointer_arithmetic")

    val program = results.ir.program
    val dsg = results.analysis.get.localDSA(program.mainProcedure)

    // stackX is the pointee of stack object at position X instead of the stack object itself
    val stack0 = dsg.adjust(dsg.get(dsg.stackMapping(0).cells(0)).getPointee)
    val stack8 = dsg.adjust(dsg.get(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.get(dsg.stackMapping(24).cells(0)).getPointee)
    val stack32 = dsg.adjust(dsg.get(dsg.stackMapping(32).cells(0)).getPointee)
    val stack40 = dsg.adjust(dsg.get(dsg.stackMapping(40).cells(0)).getPointee)
    val stack48 = dsg.adjust(dsg.get(dsg.stackMapping(48).cells(0)).getPointee)
    val stack56 = dsg.adjust(dsg.get(dsg.stackMapping(56).cells(0)).getPointee)

    assert(stack0.equals(dsg.adjust(dsg.formals(R29))))
    assert(stack8.equals(dsg.adjust(dsg.formals(R30))))

    // stack24 and stack32 should point to the beginning of first Malloc (size 20)
    assert(stack24.equals(stack32))
    assert(stack24.offset == 0)
    assert(stack24.node.get.allocationRegions.size == 1)
    assert(stack24.node.get.allocationRegions.head.asInstanceOf[HeapLocation].size == 20)

    // stack24 and stack40 should be pointing to the same cell at different internal offsets
    val unadjustedStack24Pointee = dsg.get(dsg.stackMapping(24).cells(0)).getPointee
    val unadjustedStack40Pointee = dsg.get(dsg.stackMapping(40).cells(0)).getPointee
    assert(unadjustedStack24Pointee.cell.equals(unadjustedStack40Pointee.cell))
    assert(unadjustedStack40Pointee.internalOffset == 1) // result of unsafe pointer arithmetic
    assert(unadjustedStack24Pointee.internalOffset == 0)
    assert(unadjustedStack24Pointee.offset == 0)

    // stack48 points to second malloc (size 8)
    assert(stack48.node.get.allocationRegions.size == 1)
    assert(stack48.node.get.allocationRegions.head.asInstanceOf[HeapLocation].size == 8)

    // stack 48 points to a malloc address which point to the pointee of stack40 and stack56
    assert(dsg.adjust(stack48.getPointee).equals(stack40))
    assert(dsg.adjust(stack48.getPointee).equals(stack56))
  }

  test("interproc pointer arithmetic main") {
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.localDSA(program.mainProcedure)
    val stack0 = dsg.adjust(dsg.get(dsg.stackMapping(0).cells(0)).getPointee)
    val stack8 = dsg.adjust(dsg.get(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.get(dsg.stackMapping(24).cells(0)).getPointee)
    val stack32 = dsg.adjust(dsg.get(dsg.stackMapping(32).cells(0)).getPointee)
    val stack40 = dsg.adjust(dsg.get(dsg.stackMapping(40).cells(0)).getPointee)

    assert(stack0.equals(dsg.adjust(dsg.formals(R29))))
    assert(stack8.equals(dsg.adjust(dsg.formals(R30))))

    // stack24 and 32 point to different offsets of the same node
    assert(stack24.node.get.equals(stack32.node.get))
    assert(stack24.offset == 0)
    assert(stack32.offset == 16)

    // stack40 points to a different offset of stack24's node but the analysis can't determine that in the local phase
    assert(stack40.pointee.isDefined)
    assert(!stack40.node.get.equals(stack24.node.get))
  }

  test("interproc pointer arithmetic callee") {
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.localDSA(program.nameToProcedure("callee"))
    val stack8 = dsg.adjust(dsg.get(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.get(dsg.stackMapping(24).cells(0)).getPointee)

    // stack8 points to the formal argument object
    assert(stack8.equals(dsg.adjust(dsg.formals(R0))))
    assert(stack8.offset == 0)
    // stack 24 points to the formal argument object at offset 16, instead
    assert(stack24.equals(dsg.adjust(dsg.formals(R0)).node.get.cells(16)))
  }

  test("internal merge") {
    // this is an internal merge (two cells of the same node overlap and are merged together)
    val mem = SharedMemory("mem", 64, 8)
    val locAssign1 = LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val program = prog(
      proc(
        "main",
        block(
          "operations",
          locAssign1, // R6 = R0 + 4
          locAssign2, // R7 = R0 + 5
          MemoryStore(mem, R7, R1, Endian.BigEndian, 64, Some("00003")), // *R7 = R1, (*R6 + 1) = R1
          MemoryStore(mem, R6, R2, Endian.BigEndian, 64, Some("00004")), // *R6 = R2
          ret
        )
      )
    )

    val results = runAnalysis(program)

    val dsg: Graph = results.localDSA(program.mainProcedure)

    // R6 and R7 address the same cell (overlapping cells in the same node that are merged)
    assert(dsg.get(dsg.varToCell(locAssign1)(R6)).equals(dsg.get(dsg.varToCell(locAssign2)(R7))))

    // outgoing edges of R6 and R7 are unified since the cells are merged
    // object of formals R1 and R2 are written to overlapping fields of the same node (R6, R7); causing them to be merged together
    assert(dsg.adjust(dsg.formals(R1)).equals(dsg.adjust(dsg.formals(R2))))
    // however, they address different internal offets in those cells
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).internalOffset == 0)
    assert(dsg.find(dsg.varToCell(locAssign2)(R7)).internalOffset == 1)

    // Since R6 and R7 are pointing to the same cell (R1 and R2)
    // R6 (or R7)'s pointee should be the same as R1 and R2
    assert(dsg.adjust(dsg.varToCell(locAssign1)(R6)).pointee.isDefined)
    assert(dsg.adjust(dsg.adjust(dsg.varToCell(locAssign1)(R6)).getPointee).equals(dsg.adjust(dsg.formals(R1))))
  }

  test("offsetting from middle of cell to a new cell") {
    val mem = SharedMemory("mem", 64, 8)
    val locAssign1 = LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = LocalAssign(R5, BinaryExpr(BVADD, R7, BitVecLiteral(8, 64)), Some("00005"))

    val program = prog(
      proc(
        "main",
        block(
          "operations",
          locAssign1, // R6 = R0 + 4
          locAssign2, // R7 = R0 + 5
          MemoryStore(mem, R7, R1, Endian.BigEndian, 64, Some("00003")),
          MemoryStore(mem, R6, R2, Endian.BigEndian, 64, Some("00004")),
          locAssign3, // R5 = R7 + 8
          ret
        )
      )
    )

    val results = runAnalysis(program)
    val dsg: Graph = results.localDSA(program.mainProcedure)
    // check that R5 points to separate cell at offset 13
    assert(dsg.get(dsg.varToCell(locAssign3)(R5)).offset == 13)
  }

  test("offsetting from middle of cell to the same cell") {
    // similar to above except instead of creating new cell the last assign
    // points R5's cell at an internal offset of 8
    val mem = SharedMemory("mem", 64, 8)
    val locAssign1 = LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = LocalAssign(R5, BinaryExpr(BVADD, R7, BitVecLiteral(7, 64)), Some("00005"))

    val program = prog(
      proc(
        "main",
        block(
          "operations",
          locAssign1,
          locAssign2,
          MemoryStore(mem, R7, R1, Endian.BigEndian, 64, Some("00003")),
          MemoryStore(mem, R6, R2, Endian.BigEndian, 64, Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val results = runAnalysis(program)
    val dsg: Graph = results.localDSA(program.mainProcedure)
    assert(dsg.get(dsg.formals(R1)).equals(dsg.get(dsg.formals(R2))))
    assert(dsg.get(dsg.varToCell(locAssign1)(R6)).equals(dsg.get(dsg.varToCell(locAssign2)(R7))))
    assert(dsg.get(dsg.varToCell(locAssign1)(R6)).equals(dsg.get(dsg.varToCell(locAssign3)(R5))))
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).internalOffset == 0)
    assert(dsg.find(dsg.varToCell(locAssign2)(R7)).internalOffset == 1)
    assert(dsg.find(dsg.varToCell(locAssign3)(R5)).internalOffset == 8)
    assert(dsg.get(dsg.varToCell(locAssign1)(R6)).pointee.isDefined)
    assert(dsg.get(dsg.get(dsg.varToCell(locAssign1)(R6)).getPointee).equals(dsg.get(dsg.formals(R1))))
  }

  test("internal offset transfer") {
    // this is a test to check assignments transfer internal offset of slices.
    val mem = SharedMemory("mem", 64, 8)
    val locAssign1 = LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = LocalAssign(R5, R7, Some("00005"))

    val program = prog(
      proc(
        "main",
        block(
          "operations",
          //          Assign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          locAssign1,
          locAssign2,
          MemoryStore(mem, R7, R1, Endian.BigEndian, 64, Some("00003")),
          MemoryStore(mem, R6, R2, Endian.BigEndian, 64, Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val results = runAnalysis(program)

    val dsg: Graph = results.localDSA(program.mainProcedure)
    assert(dsg.get(dsg.varToCell(locAssign2)(R7)).equals(dsg.get(dsg.varToCell(locAssign3)(R5))))
  }

  // bottom up tests
  ignore("bottom up jumptable2 callees") {
    // this is the same as local graphs
    // nothing should be changed
    // TODO count point-to relations and ensure no more constraints are added in this phase
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")

    val program = results.ir.program
    // test that all three callees have the same local graph
    val callees = Set("sub_seven", "add_two", "add_six")
    val procs = program.nameToProcedure
    callees.foreach { callee =>
      val dsg = results.analysis.get.bottomUpDSA(procs(callee))
      val x = dsg.getGlobal("x").get
      assert(dsg.stackMapping.isEmpty) // stack is not used in either callee
      assertJumptable2Globals(dsg) // globals should be the same everywhere unused in callees
      // x should point to a collapsed object, in all 3 functions
      // all three load value of x
      // the analysis doesn't know if x is a pointer or not therefore assumes it is for soundness
      // arbitrary pointer is used in arithmetic causing collapse
      assert(dsg.adjust(dsg.get(dsg.globalMapping(x).node.cells(0)).getPointee).node.get.collapsed)
    }
  }

  test("bottom up jumptable2 main") {
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")
    val program = results.ir.program
    val dsg = results.analysis.get.bottomUpDSA(program.mainProcedure)

    val x = dsg.getGlobal("x").get
    val framePointer = dsg.get(dsg.stackMapping(0).cells(0))
    val stack8 = dsg.get(dsg.stackMapping(8).cells(0))
    val stack16 = dsg.get(dsg.stackMapping(16).cells(0))
    val stack28 = dsg.get(dsg.stackMapping(28).cells(0))
    assert(dsg.adjust(framePointer.getPointee).equals(dsg.adjust(dsg.formals(R29))))
    assert(dsg.adjust(stack8.getPointee).equals(dsg.adjust(dsg.formals(R30))))
    assert(dsg.adjust(stack16.getPointee).equals(dsg.adjust(dsg.formals(R1))))
    assert(dsg.adjust(stack28.getPointee).equals(dsg.adjust(dsg.formals(R0))))

    // initial global mappings
    assertJumptable2Globals(dsg)

    // bottom-up x now should be collapsed since it was collapsed in callees
    assert(dsg.get(dsg.globalMapping(x).node.cells(0)).getPointee.node.collapsed)
  }

  ignore("bottom up interproc pointer arithmetic callee") {
    // same as interproc pointer arithmetic callee's local graph (no changes should have been made)
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.bottomUpDSA(program.nameToProcedure("callee"))
    val stack8 = dsg.adjust(dsg.get(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.get(dsg.stackMapping(24).cells(0)).getPointee)

    assert(stack8.equals(dsg.adjust(dsg.formals(R0))))
    assert(stack8.offset == 0)
    assert(stack24.equals(dsg.adjust(dsg.formals(R0)).node.get.cells(16)))
  }

  test("bottom up interproc pointer arithmetic main") {
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.bottomUpDSA(program.mainProcedure)

    val stack0 = dsg.adjust(dsg.get(dsg.stackMapping(0).cells(0)).getPointee)
    val stack8 = dsg.adjust(dsg.get(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.get(dsg.stackMapping(24).cells(0)).getPointee)
    val stack32 = dsg.adjust(dsg.get(dsg.stackMapping(32).cells(0)).getPointee)
    val stack40 = dsg.adjust(dsg.get(dsg.stackMapping(40).cells(0)).getPointee)

    // same as the local graph with the difference that stack40 points to cell at
    // a different of the same node as pointees of stack32 and stack24
    assert(stack0.equals(dsg.adjust(dsg.formals(R29))))
    assert(stack8.equals(dsg.adjust(dsg.formals(R30))))
    assert(stack24.node.get.equals(stack32.node.get))
    assert(stack24.offset == 0)
    assert(stack32.offset == 16)
    assert(stack40.pointee.isDefined)
    assert(stack40.node.get.equals(stack24.node.get))
    assert(stack40.offset == 32)
    assert(dsg.get(dsg.stackMapping(40).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.get(dsg.stackMapping(32).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.get(dsg.stackMapping(24).cells(0)).getPointee.internalOffset == 0)
  }

  // top down tests
  ignore("top down jumptable2 main") {
    // no changes should be made from previous phase
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")
    val program = results.ir.program
    val dsg = results.analysis.get.topDownDSA(program.mainProcedure)
//    assert(dsg.pointTo.size == 13) // 13
    val x = dsg.getGlobal("x").get

    val framePointer = dsg.get(dsg.stackMapping(0).cells(0))
    val stack8 = dsg.get(dsg.stackMapping(8).cells(0))
    val stack16 = dsg.get(dsg.stackMapping(16).cells(0))
    val stack28 = dsg.get(dsg.stackMapping(28).cells(0))
    assert(dsg.adjust(framePointer.getPointee).equals(dsg.adjust(dsg.formals(R29))))
    assert(dsg.adjust(stack8.getPointee).equals(dsg.adjust(dsg.formals(R30))))
    assert(dsg.adjust(stack16.getPointee).equals(dsg.adjust(dsg.formals(R1))))
    assert(dsg.adjust(stack28.getPointee).equals(dsg.adjust(dsg.formals(R0))))
    assertJumptable2Globals(dsg)

    // bottom-up
    assert(dsg.get(dsg.globalMapping(x).node.cells(0)).getPointee.node.collapsed)

  }

  ignore("top down jumptable2 callees") {
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")

    val program = results.ir.program
    // test that all three callees have the same local graph
    val callees = Set("sub_seven", "add_two", "add_six")
    val procs = program.nameToProcedure
    callees.foreach { callee =>
      val dsg = results.analysis.get.topDownDSA(procs(callee))
      val x = dsg.getGlobal("x").get
      assert(dsg.stackMapping.isEmpty) // stack is not used in either callee
      assertJumptable2Globals(dsg) // globals should be the same everywhere unused in callees
      // x should point to a collapsed object, in all 3 functions
      // all three load value of x
      // the analysis doesn't know if x is a pointer or not therefore assumes it is for soundness
      // arbitrary pointer is used in arithmetic causing collapse
      assert(dsg.adjust(dsg.get(dsg.globalMapping(x).node.cells(0)).getPointee).node.get.collapsed)
    }
  }

  test("top down interproc pointer arithmetic callee") {
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.topDownDSA(program.nameToProcedure("callee"))

    val stack8 = dsg.adjust(dsg.get(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.get(dsg.stackMapping(24).cells(0)).getPointee)

    // callee should now have different offsets due to formal and actual input parameters being unified
    assert(stack8.equals(dsg.adjust(dsg.formals(R0))))
    assert(stack8.offset == 16)
    assert(stack24.equals(dsg.adjust(dsg.formals(R0)).node.get.cells(32)))
  }

  // top-down phase should be the same as bottom-up phase
  ignore("top down interproc pointer arithmetic main") {
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.topDownDSA(program.mainProcedure)

    val stack0 = dsg.adjust(dsg.get(dsg.stackMapping(0).cells(0)).getPointee)
    val stack8 = dsg.adjust(dsg.get(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.get(dsg.stackMapping(24).cells(0)).getPointee)
    val stack32 = dsg.adjust(dsg.get(dsg.stackMapping(32).cells(0)).getPointee)
    val stack40 = dsg.adjust(dsg.get(dsg.stackMapping(40).cells(0)).getPointee)

    assert(stack0.equals(dsg.adjust(dsg.formals(R29))))
    assert(stack8.equals(dsg.adjust(dsg.formals(R30))))
    assert(stack24.node.get.equals(stack32.node.get))
    assert(stack24.offset == 0)
    assert(stack32.offset == 16)
    assert(stack40.pointee.isDefined)
    assert(stack40.node.get.equals(stack24.node.get))
    assert(stack40.offset == 32)
    assert(dsg.get(dsg.stackMapping(40).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.get(dsg.stackMapping(32).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.get(dsg.stackMapping(24).cells(0)).getPointee.internalOffset == 0)
  }

  test("overlapping accesses soundness") {
    val mem = SharedMemory("mem", 64, 8)
    val V0 = Register("V0", 128)
    val xAddress = BitVecLiteral(2000, 64)
    val yAddress = BitVecLiteral(3000, 64)
    val xPointer = BitVecLiteral(1000, 64)
    val yPointer = BitVecLiteral(1008, 64)
    val globalOffsets = Map(xPointer.value -> xAddress.value, yPointer.value -> yAddress.value)
    val x = SpecGlobal("x", 64, None, xAddress.value)
    val y = SpecGlobal("y", 64, None, yAddress.value)
    val globals = Set(x, y)

    val load = MemoryLoad(V0, mem, xPointer, Endian.LittleEndian, 128, Some("001"))

    val program = prog(proc("main", block("block", load, ret)))

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val spec = Specification(Set(), globals, Map(), List(), List(), List(), Set())
    val context = IRContext(List(), Set(), globals, Set(), globalOffsets, spec, program)
    val staticAnalysisResult = RunUtils.staticAnalysis(StaticAnalysisConfig(), context)

    val dsg = staticAnalysisResult.topDownDSA(program.mainProcedure)

    val V0pointee = dsg.adjust(dsg.varToCell(load)(V0))

    val xPointerField = dsg.globalMapping(AddressRange(1000, 1008))
    val yPointerField = dsg.globalMapping(AddressRange(1008, 1016))

    val xPointerCell = xPointerField.node.getCell(xPointerField.offset)
    val yPointerCell = yPointerField.node.getCell(yPointerField.offset)

    // for this to be sound, the location 1000, the location 1008, and V0 should all point to a collapsed node which
    // represents x and y

    assert(xPointerCell.pointee.isDefined)
    assert(dsg.adjust(xPointerCell.pointee.get).equals(V0pointee))

    assert(yPointerCell.pointee.isDefined)
    assert(dsg.adjust(yPointerCell.pointee.get).equals(V0pointee))

    val node = V0pointee.node
    assert(node.isDefined)
    assert(node.get.collapsed)

  }
}
