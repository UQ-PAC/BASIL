import analysis.{AddressRange, DSC, DSG, DSN, DataLocation, HeapLocation}
import ir.{Assign, BVADD, BinaryExpr, BitVecLiteral, CFGPosition, DirectCall, Endian, Memory, MemoryAssign, MemoryLoad, Program, Register, SharedMemory, cilvisitor, transforms}
import org.scalatest.funsuite.AnyFunSuite
import ir.dsl.*
import specification.Specification
import util.{BASILConfig, BASILResult, BoogieGeneratorConfig, ILLoadingConfig, IRContext, RunUtils, StaticAnalysisConfig, StaticAnalysisContext}

/**
 * This is the test suite for testing DSA functionality
 * The tests follow a general pattern of running BASIL analyses on a test program
 * and then asserting properties about the Data Structure Graph (DSG) of the function produced at
 * different levels
 *
 * DSA has three phases.
 * BASILRESULT.analysis.get.local is the set of graphs from the end of the local phase
 * BASILRESULT.analysis.get.bu is the set of graphs from the end of the bottom-up phase
 * BASILRESULT.analysis.get.td is the set of graphs from the end of the top-down phase
 *
 */
class DSATest extends AnyFunSuite {

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
        loading = ILLoadingConfig(
          inputFile = path + ".adt",
          relfFile = path + ".relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
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
    val dsg = results.analysis.get.locals.get(program.mainProcedure)

    // dsg.formals(R29) is the slice representing formal R29
    val R29formal = dsg.adjust(dsg.formals(R29))

    // cells representing the stack at various offsets
    val stack0 = dsg.find(dsg.stackMapping(0).cells(0)) // R31
    val stack8 = dsg.find(dsg.stackMapping(8).cells(0)) //  R31 + 8
    val stack40 = dsg.find(dsg.stackMapping(40).cells(0))//  R31 + 40
    val stack32 = dsg.find(dsg.stackMapping(32).cells(0)) //  R31 + 32
    val stack24 = dsg.find(dsg.stackMapping(24).cells(0)) //  R31 + 24 and Malloc

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
    assert(dsg.find(dsg.adjust(stack40.getPointee)).equals(dsg.find(dsg.adjust(dsg.find(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69600)).node.cells(0).getPointee)).getPointee))))

  }
  */

  // this function asserts universal properties about global objects in Jumptable2  example
  def assertJumptable2Globals(dsg: DSG): Unit = {
    // global mappings

    // jump_table relocation
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69624 + 8)).node.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24)).node.cells(0))))
    // add_two relocation
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24)).node.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1940, 1940 + 36)).node.cells(0))))
    // add_six relocation
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24)).node.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1976, 1976 + 36)).node.cells(0))))
    // sub_seven relocation
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24)).node.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2012, 2012 + 36)).node.cells(0))))
    // main relocation
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69608 + 8)).node.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2048, 2048 + 76)).node.cells(0))))
    // x relocation
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69592, 69592 + 8)).node.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4)).node.cells(0))))
  }

  test("local jumptable2 callees") {
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")

    val program = results.ir.program
    // test that all three callees have the same local graph
    val callees = Set("sub_seven", "add_two", "add_six")
    val procs = program.nameToProcedure

    callees.foreach { callee =>
      val dsg = results.analysis.get.locals.get(procs(callee))
      assert(dsg.stackMapping.isEmpty) // stack is not used in either callee
      assertJumptable2Globals(dsg) // globals should be the same everywhere unused in callees
      // x should point to a collapsed object, in all 3 functions
      // all three load value of x
      // the analysis doesn't know if x is a pointer or not therefore assumes it is for soundness
      // arbitrary pointer is used in arithmetic causing collapse
      assert(dsg.adjust(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4)).node.cells(0)).getPointee).node.get.collapsed)
    }
  }

  test("local jumptable2 main") {
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")

    val program = results.ir.program
    val dsg = results.analysis.get.locals.get(program.mainProcedure)
    val stack0 = dsg.find(dsg.stackMapping(0).cells(0))
    val stack8 = dsg.find(dsg.stackMapping(8).cells(0))
    val stack16 = dsg.find(dsg.stackMapping(16).cells(0))
    val stack28 = dsg.find(dsg.stackMapping(28).cells(0))
    assert(dsg.adjust(stack0.getPointee).equals(dsg.adjust(dsg.formals(R29))))
    assert(dsg.adjust(stack8.getPointee).equals(dsg.adjust(dsg.formals(R30))))
    assert(dsg.adjust(stack16.getPointee).equals(dsg.adjust(dsg.formals(R1)))) // input args
    assert(dsg.adjust(stack28.getPointee).equals(dsg.adjust(dsg.formals(R0)))) // input args

    // initial global mappings
    assertJumptable2Globals(dsg)

    // x should not be collapsed in the main function's local graph
    assert(!dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4)).node.cells(0)).getPointee.node.collapsed)
  }

  test("unsafe pointer arithmetic") {
    val results = runTest("src/test/dsa/unsafe_pointer_arithmetic/unsafe_pointer_arithmetic")

    val program = results.ir.program
    val dsg = results.analysis.get.locals.get(program.mainProcedure)

    // stackX is the pointee of stack object at position X instead of the stack object itself
    val stack0 = dsg.adjust(dsg.find(dsg.stackMapping(0).cells(0)).getPointee)
    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)
    val stack32 = dsg.adjust(dsg.find(dsg.stackMapping(32).cells(0)).getPointee)
    val stack40 = dsg.adjust(dsg.find(dsg.stackMapping(40).cells(0)).getPointee)
    val stack48 = dsg.adjust(dsg.find(dsg.stackMapping(48).cells(0)).getPointee)
    val stack56 = dsg.adjust(dsg.find(dsg.stackMapping(56).cells(0)).getPointee)

    assert(stack0.equals(dsg.adjust(dsg.formals(R29))))
    assert(stack8.equals(dsg.adjust(dsg.formals(R30))))

    // stack24 and stack32 should point to the beginning of first Malloc (size 20)
    assert(stack24.equals(stack32))
    assert(stack24.offset == 0)
    assert(stack24.node.get.allocationRegions.size == 1)
    assert(stack24.node.get.allocationRegions.head.asInstanceOf[HeapLocation].size == 20)

    // stack24 and stack40 should be pointing to the same cell at different internal offsets
    val unadjustedStack24Pointee = dsg.find(dsg.stackMapping(24).cells(0)).getPointee
    val unadjustedStack40Pointee = dsg.find(dsg.stackMapping(40).cells(0)).getPointee
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
    val dsg = results.analysis.get.locals.get(program.mainProcedure)
    val stack0 = dsg.adjust(dsg.find(dsg.stackMapping(0).cells(0)).getPointee)
    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)
    val stack32 = dsg.adjust(dsg.find(dsg.stackMapping(32).cells(0)).getPointee)
    val stack40 = dsg.adjust(dsg.find(dsg.stackMapping(40).cells(0)).getPointee)

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
    val dsg = results.analysis.get.locals.get(program.nameToProcedure("callee"))
    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)

    // stack8 points to the formal argument object
    assert(stack8.equals(dsg.adjust(dsg.formals(R0))))
    assert(stack8.offset == 0)
    // stack 24 points to the formal argument object at offset 16, instead
    assert(stack24.equals(dsg.adjust(dsg.formals(R0)).node.get.cells(16)))
  }


  test("internal merge") {
    // this is an internal merge (two cells of the same node overlap and are merged together)
    val mem = SharedMemory("mem", 64, 8)
    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val program = prog(
      proc("main",
        block("operations",
          locAssign1, // R6 = R0 + 4
          locAssign2, // R7 = R0 + 5
          MemoryAssign(mem, R7, R1, Endian.BigEndian, 64, Some("00003")), // *R7 = R1, (*R6 + 1) = R1
          MemoryAssign(mem, R6, R2, Endian.BigEndian, 64, Some("00004")), // *R6 = R2
          ret
        )
      )
    )

    val results = runAnalysis(program)

    val dsg: DSG = results.locals.get(program.mainProcedure)

    // R6 and R7 address the same cell (overlapping cells in the same node that are merged)
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).cell.equals(dsg.find(dsg.varToCell(locAssign2)(R7)).cell))

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
    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = Assign(R5, BinaryExpr(BVADD, R7,  BitVecLiteral(8, 64)), Some("00005"))

    val program = prog(
      proc("main",
        block("operations",
          locAssign1, // R6 = R0 + 4
          locAssign2, // R7 = R0 + 5
          MemoryAssign(mem, R7, R1, Endian.BigEndian, 64, Some("00003")),
          MemoryAssign(mem, R6, R2, Endian.BigEndian, 64, Some("00004")),
          locAssign3, // R5 = R7 + 8
          ret
        )
      )
    )

    val results = runAnalysis(program)
    val dsg: DSG = results.locals.get(program.mainProcedure)
    // check that R5 points to separate cell at offset 13
    assert(dsg.find(dsg.varToCell(locAssign3)(R5)).offset == 13)
  }

  test("offsetting from middle of cell to the same cell") {
    // similar to above except instead of creating new cell the last assign
    // points R5's cell at an internal offset of 8
    val mem = SharedMemory("mem", 64, 8)
    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = Assign(R5, BinaryExpr(BVADD, R7, BitVecLiteral(7, 64)), Some("00005"))

    val program = prog(
      proc("main",
        block("operations",
          locAssign1,
          locAssign2,
          MemoryAssign(mem, R7, R1, Endian.BigEndian, 64, Some("00003")),
          MemoryAssign(mem,  R6, R2, Endian.BigEndian, 64, Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val results = runAnalysis(program)
    val dsg: DSG = results.locals.get(program.mainProcedure)
    assert(dsg.find(dsg.formals(R1)).equals(dsg.find(dsg.formals(R2))))
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).cell.equals(dsg.find(dsg.varToCell(locAssign2)(R7)).cell))
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).cell.equals(dsg.find(dsg.varToCell(locAssign3)(R5)).cell))
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).internalOffset == 0)
    assert(dsg.find(dsg.varToCell(locAssign2)(R7)).internalOffset == 1)
    assert(dsg.find(dsg.varToCell(locAssign3)(R5)).internalOffset == 8)
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).cell.pointee.isDefined)
    assert(dsg.find(dsg.find(dsg.varToCell(locAssign1)(R6)).cell.getPointee).equals(dsg.find(dsg.formals(R1))))
  }

  test("internal offset transfer") {
    // this is a test to check assignments transfer internal offset of slices.
    val mem = SharedMemory("mem", 64, 8)
    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = Assign(R5, R7, Some("00005"))

    val program = prog(
      proc("main",
        block("operations",
          //          Assign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          locAssign1,
          locAssign2,
          MemoryAssign(mem, R7, R1, Endian.BigEndian, 64, Some("00003")),
          MemoryAssign(mem, R6, R2, Endian.BigEndian, 64, Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val results = runAnalysis(program)

    val dsg: DSG = results.locals.get(program.mainProcedure)
    assert(dsg.find(dsg.varToCell(locAssign2)(R7)).equals(dsg.find(dsg.varToCell(locAssign3)(R5))))
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
      val dsg = results.analysis.get.bus.get(procs(callee))
      assert(dsg.stackMapping.isEmpty) // stack is not used in either callee
      assertJumptable2Globals(dsg) // globals should be the same everywhere unused in callees
      // x should point to a collapsed object, in all 3 functions
      // all three load value of x
      // the analysis doesn't know if x is a pointer or not therefore assumes it is for soundness
      // arbitrary pointer is used in arithmetic causing collapse
      assert(dsg.adjust(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4)).node.cells(0)).getPointee).node.get.collapsed)
    }
  }

  test("bottom up jumptable2 main") {
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")
    val program = results.ir.program
    val dsg = results.analysis.get.bus.get(program.mainProcedure)

    val framePointer = dsg.find(dsg.stackMapping(0).cells(0))
    val stack8 = dsg.find(dsg.stackMapping(8).cells(0))
    val stack16 = dsg.find(dsg.stackMapping(16).cells(0))
    val stack28 = dsg.find(dsg.stackMapping(28).cells(0))
    assert(dsg.adjust(framePointer.getPointee).equals(dsg.adjust(dsg.formals(R29))))
    assert(dsg.adjust(stack8.getPointee).equals(dsg.adjust(dsg.formals(R30))))
    assert(dsg.adjust(stack16.getPointee).equals(dsg.adjust(dsg.formals(R1))))
    assert(dsg.adjust(stack28.getPointee).equals(dsg.adjust(dsg.formals(R0))))

    // initial global mappings
    assertJumptable2Globals(dsg)

    // bottom-up x now should be collapsed since it was collapsed in callees
    assert(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4)).node.cells(0)).getPointee.node.collapsed)
  }

  ignore("bottom up interproc pointer arithmetic callee") {
    // same as interproc pointer arithmetic callee's local graph (no changes should have been made)
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.bus.get(program.nameToProcedure("callee"))
    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)

    assert(stack8.equals(dsg.adjust(dsg.formals(R0))))
    assert(stack8.offset == 0)
    assert(stack24.equals(dsg.adjust(dsg.formals(R0)).node.get.cells(16)))
  }

  test("bottom up interproc pointer arithmetic main") {
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.bus.get(program.mainProcedure)

    val stack0 = dsg.adjust(dsg.find(dsg.stackMapping(0).cells(0)).getPointee)
    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)
    val stack32 = dsg.adjust(dsg.find(dsg.stackMapping(32).cells(0)).getPointee)
    val stack40 = dsg.adjust(dsg.find(dsg.stackMapping(40).cells(0)).getPointee)

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
    assert(dsg.find(dsg.stackMapping(40).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.find(dsg.stackMapping(32).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.find(dsg.stackMapping(24).cells(0)).getPointee.internalOffset == 0)
  }

  // top down tests
  ignore("top down jumptable2 main") {
    // no changes should be made from previous phase
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")
    val program = results.ir.program
    val dsg = results.analysis.get.tds.get(program.mainProcedure)
//    assert(dsg.pointTo.size == 13) // 13

    val framePointer = dsg.find(dsg.stackMapping(0).cells(0))
    val stack8 = dsg.find(dsg.stackMapping(8).cells(0))
    val stack16 = dsg.find(dsg.stackMapping(16).cells(0))
    val stack28 = dsg.find(dsg.stackMapping(28).cells(0))
    assert(dsg.adjust(framePointer.getPointee).equals(dsg.adjust(dsg.formals(R29))))
    assert(dsg.adjust(stack8.getPointee).equals(dsg.adjust(dsg.formals(R30))))
    assert(dsg.adjust(stack16.getPointee).equals(dsg.adjust(dsg.formals(R1))))
    assert(dsg.adjust(stack28.getPointee).equals(dsg.adjust(dsg.formals(R0))))
    assertJumptable2Globals(dsg)

    // bottom-up
    assert(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4)).node.cells(0)).getPointee.node.collapsed)

  }

  ignore("top down jumptable2 callees") {
    val results = runTest("src/test/indirect_calls/jumptable2/gcc_pic/jumptable2")

    val program = results.ir.program
    // test that all three callees have the same local graph
    val callees = Set("sub_seven", "add_two", "add_six")
    val procs = program.nameToProcedure
    callees.foreach { callee =>
      val dsg = results.analysis.get.tds.get(procs(callee))
      assert(dsg.stackMapping.isEmpty) // stack is not used in either callee
      assertJumptable2Globals(dsg) // globals should be the same everywhere unused in callees
      // x should point to a collapsed object, in all 3 functions
      // all three load value of x
      // the analysis doesn't know if x is a pointer or not therefore assumes it is for soundness
      // arbitrary pointer is used in arithmetic causing collapse
      assert(dsg.adjust(dsg.find(dsg.globalMapping(AddressRange(69648, 69652)).node.cells(0)).getPointee).node.get.collapsed)
    }
  }

  test("top down interproc pointer arithmetic callee") {
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.tds.get(program.nameToProcedure("callee"))

    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)

    // callee should now have different offsets due to formal and actual input parameters being unified
    assert(stack8.equals(dsg.adjust(dsg.formals(R0))))
    assert(stack8.offset == 16)
    assert(stack24.equals(dsg.adjust(dsg.formals(R0)).node.get.cells(32)))
  }

  // top-down phase should be the same as bottom-up phase
  ignore("top down interproc pointer arithmetic main") {
    val results = runTest("src/test/dsa/interproc_pointer_arithmetic/interproc_pointer_arithmetic")
    val program = results.ir.program
    val dsg = results.analysis.get.tds.get(program.mainProcedure)

    val stack0 = dsg.adjust(dsg.find(dsg.stackMapping(0).cells(0)).getPointee)
    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)
    val stack32 = dsg.adjust(dsg.find(dsg.stackMapping(32).cells(0)).getPointee)
    val stack40 = dsg.adjust(dsg.find(dsg.stackMapping(40).cells(0)).getPointee)

    assert(stack0.equals(dsg.adjust(dsg.formals(R29))))
    assert(stack8.equals(dsg.adjust(dsg.formals(R30))))
    assert(stack24.node.get.equals(stack32.node.get))
    assert(stack24.offset == 0)
    assert(stack32.offset == 16)
    assert(stack40.pointee.isDefined)
    assert(stack40.node.get.equals(stack24.node.get))
    assert(stack40.offset == 32)
    assert(dsg.find(dsg.stackMapping(40).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.find(dsg.stackMapping(32).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.find(dsg.stackMapping(24).cells(0)).getPointee.internalOffset == 0)
  }

}
