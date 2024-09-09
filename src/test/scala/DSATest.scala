import analysis.{AddressRange, DSC, DSG, DSN, DataLocation, Derm, HeapLocation}
import ir.Endian.BigEndian
import ir.{Assign, BVADD, BinaryExpr, BitVecLiteral, CFGPosition, ConvertToSingleProcedureReturn, DirectCall, Memory, MemoryAssign, MemoryLoad, Register, SharedMemory}
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil
import ir.dsl.*
import specification.Specification
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, IRContext, RunUtils, StaticAnalysisConfig}

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
class DSATest extends AnyFunSuite, TestUtil {

  // Local DSA tests
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
    assert(dsg.adjust(stack32.getPointee).equals(stack24))  //
    assert(stack24.node.get.collapsed) // stack24 is collapsed
    assert(dsg.adjust(stack24.getPointee).equals(stack24))
    assert(dsg.find(dsg.adjust(stack40.getPointee)).equals(dsg.find(dsg.adjust(dsg.find(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69600))._1.cells(0).getPointee)).getPointee))))

    writeToFile(dsg.toDot, "test1.dot")
  }

  test("local jumptable2 sub_seven") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.locals.get(program.procs("sub_seven"))
//    assert(dsg.pointTo.size == 9)
    assert(dsg.stackMapping.isEmpty)
    assert(dsg.adjust(dsg.find(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0)).getPointee).node.get.collapsed)

    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))

  }

  test("local jumptable2 add_six") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.locals.get(program.procs("add_six"))
//    assert(dsg.pointTo.size == 9)
    assert(dsg.stackMapping.isEmpty)
    assert(dsg.adjust(dsg.find(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0)).getPointee).node.get.collapsed)

    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))

  }

  test("local jumptable2 add_two") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.locals.get(program.procs("add_two"))
//    assert(dsg.pointTo.size == 9)
    assert(dsg.stackMapping.isEmpty)
    assert(dsg.adjust(dsg.find(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0)).getPointee).node.get.collapsed)

    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))

  }

  test("local jumptable2 main") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )


    val program = results.ir.program
    val dsg = results.analysis.get.locals.get(program.mainProcedure)
//    assert(dsg.pointTo.size == 12) // 12
    val framePointer = dsg.find(dsg.stackMapping(0).cells(0))
    val stack8 = dsg.find(dsg.stackMapping(8).cells(0))
    val stack16 = dsg.find(dsg.stackMapping(16).cells(0))
    val stack28 = dsg.find(dsg.stackMapping(28).cells(0))
    assert(dsg.adjust(framePointer.getPointee).equals(dsg.adjust(dsg.formals(R29))))
    assert(dsg.adjust(stack8.getPointee).equals(dsg.adjust(dsg.formals(R30))))
    assert(dsg.adjust(stack16.getPointee).equals(dsg.adjust(dsg.formals(R1))))
    assert(dsg.adjust(stack28.getPointee).equals(dsg.adjust(dsg.formals(R0))))

    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))


  }

  test("unsafe pointer arithmetic") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/unsafe_pointer_arithmetic/unsafe_pointer_arithmetic.adt",
          relfFile = "examples/unsafe_pointer_arithmetic/unsafe_pointer_arithmetic.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )


    val program = results.ir.program
    val dsg = results.analysis.get.locals.get(program.mainProcedure)
    writeToFile(dsg.toDot, "test2.dot")

    val stack0 = dsg.adjust(dsg.find(dsg.stackMapping(0).cells(0)).getPointee)
    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)
    val stack32 = dsg.adjust(dsg.find(dsg.stackMapping(32).cells(0)).getPointee)
    val stack40 = dsg.adjust(dsg.find(dsg.stackMapping(40).cells(0)).getPointee)
    val stack48 = dsg.adjust(dsg.find(dsg.stackMapping(48).cells(0)).getPointee)
    val stack56 = dsg.adjust(dsg.find(dsg.stackMapping(56).cells(0)).getPointee)
//    assert(dsg.pointTo.size==10)
    assert(stack0.equals(dsg.adjust(dsg.formals(R29))))
    assert(stack8.equals(dsg.adjust(dsg.formals(R30))))
    assert(stack24.equals(stack32))
    assert(stack24.offset == 0)
    assert(stack24.node.get.allocationRegions.size == 1)
    assert(stack24.node.get.allocationRegions.head.asInstanceOf[HeapLocation].size == 20)
    assert(stack40.node.get.allocationRegions.size == 1)
    assert(stack48.node.get.allocationRegions.head.asInstanceOf[HeapLocation].size == 8)
    assert(dsg.adjust(stack48.getPointee).equals(stack40))
    assert(dsg.adjust(stack48.getPointee).equals(stack56))
    val unadjustedStack24Pointee = dsg.find(dsg.stackMapping(24).cells(0)).getPointee
    val unadjustedStack40Pointee = dsg.find(dsg.stackMapping(40).cells(0)).getPointee
    assert(unadjustedStack24Pointee.cell.equals(unadjustedStack40Pointee.cell))
    assert(unadjustedStack40Pointee.internalOffset == 1)
    assert(unadjustedStack24Pointee.internalOffset == 0)
    assert(unadjustedStack24Pointee.offset == 0)
  }

  test("interproc pointer arithmetic main") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.locals.get(program.mainProcedure)
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
    assert(stack40._pointee.isDefined)
    assert(!stack40.node.get.equals(stack24.node.get))
  }

  test("interproc pointer arithmetic callee") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.locals.get(program.procs("callee"))
    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)

    assert(stack8.equals(dsg.adjust(dsg.formals(R0))))
    assert(stack8.offset == 0)
    assert(stack24.equals(dsg.adjust(dsg.formals(R0)).node.get.cells(16)))
  }


  test("internal merge") {
    val mem = SharedMemory("mem", 10000, 10000)
    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    var program = prog(
      proc("main",
        block("operations",
//          Assign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          locAssign1,
          locAssign2,
          MemoryAssign(mem,  R7, R1, BigEndian, 64, Some("00003")),
          MemoryAssign(mem,  R6, R2, BigEndian, 64, Some("00004")),
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Set.empty, Map.empty, Specification(Set(), Set(), Map(), List(), List(), List(), Set()), program))
    val dsg: DSG = results.locals.get(program.mainProcedure)
    assert(dsg.adjust(dsg.formals(R1)).equals(dsg.adjust(dsg.formals(R2))))
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).cell.equals(dsg.find(dsg.varToCell(locAssign2)(R7)).cell))
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).internalOffset == 0)
    assert(dsg.find(dsg.varToCell(locAssign2)(R7)).internalOffset == 1)
    assert(dsg.adjust(dsg.varToCell(locAssign1)(R6))._pointee.isDefined)
    assert(dsg.adjust(dsg.adjust(dsg.varToCell(locAssign1)(R6)).getPointee).equals(dsg.adjust(dsg.formals(R1))))

  }

  test("offsetting from middle of cell to a new cell") {
    val mem = SharedMemory("mem", 10000, 10000)
    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = Assign(R5, BinaryExpr(BVADD, R7,  BitVecLiteral(8, 64)), Some("00005"))

    var program = prog(
      proc("main",
        block("operations",
          locAssign1,
          locAssign2,
          MemoryAssign(mem, R7, R1, BigEndian, 64, Some("00003")),
          MemoryAssign(mem, R6, R2, BigEndian, 64, Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Set.empty, Map.empty, Specification(Set(), Set(), Map(), List(), List(), List(), Set()), program))
    val dsg: DSG = results.locals.get(program.mainProcedure)
    assert(dsg.find(dsg.varToCell(locAssign3)(R5)).offset == 13)
  }

  test("offsetting from middle of cell to the same cell") {
    val mem = SharedMemory("mem", 10000, 10000)
    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = Assign(R5, BinaryExpr(BVADD, R7, BitVecLiteral(7, 64)), Some("00005"))

    var program = prog(
      proc("main",
        block("operations",
          //          Assign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          locAssign1,
          locAssign2,
          MemoryAssign(mem,  R7, R1, BigEndian, 64, Some("00003")),
          MemoryAssign(mem,  R6, R2, BigEndian, 64, Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Set.empty, Map.empty, Specification(Set(), Set(), Map(), List(), List(), List(), Set()), program))
    val dsg: DSG = results.locals.get(program.mainProcedure)
    assert(dsg.find(dsg.formals(R1)).equals(dsg.find(dsg.formals(R2))))
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).cell.equals(dsg.find(dsg.varToCell(locAssign2)(R7)).cell))
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).cell.equals(dsg.find(dsg.varToCell(locAssign3)(R5)).cell))
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).internalOffset == 0)
    assert(dsg.find(dsg.varToCell(locAssign2)(R7)).internalOffset == 1)
    assert(dsg.find(dsg.varToCell(locAssign3)(R5)).internalOffset == 8)
    assert(dsg.find(dsg.varToCell(locAssign1)(R6)).cell._pointee.isDefined)
    assert(dsg.find(dsg.find(dsg.varToCell(locAssign1)(R6)).cell.getPointee).equals(dsg.find(dsg.formals(R1))))
  }

  test("internal offset transfer") {
    val mem = SharedMemory("mem", 10000, 10000)
    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = Assign(R5, R7, Some("00005"))

    var program = prog(
      proc("main",
        block("operations",
          //          Assign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          locAssign1,
          locAssign2,
          MemoryAssign(mem,  R7, R1, BigEndian, 64, Some("00003")),
          MemoryAssign(mem,  R6, R2, BigEndian, 64, Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Set.empty, Map.empty, Specification(Set(), Set(), Map(), List(), List(), List(), Set()), program))
    val dsg: DSG = results.locals.get(program.mainProcedure)
    assert(dsg.find(dsg.varToCell(locAssign2)(R7)).equals(dsg.find(dsg.varToCell(locAssign3)(R5))))
  }

  // bottom up tests
  test("bottom up jumptable2 sub_seven") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.bus.get(program.procs("sub_seven"))
    assert(dsg.stackMapping.isEmpty)
    assert(dsg.find(dsg.find(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0)).getPointee).node.collapsed)


    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))

  }

  test("bottom up jumptable2 add_six") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.bus.get(program.procs("add_six"))
    assert(dsg.stackMapping.isEmpty)
    assert(dsg.find(dsg.find(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0)).getPointee).node.collapsed)

    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))

  }

  test("bottomup jumptable2 add_two") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.bus.get(program.procs("add_two"))
    assert(dsg.stackMapping.isEmpty)
    assert(dsg.find(dsg.find(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0)).getPointee).node.collapsed)

    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))

  }

  test("bottom up jumptable2 main") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )


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
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))

    // bu
    assert(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)).getPointee.node.collapsed)

  }



  test("bottom up interproc pointer arithmetic callee") {
    // same as interproc pointer arithmetic callee's local graph (no changes should have been made)
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.bus.get(program.procs("callee"))
    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)

    assert(stack8.equals(dsg.adjust(dsg.formals(R0))))
    assert(stack8.offset == 0)
    assert(stack24.equals(dsg.adjust(dsg.formals(R0)).node.get.cells(16)))

  }


  test("bottom up interproc pointer arithmetic main") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.bus.get(program.mainProcedure)

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
    assert(stack40._pointee.isDefined)
    assert(stack40.node.get.equals(stack24.node.get))
    assert(stack40.offset == 32)
    assert(dsg.find(dsg.stackMapping(40).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.find(dsg.stackMapping(32).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.find(dsg.stackMapping(24).cells(0)).getPointee.internalOffset == 0)

  }


  // top down tests
  test("top down jumptable2 main") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )

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


    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))

    // bu
    assert(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)).getPointee.node.collapsed)
    
  }

  test("top down jumptable2 sub_seven") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.tds.get(program.procs("sub_seven"))
    assert(dsg.stackMapping.isEmpty)
    assert(dsg.find(dsg.find(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0)).getPointee).node.collapsed)


    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))


  }

  test("top down jumptable2 add_six") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.tds.get(program.procs("add_six"))
    assert(dsg.stackMapping.isEmpty)
    assert(dsg.find(dsg.find(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0)).getPointee).node.collapsed)


    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))


  }

  test("top down jumptable2 add_two") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/jumptable2/jumptable2.adt",
          relfFile = "examples/jumptable2/jumptable2.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.tds.get(program.procs("add_two"))
    assert(dsg.stackMapping.isEmpty)
    assert(dsg.find(dsg.find(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0)).getPointee).node.collapsed)


    // initial global mappings
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0))))
    assert(dsg.adjust(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0).getPointee).equals(dsg.find(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))))


  }

  test("top down interproc pointer arithmetic callee") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
    val program = results.ir.program
    val dsg = results.analysis.get.tds.get(program.procs("callee"))

    val stack8 = dsg.adjust(dsg.find(dsg.stackMapping(8).cells(0)).getPointee)
    val stack24 = dsg.adjust(dsg.find(dsg.stackMapping(24).cells(0)).getPointee)

    assert(stack8.equals(dsg.adjust(dsg.formals(R0))))
    assert(stack8.offset == 16)
    assert(stack24.equals(dsg.adjust(dsg.formals(R0)).node.get.cells(32)))

  }


  // top down phase should be the same as bu phase
  test("top down interproc pointer arithmetic main") {
    val results = RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
          specFile = None,
          dumpIL = None,
        ),
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
      )
    )
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
    assert(stack40._pointee.isDefined)
    assert(stack40.node.get.equals(stack24.node.get))
    assert(stack40.offset == 32)
    assert(dsg.find(dsg.stackMapping(40).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.find(dsg.stackMapping(32).cells(0)).getPointee.internalOffset == 0)
    assert(dsg.find(dsg.stackMapping(24).cells(0)).getPointee.internalOffset == 0)

  }

}
