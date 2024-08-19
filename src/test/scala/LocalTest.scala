import analysis.{AddressRange, DSC, DSG, DSN, DataLocation, Derm, HeapLocation}
import ir.Endian.BigEndian
import ir.{Assign, BVADD, BinaryExpr, BitVecLiteral, ConvertToSingleProcedureReturn, DirectCall, Memory, MemoryAssign, MemoryLoad, SharedMemory}
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil
import ir.dsl.*
import specification.Specification
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, IRContext, RunUtils, StaticAnalysisConfig}

class LocalTest extends AnyFunSuite, TestUtil {

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
    val dsg = results.analysis.get.locals.get(program.mainProcedure)
//    assert(dsg.pointTo.size == 12) // 12

    val framePointer = dsg.adjust(dsg.find(dsg.stackMapping(0).cells(0)).getPointee)
    val R29formal = dsg.adjust(dsg.formals(R29))
    assert(framePointer.equals(R29formal))
    val stack8 = dsg.find(dsg.stackMapping(8).cells(0)) //  R31 + 8
    assert(dsg.adjust(stack8.getPointee).equals(dsg.adjust(dsg.formals(R30))))
    val stack40 = dsg.find(dsg.stackMapping(40).cells(0))//  R31 + 40
    val stack32 = dsg.find(dsg.stackMapping(32).cells(0)) //  R31 + 32
    val stack24 = dsg.find(dsg.stackMapping(24).cells(0)) //  R31 + 24 and Malloc
    assert(dsg.adjust(stack32.getPointee).equals(stack24))
    assert(stack24.node.get.collapsed)
    assert(dsg.adjust(stack24.getPointee).equals(stack24))
    assert(dsg.find(dsg.adjust(stack40.getPointee)).equals(dsg.find(dsg.adjust(dsg.find(dsg.adjust(dsg.globalMapping(AddressRange(69600, 69600))._1.cells(0).getPointee)).getPointee))))

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
//
//
//
//  ignore("local jumptable2_clang main") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/jumptable2/jumptable2_clang.adt",
//          relfFile = "examples/jumptable2/jumptable2_clang.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.locals.get(program.mainProcedure)
////    assert(dsg.pointTo.size == 7)
////    assert(dsg.stackMapping.isEmpty)
////    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69680, 69684))._1.cells(0))._1.node.get.collapsed)
//  }
//
//
//
//
//  ignore("interproc unsafe pointer arithmetic") {
//    // test interproc unification with points-to that have internal offsets into cells
//  }
//
//
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
    assert(stack24.equals(stack40))
    assert(stack40.offset == 1) // todo check
  }
//
//  test("interproc pointer arithmetic main") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
//          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.locals.get(program.mainProcedure)
//    val stack0 = dsg.stackMapping(0).cells(0)
//    val stack8 = dsg.stackMapping(8).cells(0)
//    val stack24 = dsg.stackMapping(24).cells(0)
//    val stack32 = dsg.stackMapping(32).cells(0)
//    val stack40 = dsg.stackMapping(40).cells(0)
//    assert(dsg.pointTo.size == 9)
//    assert(dsg.pointTo(stack0).equals(dsg.formals(R29)))
//    assert(dsg.pointTo(stack8).equals(dsg.formals(R30)))
//    assert(dsg.pointTo(stack24)._1.node.get.equals(dsg.pointTo(stack32)._1.node.get))
//    assert(dsg.pointTo(stack24)._1.offset == 0)
//    assert(dsg.pointTo(stack32)._1.offset == 16)
//    assert(dsg.pointTo.contains(dsg.pointTo(stack40)._1))
//    assert(!dsg.pointTo(stack40)._1.node.get.equals(dsg.pointTo(stack24)._1.node.get))
//  }
//
//  test("interproc pointer arithmetic callee") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
//          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.locals.get(program.procs("callee"))
//    val stack8 = dsg.stackMapping(8).cells(0) //  R31 + 8
//    val stack24 = dsg.stackMapping(24).cells(0) //  R31 + 24
//    assert(dsg.pointTo.size == 3)
//    assert(dsg.getPointee(stack8).equals(dsg.formals(R0)))
//    assert(dsg.getPointee(stack8)._1.offset == 0)
//    assert(dsg.getPointee(stack24)._1.equals(dsg.formals(R0)._1.node.get.cells(16)))
//  }
//
//
//  test("internal merge") {
//    val mem = SharedMemory("mem", 10000, 10000)
//    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
//    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
//    var program = prog(
//      proc("main",
//        block("operations",
////          Assign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
//          locAssign1,
//          locAssign2,
//          MemoryAssign(mem,  R7, R1, BigEndian, 64, Some("00003")),
//          MemoryAssign(mem,  R6, R2, BigEndian, 64, Some("00004")),
//          ret
//        )
//      )
//    )
//
//    val returnUnifier = ConvertToSingleProcedureReturn()
//    program = returnUnifier.visitProgram(program)
//
//    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Set.empty, Map.empty, Specification(Set(), Set(), Map(), List(), List(), List(), Set()), program))
//    val dsg: DSG = results.locals.get(program.mainProcedure)
//    assert(dsg.formals(R1).equals(dsg.formals(R2)))
//    assert(dsg.varToCell(locAssign1)(R6)._1.equals(dsg.varToCell(locAssign2)(R7)._1))
//    assert(dsg.varToCell(locAssign1)(R6)._2 == 0)
//    assert(dsg.varToCell(locAssign2)(R7)._2 == 1)
//    assert(dsg.pointTo.contains(dsg.varToCell(locAssign1)(R6)._1))
//    assert(dsg.pointTo(dsg.varToCell(locAssign1)(R6)._1)._1.equals(dsg.formals(R1)._1))
//    assert(dsg.pointTo.size == 1)
//
//  }
//
//  test("offsetting from middle of cell to a new cell") {
//    val mem = SharedMemory("mem", 10000, 10000)
//    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
//    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
//    val locAssign3 = Assign(R5, BinaryExpr(BVADD, R7,  BitVecLiteral(8, 64)), Some("00005"))
//
//    var program = prog(
//      proc("main",
//        block("operations",
//          locAssign1,
//          locAssign2,
//          MemoryAssign(mem, R7, R1, BigEndian, 64, Some("00003")),
//          MemoryAssign(mem, R6, R2, BigEndian, 64, Some("00004")),
//          locAssign3,
//          ret
//        )
//      )
//    )
//
//    val returnUnifier = ConvertToSingleProcedureReturn()
//    program = returnUnifier.visitProgram(program)
//
//    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Set.empty, Map.empty, Specification(Set(), Set(), Map(), List(), List(), List(), Set()), program))
//    val dsg: DSG = results.locals.get(program.mainProcedure)
//    assert(dsg.varToCell(locAssign3)(R5)._1.offset == 13)
//  }
//
//  test("offsetting from middle of cell to the same cell") {
//    val mem = SharedMemory("mem", 10000, 10000)
//    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
//    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
//    val locAssign3 = Assign(R5, BinaryExpr(BVADD, R7, BitVecLiteral(7, 64)), Some("00005"))
//
//    var program = prog(
//      proc("main",
//        block("operations",
//          //          Assign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
//          locAssign1,
//          locAssign2,
//          MemoryAssign(mem,  R7, R1, BigEndian, 64, Some("00003")),
//          MemoryAssign(mem,  R6, R2, BigEndian, 64, Some("00004")),
//          locAssign3,
//          ret
//        )
//      )
//    )
//
//    val returnUnifier = ConvertToSingleProcedureReturn()
//    program = returnUnifier.visitProgram(program)
//
//    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Set.empty, Map.empty, Specification(Set(), Set(), Map(), List(), List(), List(), Set()), program))
//    val dsg: DSG = results.locals.get(program.mainProcedure)
//    assert(dsg.formals(R1).equals(dsg.formals(R2)))
//    assert(dsg.varToCell(locAssign1)(R6)._1.equals(dsg.varToCell(locAssign2)(R7)._1))
//    assert(dsg.varToCell(locAssign1)(R6)._1.equals(dsg.varToCell(locAssign3)(R5)._1))
//    assert(dsg.varToCell(locAssign1)(R6)._2 == 0)
//    assert(dsg.varToCell(locAssign2)(R7)._2 == 1)
//    assert(dsg.varToCell(locAssign3)(R5)._2 == 8)
//    assert(dsg.pointTo.contains(dsg.varToCell(locAssign1)(R6)._1))
//    assert(dsg.pointTo(dsg.varToCell(locAssign1)(R6)._1)._1.equals(dsg.formals(R1)._1))
//    assert(dsg.pointTo.size == 1)
//  }
//
//  test("internal offset transfer") {
//    val mem = SharedMemory("mem", 10000, 10000)
//    val locAssign1 = Assign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
//    val locAssign2 = Assign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
//    val locAssign3 = Assign(R5, R7, Some("00005"))
//
//    var program = prog(
//      proc("main",
//        block("operations",
//          //          Assign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
//          locAssign1,
//          locAssign2,
//          MemoryAssign(mem,  R7, R1, BigEndian, 64, Some("00003")),
//          MemoryAssign(mem,  R6, R2, BigEndian, 64, Some("00004")),
//          locAssign3,
//          ret
//        )
//      )
//    )
//
//    val returnUnifier = ConvertToSingleProcedureReturn()
//    program = returnUnifier.visitProgram(program)
//
//    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Set.empty, Map.empty, Specification(Set(), Set(), Map(), List(), List(), List(), Set()), program))
//    val dsg: DSG = results.locals.get(program.mainProcedure)
//    assert(dsg.varToCell(locAssign2)(R7).equals(dsg.varToCell(locAssign3)(R5)))
//  }
//
//  // bottom up tests
//  test("bottom up jumptable2 sub_seven") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/jumptable2/jumptable2.adt",
//          relfFile = "examples/jumptable2/jumptable2.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.bus.get(program.procs("sub_seven"))
//    assert(dsg.pointTo.size == 9)
//    assert(dsg.stackMapping.isEmpty)
//    println(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))._1.node.get.collapsed)
//
//    // initial global mappings
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8))._1.equals(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16))._1.equals(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)))
//
//
//  }
//
//  test("bottom up jumptable2 add_six") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/jumptable2/jumptable2.adt",
//          relfFile = "examples/jumptable2/jumptable2.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.bus.get(program.procs("add_six"))
//    assert(dsg.pointTo.size == 9)
//    assert(dsg.stackMapping.isEmpty)
//    println(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))._1.node.get.collapsed)
//
//    // initial global mappings
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8))._1.equals(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16))._1.equals(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)))
//
//  }
//
//  test("bottomup jumptable2 add_two") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/jumptable2/jumptable2.adt",
//          relfFile = "examples/jumptable2/jumptable2.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.bus.get(program.procs("add_two"))
//    assert(dsg.pointTo.size == 9)
//    assert(dsg.stackMapping.isEmpty)
//    println(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))._1.node.get.collapsed)
//
//    // initial global mappings
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8))._1.equals(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16))._1.equals(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)))
//
//  }
//
//  test("bottom up jumptable2 main") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/jumptable2/jumptable2.adt",
//          relfFile = "examples/jumptable2/jumptable2.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//
//
//    val program = results.ir.program
//    val dsg = results.analysis.get.bus.get(program.mainProcedure)
//    assert(dsg.pointTo.size == 13) // 13
//    val framePointer = dsg.stackMapping(0).cells(0)
//    val stack8 = dsg.stackMapping(8).cells(0)
//    val stack16 = dsg.stackMapping(16).cells(0)
//    val stack28 = dsg.stackMapping(28).cells(0)
//    assert(dsg.pointTo(framePointer).equals(dsg.formals(R29)))
//    assert(dsg.pointTo(stack8).equals(dsg.formals(R30)))
//    assert(dsg.pointTo(stack16).equals(dsg.formals(R1)))
//    assert(dsg.pointTo(stack28).equals(dsg.formals(R0)))
//
//    // initial global mappings
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8))._1.equals(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16))._1.equals(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)))
//
//    // bu
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))._1.node.get.collapsed)
//
//  }
//
//
//
//  test("bottom up interproc pointer arithmetic callee") {
//    // same as interproc pointer arithmetic callee's local graph (no changes should have been made)
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
//          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.bus.get(program.procs("callee"))
//    val stack8 = dsg.stackMapping(8).cells(0) //  R31 + 8
//    val stack24 = dsg.stackMapping(24).cells(0) //  R31 + 24
//    assert(dsg.pointTo.size == 3)
//    assert(dsg.getPointee(stack8).equals(dsg.formals(R0)))
//    assert(dsg.getPointee(stack8)._1.offset == 0)
//    assert(dsg.getPointee(stack24)._1.equals(dsg.formals(R0)._1.node.get.cells(16)))
//  }
//
//
//  test("bottom up interproc pointer arithmetic main") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
//          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.bus.get(program.mainProcedure)
//    val stack0 = dsg.stackMapping(0).cells(0)
//    val stack8 = dsg.stackMapping(8).cells(0)
//    val stack24 = dsg.stackMapping(24).cells(0)
//    val stack32 = dsg.stackMapping(32).cells(0)
//    val stack40 = dsg.stackMapping(40).cells(0)
//    assert(dsg.pointTo.size == 9)
//    assert(dsg.pointTo(stack0).equals(dsg.formals(R29)))
//    assert(dsg.pointTo(stack8).equals(dsg.formals(R30)))
//    assert(dsg.pointTo(stack24)._1.node.get.equals(dsg.pointTo(stack32)._1.node.get))
//    assert(dsg.pointTo(stack24)._1.offset == 0)
//    assert(dsg.pointTo(stack32)._1.offset == 16)
//    assert(dsg.pointTo.contains(dsg.pointTo(stack40)._1))
//    assert(dsg.pointTo(stack40)._1.node.get.equals(dsg.pointTo(stack24)._1.node.get))
//    assert(dsg.pointTo(stack40)._1.offset == 32)
//    assert(dsg.pointTo(stack40)._2 == 0)
//    assert(dsg.pointTo(stack32)._2 == 0)
//    assert(dsg.pointTo(stack24)._2 == 0)
//  }
//
//
//  // top down tests
//  test("top down jumptable2 main") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/jumptable2/jumptable2.adt",
//          relfFile = "examples/jumptable2/jumptable2.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//
//    val program = results.ir.program
//    val dsg = results.analysis.get.tds.get(program.mainProcedure)
//    assert(dsg.pointTo.size == 13) // 13
//    val framePointer = dsg.stackMapping(0).cells(0)
//    val stack8 = dsg.stackMapping(8).cells(0)
//    val stack16 = dsg.stackMapping(16).cells(0)
//    val stack28 = dsg.stackMapping(28).cells(0)
//    assert(dsg.pointTo(framePointer).equals(dsg.formals(R29)))
//    assert(dsg.pointTo(stack8).equals(dsg.formals(R30)))
//    assert(dsg.pointTo(stack16).equals(dsg.formals(R1)))
//    assert(dsg.pointTo(stack28).equals(dsg.formals(R0)))
//
//    // initial global mappings
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8))._1.equals(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16))._1.equals(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)))
//
//    // bu
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0))._1.node.get.collapsed)
//  }
//
//  test("top down jumptable2 sub_seven") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/jumptable2/jumptable2.adt",
//          relfFile = "examples/jumptable2/jumptable2.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.tds.get(program.procs("sub_seven"))
//    assert(dsg.pointTo.size == 9)
//    assert(dsg.stackMapping.isEmpty)
//    println(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))._1.node.get.collapsed)
//
//    // initial global mappings
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8))._1.equals(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16))._1.equals(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)))
//
//
//  }
//
//  test("top down jumptable2 add_six") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/jumptable2/jumptable2.adt",
//          relfFile = "examples/jumptable2/jumptable2.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.tds.get(program.procs("add_six"))
//    assert(dsg.pointTo.size == 9)
//    assert(dsg.stackMapping.isEmpty)
//    println(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))._1.node.get.collapsed)
//
//    // initial global mappings
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8))._1.equals(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16))._1.equals(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)))
//
//  }
//
//  test("top down jumptable2 add_two") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/jumptable2/jumptable2.adt",
//          relfFile = "examples/jumptable2/jumptable2.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.tds.get(program.procs("add_two"))
//    assert(dsg.pointTo.size == 9)
//    assert(dsg.stackMapping.isEmpty)
//    println(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69648, 69652))._1.cells(0))._1.node.get.collapsed)
//
//    // initial global mappings
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69600, 69608))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2136, 2136 + 124))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(1948, 1948 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69624, 69632))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69608, 69616))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2056, 2056 + 76))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(8))._1.equals(dsg.globalMapping(AddressRange(1984, 1984 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69560, 69568))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(2264, 2268))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69656, 69656 + 24))._1.cells(16))._1.equals(dsg.globalMapping(AddressRange(2020, 2020 + 36))._1.cells(0)))
//    assert(dsg.pointTo(dsg.globalMapping(AddressRange(69584, 69584 + 8))._1.cells(0))._1.equals(dsg.globalMapping(AddressRange(69648, 69648 + 4))._1.cells(0)))
//
//  }
//
//  test("top down interproc pointer arithmetic callee") {
//    // same as interproc pointer arithmetic callee's local graph (no changes should have been made)
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
//          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.tds.get(program.procs("callee"))
//    val stack8 = dsg.stackMapping(8).cells(0) //  R31 + 8
//    val stack24 = dsg.stackMapping(24).cells(0) //  R31 + 24
//    assert(dsg.pointTo.size == 6)
//    assert(dsg.getPointee(stack8).equals(dsg.formals(R0)))
//    assert(dsg.getPointee(stack8)._1.offset == 16)
//    assert(dsg.getPointee(stack24)._1.equals(dsg.formals(R0)._1.node.get.cells(32)))
//  }
//
//
//  // top down phase should be the same as bu phase
//  test("top down interproc pointer arithmetic main") {
//    val results = RunUtils.loadAndTranslate(
//      BASILConfig(
//        loading = ILLoadingConfig(
//          inputFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.adt",
//          relfFile = "examples/interproc_pointer_arithmetic/interproc_pointer_arithmetic.relf",
//          specFile = None,
//          dumpIL = None,
//        ),
//        staticAnalysis = Some(StaticAnalysisConfig()),
//        boogieTranslation = BoogieGeneratorConfig(),
//        outputPrefix = "boogie_out",
//      )
//    )
//    val program = results.ir.program
//    val dsg = results.analysis.get.tds.get(program.mainProcedure)
//    val stack0 = dsg.stackMapping(0).cells(0)
//    val stack8 = dsg.stackMapping(8).cells(0)
//    val stack24 = dsg.stackMapping(24).cells(0)
//    val stack32 = dsg.stackMapping(32).cells(0)
//    val stack40 = dsg.stackMapping(40).cells(0)
//    assert(dsg.pointTo.size == 9)
//    assert(dsg.pointTo(stack0).equals(dsg.formals(R29)))
//    assert(dsg.pointTo(stack8).equals(dsg.formals(R30)))
//    assert(dsg.pointTo(stack24)._1.node.get.equals(dsg.pointTo(stack32)._1.node.get))
//    assert(dsg.pointTo(stack24)._1.offset == 0)
//    assert(dsg.pointTo(stack32)._1.offset == 16)
//    assert(dsg.pointTo.contains(dsg.pointTo(stack40)._1))
//    assert(dsg.pointTo(stack40)._1.node.get.equals(dsg.pointTo(stack24)._1.node.get))
//    assert(dsg.pointTo(stack40)._1.offset == 32)
//    assert(dsg.pointTo(stack40)._2 == 0)
//    assert(dsg.pointTo(stack32)._2 == 0)
//    assert(dsg.pointTo(stack24)._2 == 0)
//  }
//
}
