import analysis.{DSC, DSG, DSN}
import ir.Endian.BigEndian
import ir.{BVADD, BinaryExpr, BitVecLiteral, ConvertToSingleProcedureReturn, DirectCall, LocalAssign, Memory, MemoryAssign, MemoryLoad, MemoryStore}
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil
import ir.dsl.*
import specification.Specification
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, IRContext, RunUtils, StaticAnalysisConfig}

class LocalTest extends AnyFunSuite, TestUtil {

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
    val dsg = results.analysis.get.dsg.get
    assert(dsg.pointTo.size == 9)
    val framePointer = DSC(Some(DSN(None, 0, 1)), 0) // R31
    assert(dsg.pointTo(framePointer).equals(dsg.formals(R29)._1))
    val stack8 = DSC(Some(DSN(None, 0, 2)), 0) // R31 + 8
    assert(dsg.pointTo(stack8).equals(dsg.formals(R30)._1))
    val stack40 = DSC(Some(DSN(None, 0, 3)), 0) // R31 + 40
    val stack32 = DSC(Some(DSN(None, 0, 5)), 0) // R31 + 32
    val stack24 = dsg.pointTo(stack32) // R31 + 24 and Malloc
    assert(stack24.node.get.collapsed)
    assert(dsg.pointTo(stack24).equals(stack24))
    assert(dsg.pointTo(stack40).equals(dsg.getPointee(dsg.getPointee(DSC(Some(DSN(None,0, 12)), 0)))))

//    assert(dsg.pointTo.contains(framePointer))
  }


  test("internal merge") {
    val mem = Memory("mem", 10000, 10000)
    val locAssign1 = LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    var program = prog(
      proc("main",
        block("operations",
//          LocalAssign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          locAssign1,
          locAssign2,
          MemoryAssign(mem, MemoryStore(mem, R7, R1, BigEndian, 64), Some("00003")),
          MemoryAssign(mem, MemoryStore(mem, R6, R2, BigEndian, 64), Some("00004")),
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Map.empty, Specification(Set(), Map(), List(), List(), List(), Set()), program))
    val dsg: DSG = results.dsg.get
    assert(dsg.formals(R1).equals(dsg.formals(R2)))
    assert(dsg.varToCell(locAssign1)(R6)._1.equals(dsg.varToCell(locAssign2)(R7)._1))
    assert(dsg.varToCell(locAssign1)(R6)._2 == 0)
    assert(dsg.varToCell(locAssign2)(R7)._2 == 1)
    assert(dsg.pointTo.contains(dsg.varToCell(locAssign1)(R6)._1))
    assert(dsg.pointTo(dsg.varToCell(locAssign1)(R6)._1).equals(dsg.formals(R1)._1))
    assert(dsg.pointTo.size == 1)

  }

  test("offsetting from middle of cell to a new cell") {
    val mem = Memory("mem", 10000, 10000)
    val locAssign1 = LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = LocalAssign(R5, BinaryExpr(BVADD, R7,  BitVecLiteral(8, 64)), Some("00005"))

    var program = prog(
      proc("main",
        block("operations",
          locAssign1,
          locAssign2,
          MemoryAssign(mem, MemoryStore(mem, R7, R1, BigEndian, 64), Some("00003")),
          MemoryAssign(mem, MemoryStore(mem, R6, R2, BigEndian, 64), Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Map.empty, Specification(Set(), Map(), List(), List(), List(), Set()), program))
    val dsg: DSG = results.dsg.get
    assert(dsg.varToCell(locAssign3)(R5)._1.offset == 13)
  }

  test("offsetting from middle of cell to the same cell") {
    val mem = Memory("mem", 10000, 10000)
    val locAssign1 = LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = LocalAssign(R5, BinaryExpr(BVADD, R7, BitVecLiteral(7, 64)), Some("00005"))

    var program = prog(
      proc("main",
        block("operations",
          //          LocalAssign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          locAssign1,
          locAssign2,
          MemoryAssign(mem, MemoryStore(mem, R7, R1, BigEndian, 64), Some("00003")),
          MemoryAssign(mem, MemoryStore(mem, R6, R2, BigEndian, 64), Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Map.empty, Specification(Set(), Map(), List(), List(), List(), Set()), program))
    val dsg: DSG = results.dsg.get
    assert(dsg.formals(R1).equals(dsg.formals(R2)))
    assert(dsg.varToCell(locAssign1)(R6)._1.equals(dsg.varToCell(locAssign2)(R7)._1))
    assert(dsg.varToCell(locAssign1)(R6)._1.equals(dsg.varToCell(locAssign3)(R5)._1))
    assert(dsg.varToCell(locAssign1)(R6)._2 == 0)
    assert(dsg.varToCell(locAssign2)(R7)._2 == 1)
    assert(dsg.varToCell(locAssign3)(R5)._2 == 8)
    assert(dsg.pointTo.contains(dsg.varToCell(locAssign1)(R6)._1))
    assert(dsg.pointTo(dsg.varToCell(locAssign1)(R6)._1).equals(dsg.formals(R1)._1))
    assert(dsg.pointTo.size == 1)
  }

  test("internal offset transfer") {
    val mem = Memory("mem", 10000, 10000)
    val locAssign1 = LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001"))
    val locAssign2 = LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002"))
    val locAssign3 = LocalAssign(R5, R7, Some("00005"))

    var program = prog(
      proc("main",
        block("operations",
          //          LocalAssign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          locAssign1,
          locAssign2,
          MemoryAssign(mem, MemoryStore(mem, R7, R1, BigEndian, 64), Some("00003")),
          MemoryAssign(mem, MemoryStore(mem, R6, R2, BigEndian, 64), Some("00004")),
          locAssign3,
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Map.empty, Specification(Set(), Map(), List(), List(), List(), Set()), program))
    val dsg: DSG = results.dsg.get
    assert(dsg.varToCell(locAssign2)(R7).equals(dsg.varToCell(locAssign3)(R5)))
  }
}
