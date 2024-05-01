import ir.Endian.BigEndian
import ir.{BVADD, BinaryExpr, BitVecLiteral, ConvertToSingleProcedureReturn, DirectCall, LocalAssign, Memory, MemoryAssign, MemoryLoad, MemoryStore}
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil
import ir.dsl.*
import specification.Specification
import util.{IRContext, RunUtils, StaticAnalysisConfig}

class LocalTest extends AnyFunSuite, TestUtil {
  test("internal merge") {
    val mem = Memory("mem", 10000, 10000)
    var program = prog(
      proc("main",
        block("operations",
//          LocalAssign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001")),
          LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002")),
          MemoryAssign(mem, MemoryStore(mem, R7, R1, BigEndian, 64), Some("00003")),
          MemoryAssign(mem, MemoryStore(mem, R6, R2, BigEndian, 64), Some("00004")),
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Map.empty, Specification(Set(), Map(), List(), List(), List(), Set()), program))


  }

  test("offseting from middle of cell") {
    val mem = Memory("mem", 10000, 10000)
    var program = prog(
      proc("main",
        block("operations",
          //          LocalAssign(R0, MemoryLoad(mem, R0, BigEndian, 0), Some("00000")),
          LocalAssign(R6, BinaryExpr(BVADD, R0, BitVecLiteral(4, 64)), Some("00001")),
          LocalAssign(R7, BinaryExpr(BVADD, R0, BitVecLiteral(5, 64)), Some("00002")),
          MemoryAssign(mem, MemoryStore(mem, R7, R1, BigEndian, 64), Some("00003")),
          MemoryAssign(mem, MemoryStore(mem, R6, R2, BigEndian, 64), Some("00004")),
          LocalAssign(R5, BinaryExpr(BVADD, R7,  BitVecLiteral(16, 64)), Some("00005")), // TODO check with 8
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val results = RunUtils.staticAnalysis(StaticAnalysisConfig(None, None, None), IRContext(Set.empty, Set.empty, Map.empty, Specification(Set(), Map(), List(), List(), List(), Set()), program))


  }
}
