import analysis.StaticAnalysisContext
import boogie.*
import ir.Endian.LittleEndian
import ir.dsl.*
import ir.{IRContext, *}
import org.scalatest.*
import org.scalatest.funsuite.*
import specification.*
import test_util.CaptureOutput
import util.StaticAnalysisConfig

@test_util.tags.DisabledTest
class PointsToTest extends AnyFunSuite with CaptureOutput with OneInstancePerTest {

  def runAnalyses(
    program: Program,
    externalFunctions: Set[ExternalFunction] = Set.empty,
    globals: Set[SpecGlobal] = Set.empty,
    funcEntries: Set[FuncEntry] = Set.empty,
    globalOffsets: Map[BigInt, BigInt] = Map.empty
  ): StaticAnalysisContext = {

    val ctx = IRContext(
      List.empty,
      externalFunctions,
      globals,
      funcEntries,
      globalOffsets,
      Specification(Set(), Set(), Map(), List(), List(), List(), Set()),
      program
    )
    analysis.AnalysisPipelineMRA.analyse(ctx, StaticAnalysisConfig(), 1)
  }

  /** Test that the analysis correctly identifies the stack pointer even when it is aliased
    */
  test("stack pointer aliasing: MMM Stage") {
    val program: Program = prog(
      proc(
        "main",
        block("0x0", LocalAssign(R6, R31), goto("0x1")),
        block(
          "0x1",
          MemoryStore(mem, BinaryExpr(BVADD, R6, bv64(4)), bv64(10), LittleEndian, 64),
          goto("returntarget")
        ),
        block("returntarget", ret)
      )
    )

    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val results = runAnalyses(program)
    results.mmmResults.pushContext("main")
    assert(results.mmmResults.findStackObject(BigInt(4)).isDefined)
    assert(results.mmmResults.findStackObject(BigInt(4)).get.start == BigInt(4))
    assert(results.mmmResults.findStackObject(BigInt(4)).get.regionIdentifier == "stack_1")
  }

  /** Test that the analysis correctly identifies stack pointers within regions
    */
  test("approximate stack region: MMM Stage") {
    val program: Program = prog(
      proc(
        "main",
        block(
          "0x0",
          MemoryLoad(R1, mem, BinaryExpr(BVADD, R31, bv64(6)), LittleEndian, 64),
          MemoryLoad(R3, mem, BinaryExpr(BVADD, R31, bv64(4)), LittleEndian, 64),
          goto("0x1")
        ),
        block("0x1", goto("returntarget")),
        block("returntarget", ret)
      )
    )
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val results = runAnalyses(program)
    results.mmmResults.pushContext("main")
    assert(results.mmmResults.findStackObject(BigInt(4)).isDefined)
    assert(results.mmmResults.findStackObject(BigInt(5)).isDefined)
    assert(results.mmmResults.findStackObject(BigInt(6)).isDefined)
    assert(results.mmmResults.findStackObject(BigInt(10)).isDefined)

    assert(results.mmmResults.findStackObject(BigInt(4)).get.start == BigInt(4))
    assert(results.mmmResults.findStackObject(BigInt(5)).get.start == BigInt(4))
    assert(results.mmmResults.findStackObject(BigInt(6)).get.start == BigInt(6))
    assert(results.mmmResults.findStackObject(BigInt(10)).get.start == BigInt(6))
  }

//  /**
//   * Test that the analysis correctly collects all stack pointers
//   * TODO: if the region is not modified in the function but only referenced. Is there a need
//   *  to collect to make the analysis complete?
//   */
//  test("collects all types of stack pointers: MRA Stage") {
//    val program: Program = prog(
//      proc("main",
//        block("0x0",
//          Assign(R1, MemoryLoad(mem, BinaryExpr(BVADD, R31, bv64(6)), LittleEndian, 64)),
//          Assign(R3, BinaryExpr(BVADD, R31, bv64(10))),
//          Assign(R4, BinaryExpr(BVADD, R31, bv64(20))),
//          goto("0x1")
//        ),
//        block("0x1",
//          MemoryAssign(mem, MemoryStore(mem, BinaryExpr(BVADD, R31, bv64(4)), bv64(4), LittleEndian, 64)),
//          Assign(R6, MemoryLoad(mem, R3, LittleEndian, 64)),
//          MemoryAssign(mem, MemoryStore(mem, R4, bv64(3), LittleEndian, 64)),
//          goto("returntarget")
//        ),
//        block("returntarget",
//          ret
//        )
//      )
//    )
//
//    runSteensgaardAnalysis(program)
//    results.mmmResults.pushContext("main")
//    assert(results.mmmResults.findStackObject(BigInt(4)).isDefined) // Explicit memStore
//    assert(results.mmmResults.findStackObject(BigInt(6)).isDefined) // Explicit memLoad
//    assert(results.mmmResults.findStackObject(BigInt(10)).isDefined) // Implicit memLoad
//    assert(results.mmmResults.findStackObject(BigInt(20)).isDefined) // Implicit memStore
//
//
//    assert(results.mmmResults.findStackObject(BigInt(4)).get.start == bv64(4))
//    assert(results.mmmResults.findStackObject(BigInt(6)).get.start == bv64(6))
//    assert(results.mmmResults.findStackObject(BigInt(10)).get.start == bv64(10))
//    assert(results.mmmResults.findStackObject(BigInt(20)).get.start == bv64(20))
//  }

  /** Test that the analysis correctly collects shared regions and exposes only the shared ones
    */
  test("collects single function shared regions: MMM Stage") {
    val program: Program = prog(
      proc(
        "main",
        block(
          "0x0",
          MemoryLoad(R0, mem, BinaryExpr(BVADD, R31, bv64(6)), LittleEndian, 64),
          LocalAssign(R1, BinaryExpr(BVADD, R31, bv64(10))),
          goto("0x1")
        ),
        block("0x1", directCall("p2"), goto("returntarget")),
        block("returntarget", ret)
      ),
      proc(
        "p2",
        block("l_p2", LocalAssign(R3, R0), MemoryLoad(R2, mem, R1, LittleEndian, 64), goto("l_p2_1")),
        block("l_p2_1", ret)
      )
    )

    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val results = runAnalyses(program)
    results.mmmResults.pushContext("main")
    assert(results.mmmResults.findStackObject(BigInt(6)).isDefined)

    assert(results.mmmResults.findStackObject(BigInt(6)).get.start == BigInt(6))

    /* ------------------------------------------------------------------------- */

    results.mmmResults.pushContext("p2")
    assert(results.mmmResults.findSharedStackObject(BigInt(6)).nonEmpty)
    assert(results.mmmResults.findSharedStackObject(BigInt(10)).nonEmpty)

    assert(results.mmmResults.findSharedStackObject(BigInt(6)).head.start == BigInt(6))
    assert(results.mmmResults.findSharedStackObject(BigInt(10)).head.start == BigInt(10))
  }

  /** Test that the analysis correctly collects shared regions from multiple functions
    */
  test("collects multiple functions shared regions: MMM Stage") {
    val program: Program = prog(
      proc(
        "main",
        block(
          "0x0",
          MemoryLoad(R0, mem, BinaryExpr(BVADD, R31, bv64(6)), LittleEndian, 64),
          LocalAssign(R1, BinaryExpr(BVADD, R31, bv64(10))),
          goto("0x1")
        ),
        block("0x1", directCall("p2"), goto("returntarget")),
        block("returntarget", ret)
      ),
      proc(
        "foo",
        block(
          "l_foo",
          MemoryLoad(R0, mem, BinaryExpr(BVADD, R31, bv64(6)), LittleEndian, 64),
          LocalAssign(R1, BinaryExpr(BVADD, R31, bv64(10))),
          directCall("p2"),
          goto("l_foo_1")
        ),
        block("l_foo_1", ret)
      ),
      proc(
        "p2",
        block("l_p2", LocalAssign(R3, R0), MemoryLoad(R2, mem, R1, LittleEndian, 64), goto("l_p2_1")),
        block("l_p2_1", ret)
      )
    )

    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val results = runAnalyses(program)
    results.mmmResults.pushContext("main")
    assert(results.mmmResults.findStackObject(BigInt(6)).isDefined)

    assert(results.mmmResults.findStackObject(BigInt(6)).get.start == BigInt(6))

    /* ------------------------------------------------------------------------- */

    results.mmmResults.pushContext("p2")
    assert(results.mmmResults.findSharedStackObject(BigInt(6)).nonEmpty)
    assert(results.mmmResults.findSharedStackObject(BigInt(10)).nonEmpty)

    assert(results.mmmResults.findSharedStackObject(BigInt(6)).size == 2)
    assert(results.mmmResults.findSharedStackObject(BigInt(10)).size == 2)

    assert(results.mmmResults.findSharedStackObject(BigInt(6)).exists(_.parent.name == "main"))
    assert(results.mmmResults.findSharedStackObject(BigInt(6)).exists(_.parent.name == "foo"))
    assert(results.mmmResults.findSharedStackObject(BigInt(10)).exists(_.parent.name == "main"))
    assert(results.mmmResults.findSharedStackObject(BigInt(10)).exists(_.parent.name == "foo"))
  }

//  /**
//   * Test that the analysis correctly resolves a simple format of indirect call
//   */
//  test("resolves simple indirect call: Steensgaard Stage") {
//    val globals =
//        Set(
//            SpecGlobal("foo", 10, None, BigInt(10)),
//            SpecGlobal("bar", 10, None, BigInt(20))
//        )
//    val globalOffsets = Map(BigInt(446) -> BigInt(10), BigInt(447) -> BigInt(20))
//    val program: Program = prog(
//      proc("main",
//        block("0x0",
//          Assign(R0, bv64(400)),
//          Assign(R1, MemoryLoad(mem, BinaryExpr(BVADD, R0, bv64(46)), LittleEndian, 64)),
//          call(R1, Some("returntarget"))
//        ),
//        block("returntarget",
//          ret
//        )
//      ),
//      proc("foo",
//        block("l_foo",
//          Assign(R3, bv64(1)),
//          goto("l_foo_1"),
//        ),
//        block("l_foo_1",
//          ret,
//        )
//      )
//    )
//
//    runSteensgaardAnalysis(program, globals = globals, globalOffsets = globalOffsets)
//  }
}
