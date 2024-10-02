import ir.*
import ir.Endian.LittleEndian
import org.scalatest.*
import org.scalatest.funsuite.*
import specification.*
import boogie.*
import util.{RunUtils, StaticAnalysisConfig, StaticAnalysis, StaticAnalysisContext, IRContext}

import java.io.IOException
import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes

import ir.dsl.*

class PointsToTest extends AnyFunSuite with OneInstancePerTest {

  def runAnalyses(program: Program,
                  externalFunctions: Set[ExternalFunction] = Set.empty,
                  globals: Set[SpecGlobal] = Set.empty,
                  globalOffsets: Map[BigInt, BigInt] = Map.empty): StaticAnalysisContext = {

    val ctx = IRContext(List.empty, externalFunctions, globals, globalOffsets, Specification(Set(), Map(), List(), List(), List(), Set()), program)
    StaticAnalysis.analyse(ctx, StaticAnalysisConfig(), 1)
  }

  /**
   * Test that the analysis correctly identifies the stack pointer even when it is aliased
   */
  test("stack pointer aliasing: MMM Stage") {
    var program: Program = prog(
      proc("main",
        block("0x0",
          Assign(R6, R31),
          goto("0x1")
        ),
        block("0x1",
          MemoryAssign(mem, BinaryExpr(BVADD, R6, bv64(4)), bv64(10), LittleEndian, 64),
          goto("returntarget")
        ),
        block("returntarget",
          ret
        )
      )
    )

    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val results = runAnalyses(program)
    results.mmmResults.pushContext("main")
    assert(results.mmmResults.findStackObject(BigInt(4)).isDefined)
    assert(results.mmmResults.findStackObject(BigInt(4)).get.start == bv64(4))
    assert(results.mmmResults.findStackObject(BigInt(4)).get.regionIdentifier == "stack_1")
  }

  /**
   * Test that the analysis correctly identifies stack pointers within regions
   */
  test("approximate stack region: MMM Stage") {
    var program: Program = prog(
      proc("main",
        block("0x0",
          Assign(R1, MemoryLoad(mem, BinaryExpr(BVADD, R31, bv64(6)), LittleEndian, 64)),
          Assign(R3, MemoryLoad(mem, BinaryExpr(BVADD, R31, bv64(4)), LittleEndian, 64)),
          goto("0x1")
        ),
        block("0x1",
          goto("returntarget")
        ),
        block("returntarget",
          ret
        )
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

    assert(results.mmmResults.findStackObject(BigInt(4)).get.start == bv64(4))
    assert(results.mmmResults.findStackObject(BigInt(5)).get.start == bv64(4))
    assert(results.mmmResults.findStackObject(BigInt(6)).get.start == bv64(6))
    assert(results.mmmResults.findStackObject(BigInt(10)).get.start == bv64(6))
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

  /**
   * Test that the analysis correctly collects shared regions and exposes only the shared ones
   */
  test("collects single function shared regions: MMM Stage") {
    val program: Program = prog(
      proc("main",
        block("0x0",
          Assign(R0, MemoryLoad(mem, BinaryExpr(BVADD, R31, bv64(6)), LittleEndian, 64)),
          Assign(R1, BinaryExpr(BVADD, R31, bv64(10))),
          goto("0x1")
        ),
        block("0x1",
          directCall("p2"), goto("returntarget")
        ),
        block("returntarget",
          ret
        )
      ),
      proc("p2",
        block("l_p2",
          Assign(R3, R0),
          Assign(R2, MemoryLoad(mem, R1, LittleEndian, 64)),
          goto("l_p2_1"),
        ),
        block("l_p2_1",
          ret,
        )
      )
    )

    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val results = runAnalyses(program)
    results.mmmResults.pushContext("main")
    assert(results.mmmResults.findStackObject(BigInt(6)).isDefined)

    assert(results.mmmResults.findStackObject(BigInt(6)).get.start == bv64(6))

    /* ------------------------------------------------------------------------- */

    results.mmmResults.pushContext("p2")
    assert(results.mmmResults.findSharedStackObject(BigInt(6)).nonEmpty)
    assert(results.mmmResults.findSharedStackObject(BigInt(10)).nonEmpty)

    assert(results.mmmResults.findSharedStackObject(BigInt(6)).head.start == bv64(6))
    assert(results.mmmResults.findSharedStackObject(BigInt(10)).head.start == bv64(10))
  }

  /**
   * Test that the analysis correctly collects shared regions from multiple functions
   */
  test("collects multiple functions shared regions: MMM Stage") {
    val program: Program = prog(
      proc("main",
        block("0x0",
          Assign(R0, MemoryLoad(mem, BinaryExpr(BVADD, R31, bv64(6)), LittleEndian, 64)),
          Assign(R1, BinaryExpr(BVADD, R31, bv64(10))),
          goto("0x1")
        ),
        block("0x1",
          directCall("p2"), goto("returntarget")
        ),
        block("returntarget",
          ret
        )
      ),
      proc("foo",
        block("l_foo",
          Assign(R0, MemoryLoad(mem, BinaryExpr(BVADD, R31, bv64(6)), LittleEndian, 64)),
          Assign(R1, BinaryExpr(BVADD, R31, bv64(10))),
          directCall("p2"), goto("l_foo_1")
        ),
        block("l_foo_1",
          ret,
        )
      ),
      proc("p2",
        block("l_p2",
          Assign(R3, R0),
          Assign(R2, MemoryLoad(mem, R1, LittleEndian, 64)),
          goto("l_p2_1"),
        ),
        block("l_p2_1",
          ret,
        )
      )
    )

    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val results = runAnalyses(program)
    results.mmmResults.pushContext("main")
    assert(results.mmmResults.findStackObject(BigInt(6)).isDefined)

    assert(results.mmmResults.findStackObject(BigInt(6)).get.start == bv64(6))

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
