import ir.dsl.*
import ir.{transforms, *}
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import translating.PrettyPrinter.*

@test_util.tags.UnitTest
class SingleAssignmentTest extends AnyFunSuite with CaptureOutput {

  test("tight loop") {
    val x = LocalVar("x", BitVecType(32))

    val tightLoop = prog(
      proc(
        "errorFn_1848",
        block("errorFn_1848_entry", LocalAssign(x, x), goto("loop")),
        block("loop", LocalAssign(x, BinaryExpr(BVADD, x, BitVecLiteral(1, 32))), goto("loop"))
      )
    )

    transforms.OnePassDSA().applyTransform(tightLoop)
    while (transforms.coalesceBlocks(tightLoop)) {}
    assert(transforms.rdDSAProperty(tightLoop.mainProcedure))

    assert(
      pp_stmt(tightLoop.mainProcedure.blocks.find(_.label == "loop").get.statements.head)
        == pp_stmt(
          LocalAssign(
            LocalVar("x", BitVecType(32), 4),
            BinaryExpr(BVADD, LocalVar("x", BitVecType(32), 3), BitVecLiteral(1, 32)),
            Some("phi")
          )
        )
    )
    assert(
      pp_stmt(tightLoop.mainProcedure.blocks.find(_.label == "loop").get.statements.tail.head)
        == pp_stmt(
          SimulAssign(Vector(LocalVar("x", BitVecType(32), 3) -> LocalVar("x", BitVecType(32), 4)), Some("phi"))
        )
    )
  }

  test("tight loop 2") {
    val x = LocalVar("x", BitVecType(32))
    // val x = LocalVar("x0", BitVecType(32))

    val tightLoop = prog(
      proc(
        "errorFn_1848",
        Seq("x" -> BitVecType(32)),
        Seq(),
        block("errorFn_1848_entry", goto("loop")),
        block("loop", LocalAssign(x, BinaryExpr(BVADD, x, BitVecLiteral(1, 32))), goto("loop"))
      )
    )

    transforms.OnePassDSA().applyTransform(tightLoop)
    while (transforms.coalesceBlocks(tightLoop)) {}

    assert(
      pp_stmt(tightLoop.mainProcedure.blocks.find(_.label == "loop").get.statements.head)
        == pp_stmt(
          LocalAssign(
            LocalVar("x", BitVecType(32), 1),
            BinaryExpr(BVADD, LocalVar("x", BitVecType(32), 0), BitVecLiteral(1, 32)),
            Some("phi")
          )
        )
    )
    assert(
      pp_stmt(tightLoop.mainProcedure.blocks.find(_.label == "loop").get.statements.tail.head)
        == pp_stmt(
          SimulAssign(Vector(LocalVar("x", BitVecType(32), 0) -> LocalVar("x", BitVecType(32), 1)), Some("phi"))
        )
    )

    assert(transforms.rdDSAProperty(tightLoop.mainProcedure))
  }

  test("tight loop 3") {
    val x = LocalVar("x", BitVecType(32))
    // val x = LocalVar("x0", BitVecType(32))

    val tightLoop = prog(
      proc(
        "errorFn_1848",
        block("errorFn_1848_entry", goto("loop")),
        block("loop", LocalAssign(x, BinaryExpr(BVADD, x, BitVecLiteral(1, 32))), goto("loop"))
      )
    )

    transforms.OnePassDSA().applyTransform(tightLoop)
    while (transforms.coalesceBlocks(tightLoop)) {}

    assert(
      pp_stmt(tightLoop.mainProcedure.blocks.find(_.label == "loop").get.statements.head)
        == pp_stmt(
          LocalAssign(
            LocalVar("x", BitVecType(32), 2),
            BinaryExpr(BVADD, LocalVar("x", BitVecType(32), 1), BitVecLiteral(1, 32)),
            Some("phi")
          )
        )
    )
    assert(
      pp_stmt(tightLoop.mainProcedure.blocks.find(_.label == "loop").get.statements.tail.head)
        == pp_stmt(
          SimulAssign(Vector(LocalVar("x", BitVecType(32), 1) -> LocalVar("x", BitVecType(32), 2)), Some("phi"))
        )
    )

    assert(transforms.rdDSAProperty(tightLoop.mainProcedure))
  }
}
