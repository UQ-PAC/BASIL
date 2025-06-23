import ir.*
import ir.dsl.*
import ir.transforms.Slicer
import org.scalatest.funsuite.AnyFunSuite
import test_util.{BASILTest, getSubdirectories}
import util.*

/**
 * A collection of different scenario case unit tests for the slicer to run against.
 * Manually sliced to tag IR statements as kept or removed by pass for given criterion.
 */
@test_util.tags.UnitTest
class SlicerTests extends AnyFunSuite, test_util.CaptureOutput, BASILTest {
  Logger.setLevel(LogLevel.OFF)

  protected val SHOULD_REMAIN: Some[String] = Some("Should_Remain")
  protected val SHOULD_DELETE: Some[String] = Some("Should_Delete")

  def createSimpleProc(name: String, statements: Seq[NonCallStatement]): EventuallyProcedure = {
    proc(name, block(name + "_1", statements.:+(goto(name + "_return")): _*), block(name + "_return", ret))
  }

  def prepareProgram(program: Program): Unit = {
    program.procedures.foreach(p =>
      val returns = p.blocks
        .map(_.jump)
        .filter(_.isInstanceOf[Return])
        .toSet
      assert(returns.size == 1)
      p.returnBlock = returns.head.parent
    )
  }

  def statements(program: Program): Iterable[Statement] = {
    program.procedures.flatMap(_.blocks.flatMap(_.statements))
  }

  def remainingStatements(program: Program): Iterable[Statement] = {
    statements(program).filter(_.label.equals(SHOULD_REMAIN))
  }

  def deletableStatements(program: Program): Iterable[Statement] = {
    statements(program).filter(_.label.equals(SHOULD_DELETE))
  }

  test("intraproceduralIRCycle") {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          LocalAssign(LocalVar("Stack_x", bv32), bv32(1), SHOULD_DELETE),
          LocalAssign(LocalVar("Stack_i", bv32), bv32(0)),
          goto("main_2")
        ),
        block("main_2", LocalAssign(LocalVar("load1", bv32), LocalVar("Stack_i", bv32)), goto("main_3", "main_4")),
        block("main_3", Assume(BinaryExpr(BVSGT, LocalVar("load1", bv32), bv32(19)), None, None, true), goto("main_7")),
        block(
          "main_4",
          Assume(BinaryExpr(BVSLE, LocalVar("load1", bv32), bv32(19)), None, None, true),
          LocalAssign(LocalVar("load2", bv32), Register("Global_y", 32), SHOULD_DELETE),
          LocalAssign(LocalVar("load3", bv32), LocalVar("Stack_x", bv32), SHOULD_DELETE),
          LocalAssign(
            LocalVar("R0_7", bv32),
            BinaryExpr(BVMUL, LocalVar("load3", bv32), LocalVar("load2", bv32)),
            SHOULD_DELETE
          ),
          LocalAssign(LocalVar("Stack_x", bv32), LocalVar("R0_7", bv32), SHOULD_DELETE),
          LocalAssign(LocalVar("load4", bv32), Register("Global_y", 32)),
          goto("main_5", "main_6")
        ),
        block("main_5", Assume(BinaryExpr(EQ, LocalVar("load4", bv32), bv32(10)), None, None, true), goto("main_7")),
        block(
          "main_6",
          Assume(UnaryExpr(BoolNOT, BinaryExpr(EQ, LocalVar("load4", bv32), bv32(10))), None, None, true),
          LocalAssign(LocalVar("load5", bv32), Register("Global_y", 32)),
          MemoryAssign(Register("Global_y", 32), BinaryExpr(BVADD, LocalVar("load5", bv32), bv32(5))),
          LocalAssign(LocalVar("load6", bv32), LocalVar("Stack_i", bv32)),
          LocalAssign(LocalVar("Stack_i", bv32), BinaryExpr(BVADD, LocalVar("load6", bv32), bv32(1))),
          goto("main_2")
        ),
        block("main_7", LocalAssign(LocalVar("Stack_x", bv32), bv32(4), SHOULD_DELETE), goto("main_return")),
        block("main_return", ret)
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toBeDeleted = deletableStatements(program)

    Slicer(program, SlicerConfig("main_7", Set("Global_y"))).run()

    assert(deletableStatements(program).isEmpty)
    assert(statements(program).size == totalStatements - toBeDeleted.size)
  }

  /* Tests that analysis correctly terminates with infinite loop bug in code */
  test("intraInfiniteLoop") {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          LocalAssign(LocalVar("Stack_x", bv32), bv32(0)),
          LocalAssign(LocalVar("Stack_y", bv32), bv32(0), SHOULD_DELETE),
          goto("main_2")
        ),
        block("main_2", LocalAssign(LocalVar("load1", bv32), LocalVar("Stack_x", bv32)), goto("main_3", "main_4")),
        block(
          "main_3",
          Assume(BinaryExpr(BVSLE, LocalVar("load1", bv32), bv32(99))),
          LocalAssign(LocalVar("load2", bv32), LocalVar("Stack_y", bv32), SHOULD_DELETE),
          LocalAssign(LocalVar("Stack_y", bv32), BinaryExpr(BVADD, LocalVar("load2", bv32), bv32(1)), SHOULD_DELETE),
          goto("main_2")
        ),
        block("main_4", Assume(BinaryExpr(BVSGT, LocalVar("load1", bv32), bv32(99))), goto("main_return")),
        block("main_return", ret)
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toBeDeleted = deletableStatements(program)

    Slicer(program, SlicerConfig("main_return", Set("Stack_x"))).run()

    assert(deletableStatements(program).isEmpty)
    assert(statements(program).size == totalStatements - toBeDeleted.size)
  }

  /* Tests that assume and assert statements are correctly removed when the slicing criterion is dead (empty) */
  test("deadCriterionStatementRemoval") {
    val program = prog(
      createSimpleProc(
        "main",
        Seq(
          LocalAssign(LocalVar("load1", bv32), bv32(10)),
          Assert(BinaryExpr(EQ, LocalVar("load1", bv32), bv32(10))),
          Assume(BinaryExpr(EQ, LocalVar("Stack_x", bv32), bv32(10))),
          LocalAssign(LocalVar("Stack_x", bv32), bv32(0)),
          MemoryAssign(Register("Global_y", 32), bv32(0)),
          LocalAssign(LocalVar("Stack_x", bv32), bv32(0), SHOULD_REMAIN),
          MemoryAssign(Register("Global_y", 32), bv32(0), SHOULD_REMAIN)
        )
      )
    )

    prepareProgram(program)

    val totalStatements = statements(program).size
    val toRemain = remainingStatements(program)

    Slicer(program, SlicerConfig("main_1", Set("Global_y", "Stack_x"))).run()

    assert(totalStatements > statements(program).size)
    assert(toRemain.equals(statements(program)))
  }

  /* Tests that all assume and assertion statements are preserved if the criterion is alive at that point */
  test("aliveCriterionStatementPreservation") {
    val program = prog(
      createSimpleProc(
        "main",
        Seq(
          LocalAssign(LocalVar("load1", bv32), bv32(10)),
          LocalAssign(LocalVar("load2", bv32), bv32(10)),
          LocalAssign(LocalVar("load1", bv32), bv32(10), SHOULD_REMAIN),
          LocalAssign(LocalVar("load2", bv32), bv32(10), SHOULD_REMAIN),
          Assert(BinaryExpr(EQ, LocalVar("load3", bv32), bv32(10)), label = SHOULD_REMAIN),
          Assume(BinaryExpr(EQ, LocalVar("load1", bv32), LocalVar("load2", bv32)), label = SHOULD_REMAIN)
        )
      )
    )

    prepareProgram(program)

    val totalStatements = statements(program).size
    val toRemain = remainingStatements(program)

    Slicer(program, SlicerConfig("main_1", Set("load1"))).run()

    assert(totalStatements > statements(program).size)
    assert(toRemain.equals(statements(program)))
  }

  /* Tests a direct call that only impacts criterion through local variable assignment */
  test("singleCallLocalImpact") {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          LocalAssign(LocalVar("Stack_n4_0", bv32), bv32(0)),
          LocalAssign(LocalVar("load3", bv32), LocalVar("Stack_n4_0", bv32)),
          directCall(
            Seq("R0_out" -> LocalVar("R0", bv64, 3)),
            "func",
            Seq("R0_in" -> ZeroExtend(32, LocalVar("load3", bv32)))
          ),
          ret
        )
      ),
      proc(
        "func",
        Seq("R0_in" -> bv64),
        Seq("R0_out" -> bv64),
        block(
          "func_2",
          MemoryAssign(Register("Global_x", 32), bv32(2), SHOULD_DELETE),
          LocalAssign(LocalVar("Stack_n20_n16", bv32), Extract(32, 0, LocalVar("R0_in", bv64))),
          LocalAssign(LocalVar("load1", bv32), LocalVar("Stack_n20_n16", bv32), SHOULD_DELETE),
          LocalAssign(LocalVar("Stack_n4_0", bv32), BinaryExpr(BVADD, LocalVar("load1", bv32), bv32(2)), SHOULD_DELETE),
          LocalAssign(LocalVar("load2", bv32), LocalVar("Stack_n20_n16", bv32)),
          ret("R0_out" -> ZeroExtend(32, BinaryExpr(BVADD, LocalVar("load2", bv32), bv32(1))))
        )
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toBeDeleted = deletableStatements(program)

    Slicer(program, SlicerConfig("main_1", Set("R0_3"))).run()

    assert(deletableStatements(program).isEmpty)
    assert(statements(program).size == totalStatements - toBeDeleted.size)
  }

  /* Tests a direct call that only impacts the criterion through global variable modification in called procedure */
  test("singleCallGlobalImpact") {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          MemoryAssign(Register("Global_y", 32), bv32(4), SHOULD_REMAIN),
          directCall("func", SHOULD_REMAIN),
          ret
        )
      ),
      proc(
        "func",
        block(
          "func_2",
          LocalAssign(LocalVar("load0", bv32), bv32(0)),
          LocalAssign(LocalVar("load1", bv32), Register("Global_y", 32), SHOULD_REMAIN),
          MemoryAssign(Register("Global_y", 32), BinaryExpr(BVADD, LocalVar("load1", bv32), bv32(10)), SHOULD_REMAIN),
          ret
        )
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toRemain = remainingStatements(program)

    Slicer(program, SlicerConfig("main_1", Set("Global_y"))).run()

    assert(totalStatements > statements(program).size)
    assert(toRemain.equals(statements(program)))
  }

  /* - Tests removal of call statements when they have no impact on criterion */
  test("singleCallNoImpact") {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          LocalAssign(LocalVar("load0", bv32), bv32(0), SHOULD_REMAIN),
          LocalAssign(LocalVar("load1", bv32), bv32(0)),
          directCall(Seq("R0_out" -> LocalVar("load2", bv32)), "func", Seq("R0_in" -> LocalVar("load1", bv32))),
          goto("main_return")
        ),
        block("main_return", ret)
      ),
      proc(
        "func",
        Seq("R0_in" -> bv32),
        Seq("R0_out" -> bv32),
        block("func_1", LocalAssign(LocalVar("load3", bv32), LocalVar("R0_in", bv32)), goto("func_return")),
        block("func_return", ret("R0_out" -> LocalVar("load3", bv32)))
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toRemain = remainingStatements(program)

    Slicer(program, SlicerConfig("main_1", Set("load0"))).run()

    assert(totalStatements > statements(program).size)
    assert(toRemain.equals(statements(program)))
  }

  /* Tests a direct call that only impacts the criterion through modification that introduces new criterion in called procedure */
  test("singleCallGlobalToLocalImpact") {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          LocalAssign(LocalVar("load0", bv32), bv32(4), SHOULD_REMAIN),
          directCall(Seq(), "func", Seq("R0_in" -> LocalVar("load0", bv32)), SHOULD_REMAIN),
          ret
        )
      ),
      proc(
        "func",
        Seq("R0_in" -> bv32),
        Seq(),
        block(
          "func_2",
          LocalAssign(LocalVar("load0", bv32), LocalVar("R0_in", bv32), SHOULD_REMAIN),
          LocalAssign(LocalVar("load1", bv32), bv32(0)),
          MemoryAssign(Register("Global_y", 32), LocalVar("load0", bv32), SHOULD_REMAIN),
          ret
        )
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toRemain = remainingStatements(program)

    Slicer(program, SlicerConfig("main_1", Set("Global_y"))).run()

    assert(totalStatements > statements(program).size)
    assert(toRemain.equals(statements(program)))
  }

  /* Tests cyclical procedure call termination */
  test("callCycle") {
    val program = prog(
      proc(
        "main",
        block("main_1", MemoryAssign(Register("Global_y", 32), bv32(0)), directCall("read"), goto("main_return")),
        block("main_return", ret)
      ),
      proc(
        "read",
        block(
          "read_1",
          LocalAssign(LocalVar("load1", bv32), Register("Global_y", 32)),
          MemoryAssign(Register("Global_y", 32), BinaryExpr(BVADD, LocalVar("load1", bv32), bv32(1))),
          directCall("write"),
          goto("read_return")
        ),
        block("read_return", ret)
      ),
      proc(
        "write",
        block(
          "write_1",
          LocalAssign(LocalVar("load1", bv32), bv32(100), SHOULD_DELETE),
          LocalAssign(LocalVar("load1", bv32), Register("Global_y", 32)),
          MemoryAssign(Register("Global_y", 32), BinaryExpr(BVSUB, LocalVar("load1", bv32), bv32(1))),
          goto("write_return")
        ),
        block("write_2", Assume(BinaryExpr(BVSGT, Register("Global_y", 32), bv32(10))), goto("write_return")),
        block(
          "write_3",
          Assume(BinaryExpr(BVSLE, Register("Global_y", 32), bv32(10))),
          directCall("read"),
          goto("write_return")
        ),
        block("write_return", ret)
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toBeDeleted = deletableStatements(program)

    Slicer(program, SlicerConfig("main_1", Set("Global_y"))).run()

    assert(deletableStatements(program).isEmpty)
    assert(statements(program).size == totalStatements - toBeDeleted.size)

  }

  /* Tests that local criterion is correctly handled across calls, even if both procedures have local variables with the same names */
  test("interprocedureLocalCriterionPreservation") {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          LocalAssign(LocalVar("a", bv32), bv32(0), SHOULD_DELETE),
          LocalAssign(LocalVar("b", bv32), bv32(0)),
          LocalAssign(LocalVar("c", bv32), bv32(0)),
          MemoryAssign(Register("Global_x", 32), bv32(0)),
          directCall(Seq("R0_out" -> LocalVar("a", bv32)), "func", Seq("R0_in" -> LocalVar("b", bv32))),
          ret
        )
      ),
      proc(
        "func",
        Seq("R0_in" -> bv32),
        Seq("R0_out" -> bv32),
        block(
          "func_1",
          LocalAssign(LocalVar("c", bv32), bv32(0), SHOULD_DELETE),
          LocalAssign(LocalVar("load1", bv32), LocalVar("R0_in", bv32)),
          goto("func_return")
        ),
        block("func_return", ret("R0_out" -> LocalVar("load1", bv32)))
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toBeDeleted = deletableStatements(program)

    Slicer(program, SlicerConfig("main_1", Set("a", "Global_x", "c"))).run()

    assert(deletableStatements(program).isEmpty)
    assert(statements(program).size == totalStatements - toBeDeleted.size)
  }

  /* Tests that global criterion is correctly handled across calls */
  test("interprocedureGlobalCriterionReduction") {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          LocalAssign(LocalVar("a", bv32), bv32(0), SHOULD_DELETE),
          LocalAssign(LocalVar("b", bv32), bv32(0)),
          MemoryAssign(Register("Global_x", 32), bv32(0), SHOULD_DELETE),
          directCall(Seq("R0_out" -> LocalVar("a", bv32)), "func", Seq("R0_in" -> LocalVar("b", bv32))),
          ret
        )
      ),
      proc(
        "func",
        Seq("R0_in" -> bv32),
        Seq("R0_out" -> bv32),
        block(
          "func_1",
          MemoryAssign(Register("Global_x", 32), bv32(0)),
          LocalAssign(LocalVar("load1", bv32), LocalVar("R0_in", bv32)),
          goto("func_return")
        ),
        block("func_return", ret("R0_out" -> LocalVar("load1", bv32)))
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toBeDeleted = deletableStatements(program)

    Slicer(program, SlicerConfig("main_1", Set("a", "Global_x"))).run()

    assert(deletableStatements(program).isEmpty)
    assert(statements(program).size == totalStatements - toBeDeleted.size)
  }

  private def createMultiCallSingleImpact = {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          MemoryAssign(Register("Global_x", 32), bv32(0)),
          MemoryAssign(Register("Global_y", 32), bv32(0)),
          LocalAssign(LocalVar("load0", bv32), Register("Global_x", 32)),
          directCall(
            Seq("R0_out" -> LocalVar("R0", bv64)),
            "f",
            Seq("R0_in" -> ZeroExtend(32, LocalVar("load0", bv32)))
          ),
          goto("main_2")
        ),
        block(
          "main_2",
          MemoryAssign(Register("Global_y", 32), bv32(0), SHOULD_REMAIN),
          directCall(Seq("R0_out" -> LocalVar("R0", bv64)), "f", Seq("R0_in" -> bv64(5)), SHOULD_REMAIN),
          goto("main_return")
        ),
        block("main_return", ret)
      ),
      proc(
        "f",
        Seq("R0_in" -> bv64),
        Seq("R0_out" -> bv64),
        block(
          "f_1",
          LocalAssign(LocalVar("Stack_n", bv32), Extract(32, 0, LocalVar("R0_in", bv64))),
          LocalAssign(LocalVar("load0", bv32), Register("Global_y", 32), SHOULD_REMAIN),
          MemoryAssign(Register("Global_y", 32), BinaryExpr(BVADD, LocalVar("load0", bv32), bv32(10)), SHOULD_REMAIN),
          LocalAssign(LocalVar("load1", bv32), Register("Global_x", 32)),
          LocalAssign(LocalVar("load2", bv32), LocalVar("Stack_n", bv32)),
          LocalAssign(
            LocalVar("R0", bv64),
            ZeroExtend(32, BinaryExpr(BVADD, LocalVar("load1", bv32), LocalVar("load2", bv32)))
          ),
          goto("f_return")
        ),
        block("f_return", ret("R0_out" -> LocalVar("R0", bv64)))
      )
    )
    prepareProgram(program)
    program
  }

  private def createMultiCallMultiImpact = {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          MemoryAssign(Register("Global_x", 32), bv32(0)),
          MemoryAssign(Register("Global_y", 32), bv32(0), SHOULD_DELETE),
          LocalAssign(LocalVar("load0", bv32), Register("Global_x", 32)),
          directCall(
            Seq("R0_out" -> LocalVar("R0", bv64)),
            "f",
            Seq("R0_in" -> ZeroExtend(32, LocalVar("load0", bv32)))
          ),
          goto("main_2")
        ),
        block(
          "main_2",
          MemoryAssign(Register("Global_y", 32), Extract(32, 0, LocalVar("R0", bv64))),
          directCall(Seq("R0_out" -> LocalVar("R0", bv64)), "f", Seq("R0_in" -> bv64(5))),
          goto("main_return")
        ),
        block("main_return", ret)
      ),
      proc(
        "f",
        Seq("R0_in" -> bv64),
        Seq("R0_out" -> bv64),
        block(
          "f_1",
          LocalAssign(LocalVar("Stack_n", bv32), Extract(32, 0, LocalVar("R0_in", bv64))),
          LocalAssign(LocalVar("load0", bv32), Register("Global_y", 32)),
          MemoryAssign(Register("Global_y", 32), BinaryExpr(BVADD, LocalVar("load0", bv32), bv32(10))),
          LocalAssign(LocalVar("load1", bv32), Register("Global_x", 32)),
          LocalAssign(LocalVar("load2", bv32), LocalVar("Stack_n", bv32)),
          LocalAssign(
            LocalVar("R0", bv64),
            ZeroExtend(32, BinaryExpr(BVADD, LocalVar("load1", bv32), LocalVar("load2", bv32)))
          ),
          goto("f_return")
        ),
        block("f_return", ret("R0_out" -> LocalVar("R0", bv64)))
      )
    )
    prepareProgram(program)
    program
  }

  private def createMultiCallPartialReduction = {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          directCall(
            Seq("R0_out" -> LocalVar("load0", bv64), "R1_out" -> LocalVar("load1", bv32)),
            "f",
            Seq("R0_in" -> bv64(10), "R1_in" -> bv32(0))
          ),
          goto("main_return")
        ),
        block("main_return", ret)
      ),
      proc(
        "f",
        Seq("R0_in" -> bv64, "R1_in" -> bv32),
        Seq("R0_out" -> bv64, "R1_out" -> bv32),
        block(
          "f_1",
          LocalAssign(LocalVar("R0", bv64), ZeroExtend(32, LocalVar("R1_in", bv32))),
          LocalAssign(LocalVar("R1", bv32), bv32(50)),
          goto("f_return")
        ),
        block("f_return", ret("R0_out" -> LocalVar("R0", bv64), "R1_out" -> LocalVar("R1", bv32)))
      )
    )
    prepareProgram(program)
    program
  }

  /* Tests a procedure with multiple calls statements where only one of the calls has an impact on the criterion */
  test("multiCallSingleImpact") {
    val program = createMultiCallSingleImpact

    val totalStatements = statements(program).size
    val toRemain = remainingStatements(program)

    Slicer(program, SlicerConfig("main_return", Set("Global_y"))).run()

    assert(totalStatements > statements(program).size)
    assert(toRemain.equals(statements(program)))
  }

  /* Tests a procedure with multiple calls statements where all calls have an impact on the criterion */
  test("multiCallMultiImpact") {
    val program = createMultiCallMultiImpact
    val totalStatements = statements(program).size
    val toBeDeleted = deletableStatements(program)

    Slicer(program, SlicerConfig("main_return", Set("Global_y"))).run()

    assert(deletableStatements(program).isEmpty)
    assert(statements(program).size == totalStatements - toBeDeleted.size)
  }

  /* Tests procedure parameter reduction when none of the parameters have an impact on the criterion */
  test("fullParameterReduction") {
    val program = createMultiCallSingleImpact
    val f = program.nameToProcedure("f")

    assert(f.formalInParam.size == 1)
    assert(f.formalOutParam.size == 1)

    Slicer(program, SlicerConfig("main_return", Set("Global_y"))).run()

    assert(f.formalInParam.isEmpty)
    assert(f.formalOutParam.isEmpty)
  }

  /* Tests parameter reduction when all the parameters have an impact on the criterion */
  test("fullParameterPreservation") {
    val program = createMultiCallMultiImpact
    val f = program.nameToProcedure("f")

    val fInParam = f.formalInParam.toSet
    val fOutParam = f.formalOutParam.toSet

    Slicer(program, SlicerConfig("main_return", Set("Global_y"))).run()

    assert(f.formalInParam.equals(fInParam))
    assert(f.formalOutParam.equals(fOutParam))
  }

  /* Tests parameter reduction when some of the in and out parameters have an impact on the criterion */
  test("partialInOutParameterReduction") {
    val program = createMultiCallPartialReduction
    val f = program.nameToProcedure("f")

    Slicer(program, SlicerConfig("main_return", Set("load0"))).run()

    assert(f.formalInParam.size == 1)
    assert(f.formalInParam.map(v => v.name).contains("R1_in"))

    assert(f.formalOutParam.size == 1)
    assert(f.formalOutParam.map(v => v.name).contains("R0_out"))
  }

  /* Tests parameter reduction when some of the in parameters and all the out parameters have an impact on the criterion */
  test("partialInOnlyParameterReduction") {
    val program = createMultiCallPartialReduction
    val f = program.nameToProcedure("f")

    val fOutParam = f.formalOutParam.toSet

    Slicer(program, SlicerConfig("main_return", Set("load0", "load1"))).run()

    assert(f.formalInParam.size == 1)
    assert(f.formalInParam.map(v => v.name).contains("R1_in"))

    assert(f.formalOutParam.equals(fOutParam))
  }

  /* Tests slicing a method that is not main to ensure it is preserved despite being 'unreachable' */
  test("sliceNonMain") {
    val program = prog(
      proc(
        "main",
        block(
          "main_1",
          LocalAssign(LocalVar("load0", bv32), bv32(0)),
          LocalAssign(LocalVar("load1", bv32), bv32(0)),
          directCall(
            Seq("R0_out" -> LocalVar("R0", bv32)),
            "write",
            Seq("R0_in" -> LocalVar("load0", bv32), "R1_in" -> LocalVar("load1", bv32))
          ),
          goto("main_return")
        ),
        block("main_return", ret)
      ),
      proc(
        "write",
        Seq("R0_in" -> bv32, "R1_in" -> bv32),
        Seq("R0_out" -> bv32),
        block(
          "write_1",
          directCall(
            Seq("R0_out" -> LocalVar("R0", bv32)),
            "read",
            Seq("R0_in" -> BinaryExpr(BVADD, LocalVar("R0_in", bv32), LocalVar("R1_in", bv32)))
          ),
          goto("write_return")
        ),
        block("write_return", ret("R0_out" -> LocalVar("R0", bv32)))
      ),
      proc(
        "read",
        Seq("R0_in" -> bv32),
        Seq("R0_out" -> bv32),
        block(
          "read_1",
          LocalAssign(LocalVar("load2", bv32), LocalVar("R0_in", bv32), SHOULD_REMAIN),
          LocalAssign(LocalVar("load3", bv32), BinaryExpr(BVADD, LocalVar("load2", bv32), bv32(10)), SHOULD_REMAIN),
          goto("read_return")
        ),
        block("read_return", ret("R0_out" -> LocalVar("load3", bv32)))
      )
    )
    prepareProgram(program)

    val totalStatements = statements(program).size
    val toRemain = remainingStatements(program)

    Slicer(program, SlicerConfig("read_return", Set("R0_out"))).run()

    assert(totalStatements > statements(program).size)
    assert(toRemain.equals(statements(program)))
  }
}

/**
 * Tests that run the slicer against correct system tests to ensure that the resultant Boogie file will still verify.
 */
@test_util.tags.DisabledTest
class SlicerSystemTests extends AnyFunSuite, test_util.CaptureOutput, BASILTest {
  Logger.setLevel(LogLevel.OFF)

  private val correctPath = s"${BASILTest.rootDirectory}/src/test/correct/"

  def runExample(name: String, variant: String, slicerConfig: Option[SlicerConfig] = None): (BASILResult, Boolean) = {
    val inputFile = s"$correctPath/$name/$variant/$name.gts"
    val relfFile = s"$correctPath/$name/$variant/$name.relf"
    val staticAnalysisConfig = Some(StaticAnalysisConfig())
    val outputFile = s"$correctPath/$name/$variant/${name}_slicer.bpl"
    val result = runBASIL(
      inputFile,
      relfFile,
      None,
      outputFile,
      staticAnalysisConfig,
      simplify = true,
      dsa = Some(DSAConfig.Checks),
      slicerConfig = slicerConfig
    )
    val boogieOutput = runBoogie(s"$correctPath/$name", outputFile, Seq())
    val (_, verified, _) = checkVerify(boogieOutput, true)
    (result, verified)
  }

  def testSlicerVerification(name: String, variant: String): Unit = {
    val (initialResult, initialVerified) = runExample(name, variant)
    assert(initialVerified, s"Unsliced program $name does not verify")

    val slicingConfig: SlicerConfig = initialResult.ir.program.mainProcedure.returnBlock match {
      case Some(block) => {
        SlicerConfig(
          block.label,
          block.jump match {
            case r: Return => r.outParams.keys.toSet.map(_.name)
            case _ => ???
          }
        )
      }
      case None => {
        assert(false, s"Program $name does not have main return")
        ???
      }
    }

    val (slicedResult, slicedVerified) = runExample(name, variant, Some(slicingConfig))
    assert(slicedVerified, s"Sliced program $name does not verify")
  }

  def runTests(name: String): Unit = {
    getSubdirectories(s"$correctPath/$name").foreach { v => testSlicerVerification(name, v) }
  }

  test("arrays_simple") {
    runTests("arrays_simple")
  }

  test("basic_arrays_read") {
    runTests("basic_arrays_read")
  }

  test("basic_arrays_write") {
    runTests("basic_arrays_write")
  }

  test("basic_lock_unlock") {
    runTests("basic_lock_unlock")
  }

  test("basic_loop_assign") {
    runTests("basic_loop_assign")
  }

  test("basic_function_call_caller") {
    runTests("basic_function_call_caller")
  }

  test("cjump") {
    runTests("cjump")
  }

  test("function") {
    runTests("function")
  }

  test("function1") {
    runTests("function1")
  }

  test("functions_with_params") {
    runTests("functions_with_params")
  }

  test("jumptable2") {
    runTests("jumptable2")
  }

  test("secret_write") {
    runTests("secret_write")
  }
}
