import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import util.SimplifyMode

@test_util.tags.UnitTest
class ConditionLiftingRegressionTest extends AnyFunSuite with test_util.CaptureOutput {

  /**
   * Regression test condition lifting using for a difficult example which requires propagating conditions across multiple branches
   *
   * Future simplifications could possibly break this if the branches are simplified further (e.g. more extends/extracts removed)
   */

  val expected =
    Map(
      "main_4196096_7" ->
        BinaryExpr(BVSLE, Extract(32, 0, LocalVar("R0_in", BitVecType(64), 0)), BitVecLiteral(BigInt("1"), 32)),
      "main_4196096_9" ->
        BinaryExpr(BVSGT, Extract(32, 0, LocalVar("R0_in", BitVecType(64), 0)), BitVecLiteral(BigInt("1"), 32)),
      "Sqrt_4196496_15" ->
        BinaryExpr(EQ, LocalVar("R1", BitVecType(64), 6), BitVecLiteral(BigInt("0"), 64)),
      "Sqrt_4196496_16" ->
        UnaryExpr(BoolNOT, BinaryExpr(EQ, LocalVar("R1", BitVecType(64), 6), BitVecLiteral(BigInt("0"), 64))),
      "Sqrt_4196496_12" ->
        BinaryExpr(
          BVULT,
          ZeroExtend(1, Extract(64, 1, LocalVar("R1", BitVecType(64), 12))),
          LocalVar("R1", BitVecType(64), 11)
        ),
      "Sqrt_4196496_13" ->
        BinaryExpr(
          BVUGE,
          ZeroExtend(1, Extract(64, 1, LocalVar("R1", BitVecType(64), 12))),
          LocalVar("R1", BitVecType(64), 11)
        ),
      "Sqrt_4196496_9" ->
        BinaryExpr(
          BVUGT,
          ZeroExtend(1, Extract(64, 1, LocalVar("R1", BitVecType(64), 12))),
          LocalVar("R1", BitVecType(64), 11)
        ),
      "Sqrt_4196496_10" ->
        BinaryExpr(
          BVULE,
          ZeroExtend(1, Extract(64, 1, LocalVar("R1", BitVecType(64), 12))),
          LocalVar("R1", BitVecType(64), 11)
        ),
      "Sqrt_4196496_1" ->
        BinaryExpr(BVUGT, LocalVar("R3", BitVecType(64), 20), BitVecLiteral(BigInt("1"), 64)),
      "Sqrt_4196496_7" ->
        BinaryExpr(BVULE, LocalVar("R3", BitVecType(64), 20), BitVecLiteral(BigInt("1"), 64)),
      "Sqrt_4196496_5" ->
        BinaryExpr(
          BVULE,
          BinaryExpr(
            BVMUL,
            ZeroExtend(1, Extract(64, 1, LocalVar("R1", BitVecType(64), 12))),
            ZeroExtend(1, Extract(64, 1, LocalVar("R1", BitVecType(64), 12)))
          ),
          LocalVar("R0_in", BitVecType(64), 0)
        ),
      "Sqrt_4196496_6" ->
        BinaryExpr(
          BVUGT,
          BinaryExpr(
            BVMUL,
            ZeroExtend(1, Extract(64, 1, LocalVar("R1", BitVecType(64), 12))),
            ZeroExtend(1, Extract(64, 1, LocalVar("R1", BitVecType(64), 12)))
          ),
          LocalVar("R0_in", BitVecType(64), 0)
        )
    )

  val testProgram = prog(
    proc(
      "main_4196096",
      block(
        "main_4196096__0__NfWWPq4PTwyv0VapVhBGag",
        LocalAssign(
          LocalVar("Cse0__5_4_0", BitVecType(64), 0),
          BinaryExpr(BVADD, Register("R31", 64), BitVecLiteral(BigInt("18446744073709551584"), 64)),
          Some("4196096_0")
        ),
        MemoryStore(
          StackMemory("stack", 64, 8),
          LocalVar("Cse0__5_4_0", BitVecType(64), 0),
          Register("R29", 64),
          Endian.LittleEndian,
          64,
          Some("4196096_1")
        ),
        MemoryStore(
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, LocalVar("Cse0__5_4_0", BitVecType(64), 0), BitVecLiteral(BigInt("8"), 64)),
          Register("R30", 64),
          Endian.LittleEndian,
          64,
          Some("4196096_2")
        ),
        LocalAssign(Register("R31", 64), LocalVar("Cse0__5_4_0", BitVecType(64), 0), Some("4196096_3")),
        LocalAssign(Register("R29", 64), Register("R31", 64), Some("4196100_0")),
        LocalAssign(
          LocalVar("Cse0__5_4_2", BitVecType(64), 0),
          BinaryExpr(BVADD, Register("R31", 64), BitVecLiteral(BigInt("16"), 64)),
          Some("4196104_0")
        ),
        MemoryStore(
          StackMemory("stack", 64, 8),
          LocalVar("Cse0__5_4_2", BitVecType(64), 0),
          Register("R19", 64),
          Endian.LittleEndian,
          64,
          Some("4196104_1")
        ),
        MemoryStore(
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, LocalVar("Cse0__5_4_2", BitVecType(64), 0), BitVecLiteral(BigInt("8"), 64)),
          Register("R20", 64),
          Endian.LittleEndian,
          64,
          Some("4196104_2")
        ),
        LocalAssign(Register("R20", 64), BitVecLiteral(BigInt("5"), 64), Some("4196108_0")),
        LocalAssign(
          LocalVar("Cse0__5_4_4", BitVecType(32), 0),
          BinaryExpr(BVADD, Extract(32, 0, Register("R0", 64)), BitVecLiteral(BigInt("4294967295"), 32)),
          Some("4196112_0")
        ),
        LocalAssign(
          Register("VF", 1),
          UnaryExpr(
            BVNOT,
            BinaryExpr(
              BVCOMP,
              SignExtend(32, LocalVar("Cse0__5_4_4", BitVecType(32), 0)),
              BinaryExpr(
                BVADD,
                SignExtend(32, Extract(32, 0, Register("R0", 64))),
                BitVecLiteral(BigInt("18446744073709551615"), 64)
              )
            )
          ),
          Some("4196112_1")
        ),
        LocalAssign(
          Register("CF", 1),
          UnaryExpr(
            BVNOT,
            BinaryExpr(
              BVCOMP,
              ZeroExtend(32, LocalVar("Cse0__5_4_4", BitVecType(32), 0)),
              BinaryExpr(
                BVADD,
                ZeroExtend(32, Extract(32, 0, Register("R0", 64))),
                BitVecLiteral(BigInt("4294967295"), 64)
              )
            )
          ),
          Some("4196112_2")
        ),
        LocalAssign(
          Register("ZF", 1),
          BinaryExpr(BVCOMP, LocalVar("Cse0__5_4_4", BitVecType(32), 0), BitVecLiteral(BigInt("0"), 32)),
          Some("4196112_3")
        ),
        LocalAssign(Register("NF", 1), Extract(32, 31, LocalVar("Cse0__5_4_4", BitVecType(32), 0)), Some("4196112_4")),
        goto(
          "main_4196096__0__NfWWPq4PTwyv0VapVhBGag_goto_main_4196096__3__tXIOhSQ~R1WA_9VL5~6KQQ",
          "main_4196096__0__NfWWPq4PTwyv0VapVhBGag_goto_main_4196096__1__cdQ2GS2~QhaOa7OUvPWMRQ"
        )
      ),
      block("main_4196096__1__cdQ2GS2~QhaOa7OUvPWMRQ", goto("main_4196096__2__rIlbG4jGSTydaFqMhxCKWw")),
      block(
        "main_4196096__2__rIlbG4jGSTydaFqMhxCKWw",
        LocalAssign(Register("R20", 64), Register("R0", 64), Some("4196136_0")),
        goto("main_4196096__3__tXIOhSQ~R1WA_9VL5~6KQQ")
      ),
      block(
        "main_4196096__3__tXIOhSQ~R1WA_9VL5~6KQQ",
        LocalAssign(Register("R1", 64), Register("R20", 64), Some("4196140_0")),
        LocalAssign(Register("R19", 64), BitVecLiteral(BigInt("4194304"), 64), Some("4196144_0")),
        LocalAssign(
          Register("R19", 64),
          BinaryExpr(BVADD, Register("R19", 64), BitVecLiteral(BigInt("2304"), 64)),
          Some("4196148_0")
        ),
        LocalAssign(Register("R0", 64), Register("R19", 64), Some("4196152_0")),
        LocalAssign(Register("R30", 64), BitVecLiteral(BigInt("4196116"), 64), Some("4196156_0")),
        goto("FUN_4006f0_4196080__0__GghTYm6bT12tNFmqu0nIjA_3")
      ),
      block(
        "main_4196096__4__oqiqdATZTc6MDOYJqL9Aew",
        LocalAssign(Register("R0", 64), Register("R20", 64), Some("4196160_0")),
        LocalAssign(Register("R30", 64), BitVecLiteral(BigInt("4196104"), 64), Some("4196164_0")),
        directCall("Sqrt_4196496"),
        goto("main_4196096__5__IkNYmV06TxC75h8A4NM3wA")
      ),
      block(
        "main_4196096__5__IkNYmV06TxC75h8A4NM3wA",
        LocalAssign(Register("R1", 64), Register("R0", 64), Some("4196168_0")),
        LocalAssign(Register("R0", 64), Register("R19", 64), Some("4196172_0")),
        LocalAssign(Register("R30", 64), BitVecLiteral(BigInt("4196108"), 64), Some("4196176_0")),
        goto("FUN_4006f0_4196080__0__GghTYm6bT12tNFmqu0nIjA_1")
      ),
      block(
        "main_4196096__6__D9t2gNJrSmyMH3GAVRe3IQ",
        LocalAssign(
          LocalVar("Cse0__5_2_0", BitVecType(64), 0),
          BinaryExpr(BVADD, Register("R31", 64), BitVecLiteral(BigInt("16"), 64)),
          Some("4196180_0")
        ),
        MemoryLoad(
          LocalVar("load6", BitVecType(64), 0),
          StackMemory("stack", 64, 8),
          LocalVar("Cse0__5_2_0", BitVecType(64), 0),
          Endian.LittleEndian,
          64,
          Some("4196180_1$0")
        ),
        LocalAssign(
          LocalVar("Exp16__5_2_0", BitVecType(64), 0),
          LocalVar("load6", BitVecType(64), 0),
          Some("4196180_1$1")
        ),
        MemoryLoad(
          LocalVar("load7", BitVecType(64), 0),
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, LocalVar("Cse0__5_2_0", BitVecType(64), 0), BitVecLiteral(BigInt("8"), 64)),
          Endian.LittleEndian,
          64,
          Some("4196180_2$0")
        ),
        LocalAssign(
          LocalVar("Exp18__5_2_0", BitVecType(64), 0),
          LocalVar("load7", BitVecType(64), 0),
          Some("4196180_2$1")
        ),
        LocalAssign(Register("R19", 64), LocalVar("Exp16__5_2_0", BitVecType(64), 0), Some("4196180_3")),
        LocalAssign(Register("R20", 64), LocalVar("Exp18__5_2_0", BitVecType(64), 0), Some("4196180_4")),
        LocalAssign(Register("R0", 64), BitVecLiteral(BigInt("0"), 64), Some("4196184_0")),
        MemoryLoad(
          LocalVar("load8", BitVecType(64), 0),
          StackMemory("stack", 64, 8),
          Register("R31", 64),
          Endian.LittleEndian,
          64,
          Some("4196188_0$0")
        ),
        LocalAssign(
          LocalVar("Exp16__5_2_2", BitVecType(64), 0),
          LocalVar("load8", BitVecType(64), 0),
          Some("4196188_0$1")
        ),
        MemoryLoad(
          LocalVar("load9", BitVecType(64), 0),
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, Register("R31", 64), BitVecLiteral(BigInt("8"), 64)),
          Endian.LittleEndian,
          64,
          Some("4196188_1$0")
        ),
        LocalAssign(
          LocalVar("Exp18__5_2_2", BitVecType(64), 0),
          LocalVar("load9", BitVecType(64), 0),
          Some("4196188_1$1")
        ),
        LocalAssign(Register("R29", 64), LocalVar("Exp16__5_2_2", BitVecType(64), 0), Some("4196188_2")),
        LocalAssign(Register("R30", 64), LocalVar("Exp18__5_2_2", BitVecType(64), 0), Some("4196188_3")),
        LocalAssign(
          Register("R31", 64),
          BinaryExpr(BVADD, Register("R31", 64), BitVecLiteral(BigInt("32"), 64)),
          Some("4196188_4")
        ),
        goto("main_4196096_basil_return")
      ),
      block(
        "main_4196096__0__NfWWPq4PTwyv0VapVhBGag_goto_main_4196096__3__tXIOhSQ~R1WA_9VL5~6KQQ",
        Assume(
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BoolAND,
              BinaryExpr(EQ, Register("NF", 1), Register("VF", 1)),
              BinaryExpr(EQ, Register("ZF", 1), BitVecLiteral(BigInt("0"), 1))
            )
          ),
          None,
          None,
          true
        ),
        goto("main_4196096__3__tXIOhSQ~R1WA_9VL5~6KQQ")
      ),
      block(
        "main_4196096__0__NfWWPq4PTwyv0VapVhBGag_goto_main_4196096__1__cdQ2GS2~QhaOa7OUvPWMRQ",
        Assume(
          UnaryExpr(
            BoolNOT,
            UnaryExpr(
              BoolNOT,
              BinaryExpr(
                BoolAND,
                BinaryExpr(EQ, Register("NF", 1), Register("VF", 1)),
                BinaryExpr(EQ, Register("ZF", 1), BitVecLiteral(BigInt("0"), 1))
              )
            )
          ),
          None,
          None,
          true
        ),
        MemoryLoad(
          LocalVar("load10", BitVecType(64), 0),
          SharedMemory("mem", 64, 8),
          BinaryExpr(BVADD, Register("R1", 64), BitVecLiteral(BigInt("8"), 64)),
          Endian.LittleEndian,
          64,
          Some("4196120_0$0")
        ),
        LocalAssign(
          LocalVar("Exp14__5_6_0", BitVecType(64), 0),
          LocalVar("load10", BitVecType(64), 0),
          Some("4196120_0$1")
        ),
        LocalAssign(Register("R0", 64), LocalVar("Exp14__5_6_0", BitVecType(64), 0), Some("4196120_1")),
        LocalAssign(Register("R2", 64), BitVecLiteral(BigInt("10"), 64), Some("4196124_0")),
        LocalAssign(Register("R1", 64), BitVecLiteral(BigInt("0"), 64), Some("4196128_0")),
        LocalAssign(Register("R30", 64), BitVecLiteral(BigInt("4196112"), 64), Some("4196132_0")),
        goto("FUN_4006b0_4196016__0__Djx7L34DQzuSXaBFEj_bpQ_7")
      ),
      block("main_4196096_basil_return", ret),
      block("main_4196096__5__IkNYmV06TxC75h8A4NM3wA_inlineret", goto("main_4196096__6__D9t2gNJrSmyMH3GAVRe3IQ")),
      block(
        "FUN_4006f0_4196080__0__GghTYm6bT12tNFmqu0nIjA_1",
        LocalAssign(Register("R16", 64), BitVecLiteral(BigInt("4325376"), 64), Some("4196080_0")),
        MemoryLoad(
          LocalVar("load11", BitVecType(64), 0),
          SharedMemory("mem", 64, 8),
          BinaryExpr(BVADD, Register("R16", 64), BitVecLiteral(BigInt("32"), 64)),
          Endian.LittleEndian,
          64,
          Some("4196084_0$0")
        ),
        LocalAssign(
          LocalVar("Exp14__5_0_1", BitVecType(64), 0),
          LocalVar("load11", BitVecType(64), 0),
          Some("4196084_0$1")
        ),
        LocalAssign(Register("R17", 64), LocalVar("Exp14__5_0_1", BitVecType(64), 0), Some("4196084_1")),
        LocalAssign(
          Register("R16", 64),
          BinaryExpr(BVADD, Register("R16", 64), BitVecLiteral(BigInt("32"), 64)),
          Some("4196088_0")
        ),
        directCall("printf"),
        goto("FUN_4006f0_4196080_basil_return_2")
      ),
      block("FUN_4006f0_4196080_basil_return_2", goto("main_4196096__5__IkNYmV06TxC75h8A4NM3wA_inlineret")),
      block("main_4196096__3__tXIOhSQ~R1WA_9VL5~6KQQ_inlineret", goto("main_4196096__4__oqiqdATZTc6MDOYJqL9Aew")),
      block(
        "FUN_4006f0_4196080__0__GghTYm6bT12tNFmqu0nIjA_3",
        LocalAssign(Register("R16", 64), BitVecLiteral(BigInt("4325376"), 64), Some("4196080_0")),
        MemoryLoad(
          LocalVar("load11", BitVecType(64), 0),
          SharedMemory("mem", 64, 8),
          BinaryExpr(BVADD, Register("R16", 64), BitVecLiteral(BigInt("32"), 64)),
          Endian.LittleEndian,
          64,
          Some("4196084_0$0")
        ),
        LocalAssign(
          LocalVar("Exp14__5_0_1", BitVecType(64), 0),
          LocalVar("load11", BitVecType(64), 0),
          Some("4196084_0$1")
        ),
        LocalAssign(Register("R17", 64), LocalVar("Exp14__5_0_1", BitVecType(64), 0), Some("4196084_1")),
        LocalAssign(
          Register("R16", 64),
          BinaryExpr(BVADD, Register("R16", 64), BitVecLiteral(BigInt("32"), 64)),
          Some("4196088_0")
        ),
        directCall("printf"),
        goto("FUN_4006f0_4196080_basil_return_4")
      ),
      block("FUN_4006f0_4196080_basil_return_4", goto("main_4196096__3__tXIOhSQ~R1WA_9VL5~6KQQ_inlineret")),
      block(
        "main_4196096__0__NfWWPq4PTwyv0VapVhBGag_goto_main_4196096__1__cdQ2GS2~QhaOa7OUvPWMRQ_inlineret",
        goto("main_4196096__1__cdQ2GS2~QhaOa7OUvPWMRQ")
      ),
      block(
        "FUN_4006b0_4196016__0__Djx7L34DQzuSXaBFEj_bpQ_7",
        LocalAssign(Register("R16", 64), BitVecLiteral(BigInt("4325376"), 64), Some("4196016_0")),
        MemoryLoad(
          LocalVar("load20", BitVecType(64), 0),
          SharedMemory("mem", 64, 8),
          Register("R16", 64),
          Endian.LittleEndian,
          64,
          Some("4196020_0$0")
        ),
        LocalAssign(
          LocalVar("Exp14__5_0_1", BitVecType(64), 0),
          LocalVar("load20", BitVecType(64), 0),
          Some("4196020_0$1")
        ),
        LocalAssign(Register("R17", 64), LocalVar("Exp14__5_0_1", BitVecType(64), 0), Some("4196020_1")),
        LocalAssign(Register("R16", 64), Register("R16", 64), Some("4196024_0")),
        directCall("strtoul"),
        goto("FUN_4006b0_4196016_basil_return_8")
      ),
      block(
        "FUN_4006b0_4196016_basil_return_8",
        goto("main_4196096__0__NfWWPq4PTwyv0VapVhBGag_goto_main_4196096__1__cdQ2GS2~QhaOa7OUvPWMRQ_inlineret")
      )
    ),
    proc(
      "Sqrt_4196496",
      block(
        "Sqrt_4196496__0__H7LPJaZbQvWx~VL~kBk__Q",
        LocalAssign(Register("R1", 64), BitVecLiteral(BigInt("12297829382473034410"), 64), Some("4196496_0")),
        LocalAssign(
          Register("R1", 64),
          BinaryExpr(BVCONCAT, Extract(64, 16, Register("R1", 64)), BitVecLiteral(BigInt("43691"), 16)),
          Some("4196500_0")
        ),
        LocalAssign(
          Register("R1", 64),
          Extract(128, 64, BinaryExpr(BVMUL, ZeroExtend(64, Register("R0", 64)), ZeroExtend(64, Register("R1", 64)))),
          Some("4196504_0")
        ),
        LocalAssign(Register("R1", 64), ZeroExtend(1, Extract(64, 1, Register("R1", 64))), Some("4196508_0")),
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ",
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__0", "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__1")
      ),
      block(
        "Sqrt_4196496__2__I2aaHzp~TUWUl9oP4OxUPg",
        goto("Sqrt_4196496__2__I2aaHzp~TUWUl9oP4OxUPg$__0", "Sqrt_4196496__2__I2aaHzp~TUWUl9oP4OxUPg$__1")
      ),
      block(
        "Sqrt_4196496__2__I2aaHzp~TUWUl9oP4OxUPg$__0",
        Assume(
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BoolAND,
              BinaryExpr(EQ, Register("CF", 1), BitVecLiteral(BigInt("1"), 1)),
              BinaryExpr(EQ, Register("ZF", 1), BitVecLiteral(BigInt("0"), 1))
            )
          ),
          None,
          None,
          true
        ),
        LocalAssign(Register("R0", 64), Register("R1", 64), Some("4196568_0")),
        goto("Sqrt_4196496_basil_return")
      ),
      block(
        "Sqrt_4196496__2__I2aaHzp~TUWUl9oP4OxUPg$__1",
        Assume(
          UnaryExpr(
            BoolNOT,
            UnaryExpr(
              BoolNOT,
              BinaryExpr(
                BoolAND,
                BinaryExpr(EQ, Register("CF", 1), BitVecLiteral(BigInt("1"), 1)),
                BinaryExpr(EQ, Register("ZF", 1), BitVecLiteral(BigInt("0"), 1))
              )
            )
          ),
          None,
          None,
          true
        ),
        LocalAssign(
          Register("R0", 64),
          BinaryExpr(BVADD, Register("R1", 64), BitVecLiteral(BigInt("1"), 64)),
          Some("4196568_0")
        ),
        goto("Sqrt_4196496_basil_return")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ_goto_Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ",
        Assume(
          BinaryExpr(
            BoolAND,
            BinaryExpr(EQ, Register("CF", 1), BitVecLiteral(BigInt("1"), 1)),
            BinaryExpr(EQ, Register("ZF", 1), BitVecLiteral(BigInt("0"), 1))
          ),
          None,
          None,
          true
        ),
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ_goto_Sqrt_4196496__2__I2aaHzp~TUWUl9oP4OxUPg",
        Assume(
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BoolAND,
              BinaryExpr(EQ, Register("CF", 1), BitVecLiteral(BigInt("1"), 1)),
              BinaryExpr(EQ, Register("ZF", 1), BitVecLiteral(BigInt("0"), 1))
            )
          ),
          None,
          None,
          true
        ),
        LocalAssign(Register("R2", 64), BinaryExpr(BVMUL, Register("R1", 64), Register("R1", 64)), Some("4196560_0")),
        LocalAssign(
          LocalVar("Cse2__5_1_1", BitVecType(64), 0),
          UnaryExpr(BVNOT, Register("R0", 64)),
          Some("4196564_0")
        ),
        LocalAssign(
          LocalVar("Cse0__5_1_1", BitVecType(64), 0),
          BinaryExpr(BVADD, Register("R2", 64), UnaryExpr(BVNOT, Register("R0", 64))),
          Some("4196564_1")
        ),
        LocalAssign(
          Register("VF", 1),
          UnaryExpr(
            BVNOT,
            BinaryExpr(
              BVCOMP,
              SignExtend(
                64,
                BinaryExpr(BVADD, LocalVar("Cse0__5_1_1", BitVecType(64), 0), BitVecLiteral(BigInt("1"), 64))
              ),
              BinaryExpr(
                BVADD,
                BinaryExpr(
                  BVADD,
                  SignExtend(64, Register("R2", 64)),
                  SignExtend(64, LocalVar("Cse2__5_1_1", BitVecType(64), 0))
                ),
                BitVecLiteral(BigInt("1"), 128)
              )
            )
          ),
          Some("4196564_2")
        ),
        LocalAssign(
          Register("CF", 1),
          UnaryExpr(
            BVNOT,
            BinaryExpr(
              BVCOMP,
              ZeroExtend(
                64,
                BinaryExpr(BVADD, LocalVar("Cse0__5_1_1", BitVecType(64), 0), BitVecLiteral(BigInt("1"), 64))
              ),
              BinaryExpr(
                BVADD,
                BinaryExpr(
                  BVADD,
                  ZeroExtend(64, Register("R2", 64)),
                  ZeroExtend(64, LocalVar("Cse2__5_1_1", BitVecType(64), 0))
                ),
                BitVecLiteral(BigInt("1"), 128)
              )
            )
          ),
          Some("4196564_3")
        ),
        LocalAssign(
          Register("ZF", 1),
          BinaryExpr(
            BVCOMP,
            BinaryExpr(BVADD, LocalVar("Cse0__5_1_1", BitVecType(64), 0), BitVecLiteral(BigInt("1"), 64)),
            BitVecLiteral(BigInt("0"), 64)
          ),
          Some("4196564_4")
        ),
        LocalAssign(
          Register("NF", 1),
          Extract(
            64,
            63,
            BinaryExpr(BVADD, LocalVar("Cse0__5_1_1", BitVecType(64), 0), BitVecLiteral(BigInt("1"), 64))
          ),
          Some("4196564_5")
        ),
        goto("Sqrt_4196496__2__I2aaHzp~TUWUl9oP4OxUPg")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__0",
        Assume(BinaryExpr(EQ, Register("R1", 64), BitVecLiteral(BigInt("0"), 64)), None, None, true),
        LocalAssign(Register("R3", 64), BitVecLiteral(BigInt("0"), 64), Some("4196512_0")),
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__2")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__1",
        Assume(
          UnaryExpr(BoolNOT, BinaryExpr(EQ, Register("R1", 64), BitVecLiteral(BigInt("0"), 64))),
          None,
          None,
          true
        ),
        LocalAssign(
          Register("R3", 64),
          Extract(64, 0, BinaryExpr(BVSDIV, ZeroExtend(64, Register("R0", 64)), ZeroExtend(64, Register("R1", 64)))),
          Some("4196512_0")
        ),
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__2")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__2",
        LocalAssign(Register("R2", 64), Register("R1", 64), Some("4196516_0")),
        LocalAssign(Register("R1", 64), BinaryExpr(BVADD, Register("R3", 64), Register("R1", 64)), Some("4196520_0")),
        LocalAssign(Register("R1", 64), ZeroExtend(1, Extract(64, 1, Register("R1", 64))), Some("4196524_0")),
        LocalAssign(
          LocalVar("Cse2__5_2_4", BitVecType(64), 0),
          UnaryExpr(BVNOT, Register("R2", 64)),
          Some("4196528_0")
        ),
        LocalAssign(
          LocalVar("Cse0__5_2_4", BitVecType(64), 0),
          BinaryExpr(BVADD, Register("R1", 64), UnaryExpr(BVNOT, Register("R2", 64))),
          Some("4196528_1")
        ),
        LocalAssign(
          Register("VF", 1),
          UnaryExpr(
            BVNOT,
            BinaryExpr(
              BVCOMP,
              SignExtend(
                64,
                BinaryExpr(BVADD, LocalVar("Cse0__5_2_4", BitVecType(64), 0), BitVecLiteral(BigInt("1"), 64))
              ),
              BinaryExpr(
                BVADD,
                BinaryExpr(
                  BVADD,
                  SignExtend(64, Register("R1", 64)),
                  SignExtend(64, LocalVar("Cse2__5_2_4", BitVecType(64), 0))
                ),
                BitVecLiteral(BigInt("1"), 128)
              )
            )
          ),
          Some("4196528_2")
        ),
        LocalAssign(
          Register("CF", 1),
          UnaryExpr(
            BVNOT,
            BinaryExpr(
              BVCOMP,
              ZeroExtend(
                64,
                BinaryExpr(BVADD, LocalVar("Cse0__5_2_4", BitVecType(64), 0), BitVecLiteral(BigInt("1"), 64))
              ),
              BinaryExpr(
                BVADD,
                BinaryExpr(
                  BVADD,
                  ZeroExtend(64, Register("R1", 64)),
                  ZeroExtend(64, LocalVar("Cse2__5_2_4", BitVecType(64), 0))
                ),
                BitVecLiteral(BigInt("1"), 128)
              )
            )
          ),
          Some("4196528_3")
        ),
        LocalAssign(
          Register("ZF", 1),
          BinaryExpr(
            BVCOMP,
            BinaryExpr(BVADD, LocalVar("Cse0__5_2_4", BitVecType(64), 0), BitVecLiteral(BigInt("1"), 64)),
            BitVecLiteral(BigInt("0"), 64)
          ),
          Some("4196528_4")
        ),
        LocalAssign(
          Register("NF", 1),
          Extract(
            64,
            63,
            BinaryExpr(BVADD, LocalVar("Cse0__5_2_4", BitVecType(64), 0), BitVecLiteral(BigInt("1"), 64))
          ),
          Some("4196528_5")
        ),
        LocalAssign(
          Register("R3", 64),
          BinaryExpr(
            BVADD,
            BinaryExpr(BVADD, Register("R2", 64), UnaryExpr(BVNOT, Register("R1", 64))),
            BitVecLiteral(BigInt("1"), 64)
          ),
          Some("4196532_0")
        ),
        LocalAssign(
          Register("R2", 64),
          BinaryExpr(
            BVADD,
            BinaryExpr(BVADD, Register("R1", 64), UnaryExpr(BVNOT, Register("R2", 64))),
            BitVecLiteral(BigInt("1"), 64)
          ),
          Some("4196536_0")
        ),
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__3", "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__4")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__3",
        Assume(UnaryExpr(BoolNOT, BinaryExpr(EQ, Register("CF", 1), BitVecLiteral(BigInt("1"), 1))), None, None, true),
        LocalAssign(Register("R3", 64), Register("R3", 64), Some("4196540_0")),
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__5")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__4",
        Assume(
          UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, BinaryExpr(EQ, Register("CF", 1), BitVecLiteral(BigInt("1"), 1)))),
          None,
          None,
          true
        ),
        LocalAssign(Register("R3", 64), BitVecLiteral(BigInt("0"), 64), Some("4196540_0")),
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__5")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__5",
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__6", "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__7")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__6",
        Assume(
          BinaryExpr(
            BoolAND,
            BinaryExpr(EQ, Register("CF", 1), BitVecLiteral(BigInt("1"), 1)),
            BinaryExpr(EQ, Register("ZF", 1), BitVecLiteral(BigInt("0"), 1))
          ),
          None,
          None,
          true
        ),
        LocalAssign(Register("R2", 64), Register("R2", 64), Some("4196544_0")),
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__8")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__7",
        Assume(
          UnaryExpr(
            BoolNOT,
            BinaryExpr(
              BoolAND,
              BinaryExpr(EQ, Register("CF", 1), BitVecLiteral(BigInt("1"), 1)),
              BinaryExpr(EQ, Register("ZF", 1), BitVecLiteral(BigInt("0"), 1))
            )
          ),
          None,
          None,
          true
        ),
        LocalAssign(Register("R2", 64), BitVecLiteral(BigInt("0"), 64), Some("4196544_0")),
        goto("Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__8")
      ),
      block(
        "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ$__8",
        LocalAssign(Register("R3", 64), BinaryExpr(BVADD, Register("R3", 64), Register("R2", 64)), Some("4196548_0")),
        LocalAssign(
          LocalVar("Cse0__5_2_10", BitVecType(64), 0),
          BinaryExpr(BVADD, Register("R3", 64), BitVecLiteral(BigInt("18446744073709551615"), 64)),
          Some("4196552_0")
        ),
        LocalAssign(
          Register("VF", 1),
          UnaryExpr(
            BVNOT,
            BinaryExpr(
              BVCOMP,
              SignExtend(64, LocalVar("Cse0__5_2_10", BitVecType(64), 0)),
              BinaryExpr(
                BVADD,
                SignExtend(64, Register("R3", 64)),
                BitVecLiteral(BigInt("340282366920938463463374607431768211455"), 128)
              )
            )
          ),
          Some("4196552_1")
        ),
        LocalAssign(
          Register("CF", 1),
          UnaryExpr(
            BVNOT,
            BinaryExpr(
              BVCOMP,
              ZeroExtend(64, LocalVar("Cse0__5_2_10", BitVecType(64), 0)),
              BinaryExpr(BVADD, ZeroExtend(64, Register("R3", 64)), BitVecLiteral(BigInt("18446744073709551615"), 128))
            )
          ),
          Some("4196552_2")
        ),
        LocalAssign(
          Register("ZF", 1),
          BinaryExpr(BVCOMP, LocalVar("Cse0__5_2_10", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64)),
          Some("4196552_3")
        ),
        LocalAssign(Register("NF", 1), Extract(64, 63, LocalVar("Cse0__5_2_10", BitVecType(64), 0)), Some("4196552_4")),
        goto(
          "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ_goto_Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ",
          "Sqrt_4196496__1__5jlZESzjQ4a4yO3~e1nyHQ_goto_Sqrt_4196496__2__I2aaHzp~TUWUl9oP4OxUPg"
        )
      ),
      block("Sqrt_4196496_basil_return", ret)
    ),
    proc("printf"),
    proc("strtoul")
  )

  test("conds inline test tvsimp") {

    var ctx = util.IRLoading.load(testProgram)
    util.IRTransform.doCleanup(ctx, true)
    ir.transforms.clearParams(ctx.program)

    ir.transforms.validate.validatedSimplifyPipeline(ctx, SimplifyMode.Simplify)
    for (p <- ctx.program.procedures) {
      p.normaliseBlockNames()
    }

    (ctx.program).foreach {
      case a: Assume => {
        assert(!(a.body.variables.exists(v => {
          v.name.startsWith("ZF")
          || v.name.startsWith("CF")
          || v.name.startsWith("VF")
          || v.name.startsWith("NF")
          || v.name.startsWith("Cse")
        })))
      }
      case _ => ()
    }
  }

  test("conds inline test") {

    var ctx = util.IRLoading.load(testProgram)
    util.IRTransform.doCleanup(ctx, true)
    ir.transforms.clearParams(ctx.program)
    ctx = ir.transforms.liftProcedureCallAbstraction(ctx)
    util.RunUtils.doSimplify(ctx, None)
    for (p <- ctx.program.procedures) {
      p.normaliseBlockNames()
    }

    (ctx.program).foreach {
      case a: Assume => {
        assert(!(a.body.variables.exists(v => {
          v.name.startsWith("ZF")
          || v.name.startsWith("CF")
          || v.name.startsWith("VF")
          || v.name.startsWith("NF")
          || v.name.startsWith("Cse")
        })))
      }
      case _ => ()
    }
  }

}
