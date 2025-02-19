package ir

import ir.*
import ir.dsl.*

object IRToDSLTestData {

  lazy val function1 = prog(
    proc(
      "get_two",
      Seq(
        "R0_in" -> BitVecType(64),
        "R10_in" -> BitVecType(64),
        "R11_in" -> BitVecType(64),
        "R12_in" -> BitVecType(64),
        "R13_in" -> BitVecType(64),
        "R14_in" -> BitVecType(64),
        "R15_in" -> BitVecType(64),
        "R16_in" -> BitVecType(64),
        "R17_in" -> BitVecType(64),
        "R18_in" -> BitVecType(64),
        "R1_in" -> BitVecType(64),
        "R29_in" -> BitVecType(64),
        "R2_in" -> BitVecType(64),
        "R30_in" -> BitVecType(64),
        "R31_in" -> BitVecType(64),
        "R3_in" -> BitVecType(64),
        "R4_in" -> BitVecType(64),
        "R5_in" -> BitVecType(64),
        "R6_in" -> BitVecType(64),
        "R7_in" -> BitVecType(64),
        "R8_in" -> BitVecType(64),
        "R9_in" -> BitVecType(64)
      ),
      Seq("R0_out" -> BitVecType(64), "R1_out" -> BitVecType(64), "R31_out" -> BitVecType(64)),
      block(
        "lget_two",
        LocalAssign(LocalVar("R0", BitVecType(64), 0), LocalVar("R0_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R10", BitVecType(64), 0), LocalVar("R10_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R11", BitVecType(64), 0), LocalVar("R11_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R12", BitVecType(64), 0), LocalVar("R12_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R13", BitVecType(64), 0), LocalVar("R13_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R14", BitVecType(64), 0), LocalVar("R14_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R15", BitVecType(64), 0), LocalVar("R15_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R16", BitVecType(64), 0), LocalVar("R16_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R17", BitVecType(64), 0), LocalVar("R17_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R18", BitVecType(64), 0), LocalVar("R18_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R1", BitVecType(64), 0), LocalVar("R1_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R29", BitVecType(64), 0), LocalVar("R29_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R2", BitVecType(64), 0), LocalVar("R2_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R30", BitVecType(64), 0), LocalVar("R30_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R31", BitVecType(64), 0), LocalVar("R31_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R3", BitVecType(64), 0), LocalVar("R3_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R4", BitVecType(64), 0), LocalVar("R4_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R5", BitVecType(64), 0), LocalVar("R5_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R6", BitVecType(64), 0), LocalVar("R6_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R7", BitVecType(64), 0), LocalVar("R7_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R8", BitVecType(64), 0), LocalVar("R8_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R9", BitVecType(64), 0), LocalVar("R9_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R30_begin", BitVecType(64), 0), LocalVar("R30", BitVecType(64), 0), None),
        LocalAssign(
          LocalVar("R31", BitVecType(64), 0),
          BinaryExpr(BVADD, LocalVar("R31", BitVecType(64), 0), BitVecLiteral(BigInt("18446744073709551600"), 64)),
          Some("%00000346")
        ),
        MemoryStore(
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, LocalVar("R31", BitVecType(64), 0), BitVecLiteral(BigInt("15"), 64)),
          Extract(8, 0, LocalVar("R0", BitVecType(64), 0)),
          Endian.LittleEndian,
          8,
          Some("%0000034e")
        ),
        MemoryStore(
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, LocalVar("R31", BitVecType(64), 0), BitVecLiteral(BigInt("8"), 64)),
          Extract(32, 0, LocalVar("R1", BitVecType(64), 0)),
          Endian.LittleEndian,
          32,
          Some("%00000356")
        ),
        MemoryStore(
          StackMemory("stack", 64, 8),
          LocalVar("R31", BitVecType(64), 0),
          LocalVar("R2", BitVecType(64), 0),
          Endian.LittleEndian,
          64,
          Some("%0000035e")
        ),
        MemoryLoad(
          LocalVar("load18", BitVecType(8), 0),
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, LocalVar("R31", BitVecType(64), 0), BitVecLiteral(BigInt("15"), 64)),
          Endian.LittleEndian,
          8,
          Some("%00000365$0")
        ),
        LocalAssign(
          LocalVar("R1", BitVecType(64), 0),
          ZeroExtend(56, LocalVar("load18", BitVecType(8), 0)),
          Some("%00000365$1")
        ),
        MemoryLoad(
          LocalVar("load19", BitVecType(32), 0),
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, LocalVar("R31", BitVecType(64), 0), BitVecLiteral(BigInt("8"), 64)),
          Endian.LittleEndian,
          32,
          Some("%0000036c$0")
        ),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 0),
          ZeroExtend(32, LocalVar("load19", BitVecType(32), 0)),
          Some("%0000036c$1")
        ),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 0),
          ZeroExtend(
            32,
            BinaryExpr(
              BVADD,
              Extract(32, 0, LocalVar("R1", BitVecType(64), 0)),
              Extract(32, 0, LocalVar("R0", BitVecType(64), 0))
            )
          ),
          Some("%00000373")
        ),
        LocalAssign(
          LocalVar("R1", BitVecType(64), 0),
          ZeroExtend(32, Extract(32, 0, LocalVar("R0", BitVecType(64), 0))),
          Some("%00000379")
        ),
        MemoryLoad(
          LocalVar("load20", BitVecType(64), 0),
          StackMemory("stack", 64, 8),
          LocalVar("R31", BitVecType(64), 0),
          Endian.LittleEndian,
          64,
          Some("%00000380$0")
        ),
        LocalAssign(LocalVar("R0", BitVecType(64), 0), LocalVar("load20", BitVecType(64), 0), Some("%00000380$1")),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 0),
          ZeroExtend(
            32,
            BinaryExpr(
              BVADD,
              Extract(32, 0, LocalVar("R1", BitVecType(64), 0)),
              Extract(32, 0, LocalVar("R0", BitVecType(64), 0))
            )
          ),
          Some("%00000387")
        ),
        LocalAssign(
          LocalVar("R31", BitVecType(64), 0),
          BinaryExpr(BVADD, LocalVar("R31", BitVecType(64), 0), BitVecLiteral(BigInt("16"), 64)),
          Some("%0000038d")
        ),
        goto("get_two_1876_basil_return")
      ),
      block(
        "get_two_1876_basil_return",
        ret(
          "R0_out" -> LocalVar("R0", BitVecType(64), 0),
          "R1_out" -> LocalVar("R1", BitVecType(64), 0),
          "R31_out" -> LocalVar("R31", BitVecType(64), 0)
        )
      )
    ),
    proc(
      "main",
      Seq(
        "R0_in" -> BitVecType(64),
        "R10_in" -> BitVecType(64),
        "R11_in" -> BitVecType(64),
        "R12_in" -> BitVecType(64),
        "R13_in" -> BitVecType(64),
        "R14_in" -> BitVecType(64),
        "R15_in" -> BitVecType(64),
        "R16_in" -> BitVecType(64),
        "R17_in" -> BitVecType(64),
        "R18_in" -> BitVecType(64),
        "R1_in" -> BitVecType(64),
        "R29_in" -> BitVecType(64),
        "R2_in" -> BitVecType(64),
        "R30_in" -> BitVecType(64),
        "R31_in" -> BitVecType(64),
        "R3_in" -> BitVecType(64),
        "R4_in" -> BitVecType(64),
        "R5_in" -> BitVecType(64),
        "R6_in" -> BitVecType(64),
        "R7_in" -> BitVecType(64),
        "R8_in" -> BitVecType(64),
        "R9_in" -> BitVecType(64)
      ),
      Seq(
        "R0_out" -> BitVecType(64),
        "R1_out" -> BitVecType(64),
        "R29_out" -> BitVecType(64),
        "R2_out" -> BitVecType(64),
        "R30_out" -> BitVecType(64),
        "R31_out" -> BitVecType(64)
      ),
      block(
        "lmain",
        LocalAssign(LocalVar("R0", BitVecType(64), 0), LocalVar("R0_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R10", BitVecType(64), 0), LocalVar("R10_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R11", BitVecType(64), 0), LocalVar("R11_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R12", BitVecType(64), 0), LocalVar("R12_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R13", BitVecType(64), 0), LocalVar("R13_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R14", BitVecType(64), 0), LocalVar("R14_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R15", BitVecType(64), 0), LocalVar("R15_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R16", BitVecType(64), 0), LocalVar("R16_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R17", BitVecType(64), 0), LocalVar("R17_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R18", BitVecType(64), 0), LocalVar("R18_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R1", BitVecType(64), 0), LocalVar("R1_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R29", BitVecType(64), 0), LocalVar("R29_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R2", BitVecType(64), 0), LocalVar("R2_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R30", BitVecType(64), 0), LocalVar("R30_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R31", BitVecType(64), 0), LocalVar("R31_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R3", BitVecType(64), 0), LocalVar("R3_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R4", BitVecType(64), 0), LocalVar("R4_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R5", BitVecType(64), 0), LocalVar("R5_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R6", BitVecType(64), 0), LocalVar("R6_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R7", BitVecType(64), 0), LocalVar("R7_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R8", BitVecType(64), 0), LocalVar("R8_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R9", BitVecType(64), 0), LocalVar("R9_in", BitVecType(64), 0), None),
        LocalAssign(LocalVar("R30_begin", BitVecType(64), 0), LocalVar("R30", BitVecType(64), 0), None),
        LocalAssign(
          LocalVar("#4", BitVecType(64), 0),
          BinaryExpr(BVADD, LocalVar("R31", BitVecType(64), 0), BitVecLiteral(BigInt("18446744073709551600"), 64)),
          Some("%00000398")
        ),
        MemoryStore(
          StackMemory("stack", 64, 8),
          LocalVar("#4", BitVecType(64), 0),
          LocalVar("R29", BitVecType(64), 0),
          Endian.LittleEndian,
          64,
          Some("%0000039e")
        ),
        MemoryStore(
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, LocalVar("#4", BitVecType(64), 0), BitVecLiteral(BigInt("8"), 64)),
          LocalVar("R30", BitVecType(64), 0),
          Endian.LittleEndian,
          64,
          Some("%000003a4")
        ),
        LocalAssign(LocalVar("R31", BitVecType(64), 0), LocalVar("#4", BitVecType(64), 0), Some("%000003a8")),
        LocalAssign(LocalVar("R29", BitVecType(64), 0), LocalVar("R31", BitVecType(64), 0), Some("%000003ae")),
        LocalAssign(LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("69632"), 64), Some("%000003b3")),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 0),
          BinaryExpr(BVADD, LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("20"), 64)),
          Some("%000003b9")
        ),
        LocalAssign(LocalVar("R1", BitVecType(64), 0), BitVecLiteral(BigInt("1"), 64), Some("%000003be")),
        MemoryStore(
          SharedMemory("mem", 64, 8),
          LocalVar("R0", BitVecType(64), 0),
          Extract(32, 0, LocalVar("R1", BitVecType(64), 0)),
          Endian.LittleEndian,
          32,
          Some("%000003c6")
        ),
        LocalAssign(LocalVar("R2", BitVecType(64), 0), BitVecLiteral(BigInt("58368"), 64), Some("%000003cb")),
        LocalAssign(
          LocalVar("R2", BitVecType(64), 0),
          BinaryExpr(
            BVCONCAT,
            Extract(64, 32, LocalVar("R2", BitVecType(64), 0)),
            BinaryExpr(BVCONCAT, BitVecLiteral(BigInt("21515"), 16), Extract(16, 0, LocalVar("R2", BitVecType(64), 0)))
          ),
          Some("%000003d2")
        ),
        LocalAssign(
          LocalVar("R2", BitVecType(64), 0),
          BinaryExpr(
            BVCONCAT,
            Extract(64, 48, LocalVar("R2", BitVecType(64), 0)),
            BinaryExpr(BVCONCAT, BitVecLiteral(BigInt("2"), 16), Extract(32, 0, LocalVar("R2", BitVecType(64), 0)))
          ),
          Some("%000003d9")
        ),
        LocalAssign(LocalVar("R1", BitVecType(64), 0), BitVecLiteral(BigInt("10"), 64), Some("%000003de")),
        LocalAssign(LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("97"), 64), Some("%000003e3")),
        LocalAssign(LocalVar("R30", BitVecType(64), 0), BitVecLiteral(BigInt("1972"), 64), Some("%000003e8")),
        directCall("get_two"),
        goto("l000003ec")
      ),
      block(
        "l000003ec",
        LocalAssign(
          LocalVar("R1", BitVecType(64), 0),
          ZeroExtend(32, Extract(32, 0, LocalVar("R0", BitVecType(64), 0))),
          Some("%000003f0")
        ),
        LocalAssign(LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("69632"), 64), Some("%000003f5")),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 0),
          BinaryExpr(BVADD, LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("24"), 64)),
          Some("%000003fb")
        ),
        MemoryStore(
          SharedMemory("mem", 64, 8),
          LocalVar("R0", BitVecType(64), 0),
          Extract(32, 0, LocalVar("R1", BitVecType(64), 0)),
          Endian.LittleEndian,
          32,
          Some("%00000403")
        ),
        LocalAssign(LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("69632"), 64), Some("%00000408")),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 0),
          BinaryExpr(BVADD, LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("24"), 64)),
          Some("%0000040e")
        ),
        MemoryLoad(
          LocalVar("load21", BitVecType(32), 0),
          SharedMemory("mem", 64, 8),
          LocalVar("R0", BitVecType(64), 0),
          Endian.LittleEndian,
          32,
          Some("%00000415$0")
        ),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 0),
          ZeroExtend(32, LocalVar("load21", BitVecType(32), 0)),
          Some("%00000415$1")
        ),
        LocalAssign(
          LocalVar("R1", BitVecType(64), 0),
          ZeroExtend(32, Extract(32, 0, LocalVar("R0", BitVecType(64), 0))),
          Some("%0000041b")
        ),
        LocalAssign(LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64), Some("%00000420")),
        LocalAssign(
          LocalVar("R0", BitVecType(64), 0),
          BinaryExpr(BVADD, LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("2056"), 64)),
          Some("%00000426")
        ),
        LocalAssign(LocalVar("R30", BitVecType(64), 0), BitVecLiteral(BigInt("2016"), 64), Some("%0000042b")),
        directCall("printf"),
        goto("l00000430")
      ),
      block(
        "l00000430",
        LocalAssign(LocalVar("R0", BitVecType(64), 0), BitVecLiteral(BigInt("0"), 64), Some("%00000433")),
        MemoryLoad(
          LocalVar("load22", BitVecType(64), 0),
          StackMemory("stack", 64, 8),
          LocalVar("R31", BitVecType(64), 0),
          Endian.LittleEndian,
          64,
          Some("%0000043a$0")
        ),
        LocalAssign(LocalVar("R29", BitVecType(64), 0), LocalVar("load22", BitVecType(64), 0), Some("%0000043a$1")),
        MemoryLoad(
          LocalVar("load23", BitVecType(64), 0),
          StackMemory("stack", 64, 8),
          BinaryExpr(BVADD, LocalVar("R31", BitVecType(64), 0), BitVecLiteral(BigInt("8"), 64)),
          Endian.LittleEndian,
          64,
          Some("%0000043f$0")
        ),
        LocalAssign(LocalVar("R30", BitVecType(64), 0), LocalVar("load23", BitVecType(64), 0), Some("%0000043f$1")),
        LocalAssign(
          LocalVar("R31", BitVecType(64), 0),
          BinaryExpr(BVADD, LocalVar("R31", BitVecType(64), 0), BitVecLiteral(BigInt("16"), 64)),
          Some("%00000443")
        ),
        goto("main_1924_basil_return")
      ),
      block(
        "main_1924_basil_return",
        ret(
          "R0_out" -> LocalVar("R0", BitVecType(64), 0),
          "R1_out" -> LocalVar("R1", BitVecType(64), 0),
          "R29_out" -> LocalVar("R29", BitVecType(64), 0),
          "R2_out" -> LocalVar("R2", BitVecType(64), 0),
          "R30_out" -> LocalVar("R30", BitVecType(64), 0),
          "R31_out" -> LocalVar("R31", BitVecType(64), 0)
        )
      )
    ),
    proc(
      "printf",
      Seq(
        "R0_in" -> BitVecType(64),
        "R10_in" -> BitVecType(64),
        "R11_in" -> BitVecType(64),
        "R12_in" -> BitVecType(64),
        "R13_in" -> BitVecType(64),
        "R14_in" -> BitVecType(64),
        "R15_in" -> BitVecType(64),
        "R16_in" -> BitVecType(64),
        "R17_in" -> BitVecType(64),
        "R18_in" -> BitVecType(64),
        "R1_in" -> BitVecType(64),
        "R29_in" -> BitVecType(64),
        "R2_in" -> BitVecType(64),
        "R30_in" -> BitVecType(64),
        "R3_in" -> BitVecType(64),
        "R4_in" -> BitVecType(64),
        "R5_in" -> BitVecType(64),
        "R6_in" -> BitVecType(64),
        "R7_in" -> BitVecType(64),
        "R8_in" -> BitVecType(64),
        "R9_in" -> BitVecType(64)
      ),
      Seq(
        "R0_out" -> BitVecType(64),
        "R10_out" -> BitVecType(64),
        "R11_out" -> BitVecType(64),
        "R12_out" -> BitVecType(64),
        "R13_out" -> BitVecType(64),
        "R14_out" -> BitVecType(64),
        "R15_out" -> BitVecType(64),
        "R16_out" -> BitVecType(64),
        "R17_out" -> BitVecType(64),
        "R18_out" -> BitVecType(64),
        "R1_out" -> BitVecType(64),
        "R29_out" -> BitVecType(64),
        "R2_out" -> BitVecType(64),
        "R30_out" -> BitVecType(64),
        "R3_out" -> BitVecType(64),
        "R4_out" -> BitVecType(64),
        "R5_out" -> BitVecType(64),
        "R6_out" -> BitVecType(64),
        "R7_out" -> BitVecType(64),
        "R8_out" -> BitVecType(64),
        "R9_out" -> BitVecType(64)
      )
    )
  )

}
