prog(
  proc("main_4196292",
    in = Seq(
      "R0_in" -> BitVecType(64),
      "R1_in" -> BitVecType(64),
      "R31_in" -> BitVecType(64),
      "R8_in" -> BitVecType(64),
      "R9_in" -> BitVecType(64),
      "_PC_in" -> BitVecType(64)
    ),
    out = Seq(
      "R0_out" -> BitVecType(64),
      "R9_out" -> BitVecType(64)
    ),
    returnBlockLabel = Some("main_return")
  )(
    block("main_entry",
      MemoryStore(StackMemory("stack", 64, 8), BinaryExpr(BVADD, LocalVar("R31_in", BitVecType(64), 0), BitVecLiteral(BigInt("18446744073709551612"), 64)), BitVecLiteral(BigInt("0"), 32), Endian.LittleEndian, 32, Some("4196296_0")),
      MemoryStore(StackMemory("stack", 64, 8), BinaryExpr(BVADD, LocalVar("R31_in", BitVecType(64), 0), BitVecLiteral(BigInt("18446744073709551608"), 64)), Extract(32, 0, LocalVar("R0_in", BitVecType(64), 0)), Endian.LittleEndian, 32, Some("4196300_0")),
      MemoryStore(SharedMemory("mem", 64, 8), BitVecLiteral(BigInt("4325472"), 64), BitVecLiteral(BigInt("4325464"), 64), Endian.LittleEndian, 64, Some("4196316_0")),
      MemoryLoad(LocalVar("Exp14__5_18", BitVecType(32), 1), StackMemory("stack", 64, 8), BinaryExpr(BVADD, LocalVar("R31_in", BitVecType(64), 0), BitVecLiteral(BigInt("18446744073709551608"), 64)), Endian.LittleEndian, 32, Some("4196320_0$0")),
      LocalAssign(LocalVar("R9", BitVecType(64), 3), SignExtend(32, LocalVar("Exp14__5_18", BitVecType(32), 1)), Some("4196320_1")),
      MemoryLoad(LocalVar("Exp16__5_19", BitVecType(32), 1), SharedMemory("mem", 64, 8), BinaryExpr(BVADD, BinaryExpr(BVSHL, ZeroExtend(2, Extract(62, 0, SignExtend(32, LocalVar("Exp14__5_18", BitVecType(32), 1)))), BitVecLiteral(BigInt("2"), 64)), BitVecLiteral(BigInt("4325416"), 64)), Endian.LittleEndian, 32, Some("4196332_0$0")),
      LocalAssign(LocalVar("R0", BitVecType(64), 2), ZeroExtend(32, LocalVar("Exp16__5_19", BitVecType(32), 1)), Some("4196332_1")),
      goto("main_return")
    ),
    block("main_return",
      Assert(TrueLiteral, Some("R31 preserved across calls"), None),
      ret(
        "R0_out" -> LocalVar("R0", BitVecType(64), 2),
        "R9_out" -> LocalVar("R9", BitVecType(64), 3)
      )
    )
  )
)