package ir

import util.functional._
import ir.eval._
import ir.dsl._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import specification.SpecGlobal
import translating.BAPToIR
import util.{LogLevel, Logger}
import util.IRLoading.{loadBAP, loadReadELF}
import util.ILLoadingConfig

// def initialMem(): MemoryState = {
//   val SP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
//   val FP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
//   val LR: BitVecLiteral = BitVecLiteral(BigInt("FF", 16), 64)
// 
//   MemoryState()
//     .setVar(globalFrame, "mem", MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
//     .setVar(globalFrame, "stack", MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
//     .setVar(globalFrame, "R31", Scalar(SP))
//     .setVar(globalFrame, "R29", Scalar(FP))
//     .setVar(globalFrame, "R30", Scalar(LR))
// }


// def initialMem() = InterpFuns.initialState(InterpreterState(), List())

def load(s: InterpreterState, global: SpecGlobal) : Option[BitVecLiteral] = {
  println(s)
  val f = NormalInterpreter
 //  i.getMemory(global.address.toInt, global.size, Endian.LittleEndian, i.mems)
  // m.evalBV("mem", BitVecLiteral(64, global.address), Endian.LittleEndian, global.size) //  i.getMemory(global.address.toInt, global.size, Endian.LittleEndian, i.mems)
  
  try {
    Some(f.evalBV(MemoryLoad(SharedMemory("mem", 64, 8), BitVecLiteral(global.address, 64), Endian.LittleEndian, global.size)).f(s)._2)
  } catch {
    case e : InterpreterError => None
  }
}


def mems[T <: Effects[T]](m: MemoryState) : Map[BigInt, BitVecLiteral] = {
  m.getMem("mem").map((k,v) => k.value -> v)
}

class InterpreterTests extends AnyFunSuite with BeforeAndAfter {

  // var i: Interpreter = Interpreter()
  Logger.setLevel(LogLevel.DEBUG)


  def getProgram(name: String): (Program, Set[SpecGlobal]) = {

    val loading = ILLoadingConfig(
      inputFile = s"examples/$name/$name.adt",
      relfFile = s"examples/$name/$name.relf",
      specFile = None,
      dumpIL = None
    )

    val bapProgram = loadBAP(loading.inputFile)
    val (externalFunctions, globals, _, mainAddress) = loadReadELF(loading.relfFile, loading)
    val IRTranslator = BAPToIR(bapProgram, mainAddress)
    var IRProgram = IRTranslator.translate
    IRProgram = ExternalRemover(externalFunctions.map(e => e.name)).visitProgram(IRProgram)
    IRProgram = Renamer(Set("free")).visitProgram(IRProgram)
    transforms.stripUnreachableFunctions(IRProgram)
    val stackIdentification = StackSubstituter()
    stackIdentification.visitProgram(IRProgram)
    IRProgram.setModifies(Map())

    (IRProgram, globals)
  }

  def testInterpret(name: String, expected: Map[String, Int]): Unit = {
    val (program, globals) = getProgram(name)
    val fstate = interpret(program)
    val regs = fstate.memoryState.getGlobalVals

    // Show interpreted result
    Logger.info("Registers:")
    regs.foreach { (key, value) =>
      Logger.info(s"$key := $value")
    }

    Logger.info("Globals:")
    //  def loadBV(vname: String, addr: BasilValue, valueSize: Int, endian: Endian, size: Int): List[BitVecLiteral] = {
    globals.foreach { global =>
      val mem = load(fstate, global)
      mem.foreach(mem => Logger.info(s"$global := $mem"))
    }

    // Test expected value
    val actual : Map[String, Int] = expected.flatMap ( (name, expected) =>
      globals.find(_.name == name).flatMap(global =>
          load(fstate, global).map(gv => name -> gv.value.toInt)
      )
    )
    assert(fstate.nextCmd == Stopped())
    assert(expected == actual)
  }

  test("initialise") {

    val init = InterpFuns.initialState(NormalInterpreter)

    val s = State.execute(InterpreterState(), init)
    assert(s.memoryState.getVarOpt("mem").isDefined)
    assert(s.memoryState.getVarOpt("stack").isDefined)
    assert(s.memoryState.getVarOpt("R31").isDefined)
    assert(s.memoryState.getVarOpt("R29").isDefined)


  }
  test("var load store") {
  val s = for {
       s <- InterpFuns.initialState(NormalInterpreter)
       v <- NormalInterpreter.loadVar("R31")
   } yield (v)
  val l = State.evaluate(InterpreterState(), s)

  assert(l == Scalar(BitVecLiteral(4096 - 16, 64)))

  }

   test("Store = Load LittleEndian") {
     val ts = List(
       BitVecLiteral(BigInt("0D", 16), 8),
       BitVecLiteral(BigInt("0C", 16), 8),
       BitVecLiteral(BigInt("0B", 16), 8),
       BitVecLiteral(BigInt("0A", 16), 8))
 
     val loader = StVarLoader(NormalInterpreter)

     val s = for {
       _ <- InterpFuns.initialState(NormalInterpreter)
       _ <- Eval.store(NormalInterpreter)("mem", Scalar(BitVecLiteral(0, 64)), ts.map(Scalar(_)), Endian.LittleEndian)
       r <- loader.loadBV("mem", Scalar(BitVecLiteral(0, 64)), Endian.LittleEndian, 32)
     } yield(r)
     val expected: BitVecLiteral = BitVecLiteral(BigInt("0D0C0B0A", 16), 32)
     val actual: BitVecLiteral = State.evaluate(InterpreterState(), s)
     assert(actual == expected)
 
 
   }
 
//   test("store bv = loadbv le") {
//     val expected: BitVecLiteral = BitVecLiteral(BigInt("0D0C0B0A", 16), 32)
//     val s2 = Eval.storeBV(initialMem(), "mem", Scalar(BitVecLiteral(0, 64)), expected, Endian.LittleEndian)
//     val actual2: BitVecLiteral = Eval.loadBV(s2, "mem", Scalar(BitVecLiteral(0, 64)), Endian.LittleEndian, 32)
//     assert(actual2 == expected)
//   }
// 
// 
//   test("Store = Load BigEndian") {
//     val ts = List(
//       BitVecLiteral(BigInt("0D", 16), 8),
//       BitVecLiteral(BigInt("0C", 16), 8),
//       BitVecLiteral(BigInt("0B", 16), 8),
//       BitVecLiteral(BigInt("0A", 16), 8))
// 
//     val s = Eval.store(initialMem(), "mem", Scalar(BitVecLiteral(0, 64)), ts.map(Scalar(_)), Endian.LittleEndian)
//     val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
//     val actual: BitVecLiteral = Eval.loadBV(s, "mem", Scalar(BitVecLiteral(0, 64)), Endian.BigEndian , 32)
//     assert(actual == expected)
// 
// 
//   }
// 
//   test("getMemory in LittleEndian") {
//     val ts = List((BitVecLiteral(0, 64), BitVecLiteral(BigInt("0D", 16), 8)),
//     (BitVecLiteral(1, 64) , BitVecLiteral(BigInt("0C", 16), 8)),
//     (BitVecLiteral(2, 64) , BitVecLiteral(BigInt("0B", 16), 8)),
//     (BitVecLiteral(3, 64) , BitVecLiteral(BigInt("0A", 16), 8)))
//     val s = ts.foldLeft(initialMem())((m, v) => Eval.storeSingle(m, "mem", Scalar(v._1), Scalar(v._2)))
//     // val s = initialMem().store("mem")
//     // val r = s.loadBV("mem", BitVecLiteral(0, 64))
// 
//     val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
// 
//   // def loadBV(vname: String, addr: Scalar, endian: Endian, size: Int): BitVecLiteral = {
//      val actual: BitVecLiteral = Eval.loadBV(s, "mem", Scalar(BitVecLiteral(0, 64)), Endian.LittleEndian, 32)
//     assert(actual == expected)
//   }
// 
// 
//   test("StoreBV = LoadBV LE ") {
//     val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
// 
//     val s = Eval.storeBV(initialMem(), "mem", Scalar(BitVecLiteral(0, 64)), expected, Endian.LittleEndian)
//     val actual: BitVecLiteral = Eval.loadBV(s, "mem", Scalar(BitVecLiteral(0, 64)), Endian.LittleEndian, 32)
//     println(s"${actual.value.toInt.toHexString} == ${expected.value.toInt.toHexString}")
//     assert(actual == expected)
//   }
// 
//   // test("getMemory in BigEndian") {
//   //   i.mems(0) = BitVecLiteral(BigInt("0A", 16), 8)
//   //   i.mems(1) = BitVecLiteral(BigInt("0B", 16), 8)
//   //   i.mems(2) = BitVecLiteral(BigInt("0C", 16), 8)
//   //   i.mems(3) = BitVecLiteral(BigInt("0D", 16), 8)
//   //   val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
//   //   val actual: BitVecLiteral = i.getMemory(0, 32, Endian.BigEndian, i.mems)
//   //   assert(actual == expected)
//   // }
// 
//   // test("setMemory in LittleEndian") {
//   //   i.mems(0) = BitVecLiteral(BigInt("FF", 16), 8)
//   //   i.mems(1) = BitVecLiteral(BigInt("FF", 16), 8)
//   //   i.mems(2) = BitVecLiteral(BigInt("FF", 16), 8)
//   //   i.mems(3) = BitVecLiteral(BigInt("FF", 16), 8)
//   //   val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
//   //   i.setMemory(0, 32, Endian.LittleEndian, expected, i.mems)
//   //   val actual: BitVecLiteral = i.getMemory(0, 32, Endian.LittleEndian, i.mems)
//   //   assert(actual == expected)
//   // }
// 
//   // test("setMemory in BigEndian") {
//   //   i.mems(0) = BitVecLiteral(BigInt("FF", 16), 8)
//   //   i.mems(1) = BitVecLiteral(BigInt("FF", 16), 8)
//   //   i.mems(2) = BitVecLiteral(BigInt("FF", 16), 8)
//   //   i.mems(3) = BitVecLiteral(BigInt("FF", 16), 8)
//   //   val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
//   //   i.setMemory(0, 32, Endian.BigEndian, expected, i.mems)
//   //   val actual: BitVecLiteral = i.getMemory(0, 32, Endian.BigEndian, i.mems)
//   //   assert(actual == expected)
//   // }
// 
  test("basic_arrays_read") {
    val expected = Map(
      "arr" -> 0
    )
    testInterpret("basic_arrays_read", expected)
  }

  test("basic_assign_assign") {
    val expected = Map(
      "x" -> 5
    )
    testInterpret("basic_assign_assign", expected)
  }

  test("basic_assign_increment") {
    val expected = Map(
      "x" -> 1
    )
    testInterpret("basic_assign_increment", expected)
  }

  test("basic_loop_loop") {
    val expected = Map(
      "x" -> 10
    )
    testInterpret("basic_loop_loop", expected)
  }

  test("basicassign") {
    val expected = Map(
      "x" -> 0,
      "z" -> 0,
      "secret" -> 0
    )
    testInterpret("basicassign", expected)
  }

  test("function") {
    val expected = Map(
      "x" -> 1,
      "y" -> 2
    )
    testInterpret("function", expected)
  }

  test("function1") {
    val expected = Map(
      "x" -> 1,
      "y" -> 1410065515 // 10000000107 % 2147483648 = 1410065515
    )
    testInterpret("function1", expected)
  }

  test("secret_write") {
    val expected = Map(
      "z" -> 2,
      "x" -> 0,
      "secret" -> 0
    )
    testInterpret("secret_write", expected)
  }

  test("ifglobal") {
    val expected = Map(
      "x" -> 1
    )
    testInterpret("ifglobal", expected)
  }

  test("cjump") {
    val expected = Map(
      "x" -> 1,
      "y" -> 3
    )
    testInterpret("cjump", expected)
  }

  test("no_interference_update_x") {
    val expected = Map(
      "x" -> 1
    )
    testInterpret("no_interference_update_x", expected)
  }

  test("no_interference_update_y") {
    val expected = Map(
      "y" -> 1
    )
    testInterpret("no_interference_update_y", expected)
  }


  def fibonacciProg(n: Int) = {
    def expected(n: Int) : Int = {
      n match {
        case 0 => 0
        case 1 => 1
        case n => expected(n - 1) + expected(n - 2)
      }
    }
    prog(
      proc("begin", 
        block("entry",
          Assign(R8, Register("R31", 64)),
          Assign(R0, bv64(n)),
          directCall("fib"),
          goto("done")
        ),
        block("done",
          Assert(BinaryExpr(BVEQ, R0, bv64(expected(n)))),
          ret
        )),
      proc("fib",
        block("base", goto("base1", "base2", "dofib")),
        block("base1", 
          Assume(BinaryExpr(BVEQ, R0, bv64(0))),
          ret),
        block("base2", 
          Assume(BinaryExpr(BVEQ, R0, bv64(1))),
          ret),
        block("dofib",
          Assume(BinaryExpr(BoolAND, BinaryExpr(BVNEQ, R0, bv64(0)), BinaryExpr(BVNEQ, R0, bv64(1)))),
          // R8 stack pointer preserved across calls
          Assign(R7, BinaryExpr(BVADD, R8, bv64(8))),  
          MemoryAssign(stack, R7, R8, Endian.LittleEndian, 64), // sp
          Assign(R8, R7),
          Assign(R8, BinaryExpr(BVADD, R8, bv64(8))),  // sp + 8
          MemoryAssign(stack, R8, R0, Endian.LittleEndian, 64), // [sp + 8] = arg0
          Assign(R0, BinaryExpr(BVSUB, R0, bv64(1))),
          directCall("fib"),
          Assign(R2, R8), // sp + 8
          Assign(R8, BinaryExpr(BVADD, R8, bv64(8))),  // sp + 16
          MemoryAssign(stack, R8, R0, Endian.LittleEndian, 64), // [sp + 16] = r1
          Assign(R0, MemoryLoad(stack, R2, Endian.LittleEndian, 64)), // [sp + 8]
          Assign(R0, BinaryExpr(BVSUB, R0, bv64(2))),
          directCall("fib"),
          Assign(R2, MemoryLoad(stack, R8, Endian.LittleEndian, 64)), // [sp + 16] (r1)
          Assign(R0, BinaryExpr(BVADD, R0, R2)),
          Assign(R8, MemoryLoad(stack, BinaryExpr(BVSUB, R8, bv64(16)), Endian.LittleEndian, 64)),
          ret
          )
        )
      )
  }

  test("fibonacci") {

    val fib = fibonacciProg(8)
    val r = interpret(fib)
    assert(r.nextCmd == Stopped())
    // Show interpreted result
    Logger.info("Registers:")
    // r.regs.foreach { (key, value) =>
    //   Logger.info(s"$key := $value")
    // }

  }


//   test("fibonacci Trace") {
// 
//     val fib = fibonacciProg(8)
//     val r = interpretTrace(fib)
//     assert(r.getNext == Stopped())
//     // Show interpreted result
//     //
//     info(r.trace.reverse.mkString("\n"))
// 
//   }

}
