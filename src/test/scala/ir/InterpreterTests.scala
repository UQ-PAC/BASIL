package ir

import util.PerformanceTimer
import util.functional._
import ir.eval._
import ir.dsl._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import specification.SpecGlobal
import translating.BAPToIR
import util.{LogLevel, Logger}
import util.IRLoading.{loadBAP, loadReadELF}
import util.{ILLoadingConfig, IRContext, IRLoading, IRTransform} 


def load(s: InterpreterState, global: SpecGlobal) : Option[BitVecLiteral] = {
  val f = NormalInterpreter
  
  try {
    State.evaluate(s, Eval.evalBV(f)(MemoryLoad(SharedMemory("mem", 64, 8), BitVecLiteral(global.address, 64), Endian.LittleEndian, global.size))) match {
      case Right(e) => Some(e)
      case Left(e) => {
        None
      }

    }
  } catch {
    case e : InterpreterError => None
  }
}


def mems[E, T <: Effects[T, E]](m: MemoryState) : Map[BigInt, BitVecLiteral] = {
  m.getMem("mem").map((k,v) => k.value -> v)
}

class InterpreterTests extends AnyFunSuite with BeforeAndAfter {

  Logger.setLevel(LogLevel.INFO)


  def getProgram(name: String): IRContext = {
    val loading = ILLoadingConfig(
      inputFile = s"examples/$name/$name.adt",
      relfFile = s"examples/$name/$name.relf",
      specFile = None,
      dumpIL = None
    )

    val p = IRLoading.load(loading)
    val ctx = IRTransform.doCleanup(p)
    // val bapProgram = loadBAP(loading.inputFile)
    // val (symbols, externalFunctions, globals, _, mainAddress) = loadReadELF(loading.relfFile, loading)
    // val IRTranslator = BAPToIR(bapProgram, mainAddress)
    // var IRProgram = IRTranslator.translate
    // IRProgram = ExternalRemover(externalFunctions.map(e => e.name)).visitProgram(IRProgram)
    // IRProgram = Renamer(Set("free")).visitProgram(IRProgram)
    //IRProgram.stripUnreachableFunctions()
    // val stackIdentification = StackSubstituter()
    // stackIdentification.visitProgram(IRProgram)
    ctx.program.setModifies(Map())
    ctx
  }

  def testInterpret(name: String, expected: Map[String, Int]): Unit = {
    val ctx = getProgram(name)
    val fstate = interpret(ctx)
    val regs = fstate.memoryState.getGlobalVals
    val globals = ctx.globals

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

  assert(l == Right(Scalar(BitVecLiteral(4096 - 16, 64))))

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
       r <- Eval.loadBV(NormalInterpreter)("mem", Scalar(BitVecLiteral(0, 64)), Endian.LittleEndian, 32)
     } yield(r)
     val expected: BitVecLiteral = BitVecLiteral(BigInt("0D0C0B0A", 16), 32)
     val actual = State.evaluate(InterpreterState(), s)
     assert(actual == Right(expected))
 
 
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

  test("indirect_call") {
    val expected = Map[String, Int]()
    testInterpret("indirect_call_outparam", expected)
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


  test("initialisation") {

    // Logger.setLevel(LogLevel.WARN)
    val expected = Map(
      "x" -> 6,
      "y" -> ('b'.toInt), 
    )


    testInterpret("initialisation", expected)
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


  def fib(n: Int) : Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case n => fib(n - 1) + fib(n - 2)
    }
  }

  def fibonacciProg(n: Int) = {
    prog(
      proc("begin", 
        block("entry",
          Assign(R8, Register("R31", 64)),
          Assign(R0, bv64(n)),
          directCall("fib"),
          goto("done")
        ),
        block("done",
          Assert(BinaryExpr(BVEQ, R0, bv64(fib(n)))),
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

    Logger.setLevel(LogLevel.ERROR)
    val fib = fibonacciProg(8)
    val r = interpret(fib)
    assert(r.nextCmd == Stopped())
    // Show interpreted result
    // r.regs.foreach { (key, value) =>
    //   Logger.info(s"$key := $value")
    // }

  }

  test("fibonaccistress") {

    Logger.setLevel(LogLevel.ERROR)
    var res = List[(Int, Double, Double)]()

    for (i <- 0 to 12) {
      val prog = fibonacciProg(i)

      val t = PerformanceTimer("native")
      val r = fib(i)
      val native = t.elapsed()

      val intt = PerformanceTimer("interp")
      val ir = interpret(prog)
      val it = intt.elapsed()

      res = (i,native,it)::res

      println(s"${res.head}")
    }

    println(("fib number,native time,interp time"::(res.map(x => s"${x._1},${x._2},${x._3}"))).mkString("\n"))

  }



   test("fibonacci Trace") {

     val fib = fibonacciProg(8)

      val r = interpretTrace(fib)
 
     assert(r._1.nextCmd == Stopped())
     // Show interpreted result
     //
 
   }

   test("fib breakpoints") {

     val fib = fibonacciProg(8)
     val watch = IRWalk.firstInProc((fib.procedures.find(_.name == "fib")).get).get
     val bp = BreakPoint("Fibentry",  BreakPointLoc.CMDCond(watch, BinaryExpr(BVEQ, BitVecLiteral(5, 64), Register("R0", 64))), BreakPointAction(true, true, List(("R0", Register("R0", 64))), true))
     // val bp2 = BreakPoint("Fibentry",  BreakPointLoc.CMD(watch), BreakPointAction(true, false, List(("R0", Register("R0", 64))), true))
     // val interp = LayerInterpreter(NormalInterpreter, RememberBreakpoints(NormalInterpreter, List(bp)))
     // val res = InterpFuns.interpretProg(interp)(fib, (InterpreterState(), List()))
     val res = interpretWithBreakPoints(fib, List(bp), NormalInterpreter, InterpreterState())


   }

}
