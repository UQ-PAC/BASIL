package ir

import analysis.BitVectorEval.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import specification.SpecGlobal
import translating.BAPToIR
import util.{LogLevel, Logger}
import util.IRLoading.{loadBAP, loadReadELF}
import util.ILLoadingConfig

class InterpreterTests extends AnyFunSuite with BeforeAndAfter {

  var i: Interpreter = Interpreter()
  Logger.setLevel(LogLevel.DEBUG)

  def getProgram(name: String): (Program, Set[SpecGlobal]) = {


    val loading = ILLoadingConfig(
      inputFile = s"examples/$name/$name.adt",
      relfFile = s"examples/$name/$name.relf",
      specFile = None,
      dumpIL = None
    )

    val bapProgram = loadBAP(loading.inputFile)
    val (symbols, externalFunctions, globals, _, mainAddress) = loadReadELF(loading.relfFile, loading)
    val IRTranslator = BAPToIR(bapProgram, mainAddress)
    var IRProgram = IRTranslator.translate
    IRProgram = ExternalRemover(externalFunctions.map(e => e.name)).visitProgram(IRProgram)
    IRProgram = Renamer(Set("free")).visitProgram(IRProgram)
    IRProgram.stripUnreachableFunctions()
    val stackIdentification = StackSubstituter()
    stackIdentification.visitProgram(IRProgram)
    IRProgram.setModifies(Map())

    (IRProgram, globals)
  }

  def testInterpret(name: String, expected: Map[String, Int]): Unit = {
    val (program, globals) = getProgram(name)
    val regs = i.interpret(program)

    // Show interpreted result
    Logger.info("Registers:")
    regs.foreach { (key, value) =>
      Logger.info(s"$key := $value")
    }

    Logger.info("Globals:")
    globals.foreach { global =>
      val mem = i.getMemory(global.address.toInt, global.size, Endian.LittleEndian, i.mems)
      Logger.info(s"$global := $mem")
    }

    // Test expected value
    expected.foreach { (name, expected) =>
      globals.find(_.name == name) match {
        case Some(global) =>
          val actual = i.getMemory(global.address.toInt, global.size, Endian.LittleEndian, i.mems).value.toInt
          assert(actual == expected)
        case None => assert("None" == name)
      }
    }
  }

  before {
    i = Interpreter()
  }

  test("getMemory in LittleEndian") {
    i.mems(0) = BitVecLiteral(BigInt("0D", 16), 8)
    i.mems(1) = BitVecLiteral(BigInt("0C", 16), 8)
    i.mems(2) = BitVecLiteral(BigInt("0B", 16), 8)
    i.mems(3) = BitVecLiteral(BigInt("0A", 16), 8)
    val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
    val actual: BitVecLiteral = i.getMemory(0, 32, Endian.LittleEndian, i.mems)
    assert(actual == expected)
  }

  test("getMemory in BigEndian") {
    i.mems(0) = BitVecLiteral(BigInt("0A", 16), 8)
    i.mems(1) = BitVecLiteral(BigInt("0B", 16), 8)
    i.mems(2) = BitVecLiteral(BigInt("0C", 16), 8)
    i.mems(3) = BitVecLiteral(BigInt("0D", 16), 8)
    val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
    val actual: BitVecLiteral = i.getMemory(0, 32, Endian.BigEndian, i.mems)
    assert(actual == expected)
  }

  test("setMemory in LittleEndian") {
    i.mems(0) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(1) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(2) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(3) = BitVecLiteral(BigInt("FF", 16), 8)
    val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
    i.setMemory(0, 32, Endian.LittleEndian, expected, i.mems)
    val actual: BitVecLiteral = i.getMemory(0, 32, Endian.LittleEndian, i.mems)
    assert(actual == expected)
  }

  test("setMemory in BigEndian") {
    i.mems(0) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(1) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(2) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(3) = BitVecLiteral(BigInt("FF", 16), 8)
    val expected: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
    i.setMemory(0, 32, Endian.BigEndian, expected, i.mems)
    val actual: BitVecLiteral = i.getMemory(0, 32, Endian.BigEndian, i.mems)
    assert(actual == expected)
  }

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
}
