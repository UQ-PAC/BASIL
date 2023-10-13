package ir

import analysis.BitVectorEval.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfter
import bap.BAPProgram
import specification.SpecGlobal
import translating.BAPToIR
import util.Logger
import util.RunUtils.{loadBAP, loadReadELF}

import scala.collection.mutable

class InterpreterTests extends AnyFunSuite with BeforeAndAfter {

  var i: Interpreter = Interpreter()

  def getProgram(name: String): (Program, Set[SpecGlobal]) = {
    val bapProgram = loadBAP(s"examples/$name/$name.adt")
    val (externalFunctions, globals, globalOffsets, mainAddress) = loadReadELF(s"examples/$name/$name.relf")

    val IRTranslator = BAPToIR(bapProgram, mainAddress)
    var IRProgram = IRTranslator.translate
    IRProgram = ExternalRemover(externalFunctions.map(e => e.name)).visitProgram(IRProgram)
    IRProgram = Renamer(Set("free")).visitProgram(IRProgram)
    IRProgram.stripUnreachableFunctions()
    IRProgram.stackIdentification()
    IRProgram.setModifies()

    (IRProgram, globals)
  }

  def runInterpret(name: String): Unit = {
    val (program, globals) = getProgram(name)
    val i = Interpreter()
    val regs = i.interpret(program)
    regs.foreach { (key, value) =>
      Logger.info(s"$key := $value")
    }
    globals.foreach { global =>
      val mem = i.getMemory(global.address.toInt, global.size, Endian.LittleEndian, i.mems)
      Logger.info(s"$global := $mem")
    }
  }

  before {
    i = Interpreter()
  }

  test("basic_assign_assign") {
    runInterpret("basic_assign_assign")
  }

  test("basicassign") {
    runInterpret("basicassign")
  }

  test("function") {
    runInterpret("function")
  }

  test("iflocal") {
    runInterpret("iflocal")
  }

  test("ifglobal") {
    runInterpret("ifglobal")
  }

  test("array") {
    runInterpret("ifglobal")
  }

  test("nestedif") {
    runInterpret("nestedif")
  }

  test("nestedifglobal") {
    runInterpret("nestedifglobal")
  }

  test("simple_jump") {
    runInterpret("simple_jump")
  }

  test("getMemory in LittleEndian") {
    i.mems(0) = BitVecLiteral(BigInt("0D", 16), 8)
    i.mems(1) = BitVecLiteral(BigInt("0C", 16), 8)
    i.mems(2) = BitVecLiteral(BigInt("0B", 16), 8)
    i.mems(3) = BitVecLiteral(BigInt("0A", 16), 8)
    val expect: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
    val actual: BitVecLiteral = i.getMemory(0, 32, Endian.LittleEndian, i.mems)
    assert(actual == expect)
  }

  test("getMemory in BigEndian") {
    i.mems(0) = BitVecLiteral(BigInt("0A", 16), 8)
    i.mems(1) = BitVecLiteral(BigInt("0B", 16), 8)
    i.mems(2) = BitVecLiteral(BigInt("0C", 16), 8)
    i.mems(3) = BitVecLiteral(BigInt("0D", 16), 8)
    val expect: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
    val actual: BitVecLiteral = i.getMemory(0, 32, Endian.BigEndian, i.mems)
    assert(actual == expect)
  }

  test("setMemory in LittleEndian") {
    i.mems(0) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(1) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(2) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(3) = BitVecLiteral(BigInt("FF", 16), 8)
    val expect: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
    i.setMemory(0, 32, Endian.LittleEndian, expect, i.mems)
    val actual: BitVecLiteral = i.getMemory(0, 32, Endian.LittleEndian, i.mems)
    assert(actual == expect)
  }

  test("setMemory in BigEndian") {
    i.mems(0) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(1) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(2) = BitVecLiteral(BigInt("FF", 16), 8)
    i.mems(3) = BitVecLiteral(BigInt("FF", 16), 8)
    val expect: BitVecLiteral = BitVecLiteral(BigInt("0A0B0C0D", 16), 32)
    i.setMemory(0, 32, Endian.BigEndian, expect, i.mems)
    val actual: BitVecLiteral = i.getMemory(0, 32, Endian.BigEndian, i.mems)
    assert(actual == expect)
  }
}
