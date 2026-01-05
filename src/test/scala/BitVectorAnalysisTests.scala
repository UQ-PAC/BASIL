import ir.*
import ir.eval.BitVectorEval.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import util.Logger

@test_util.tags.UnitTest
class BitVectorAnalysisTests extends AnyFunSuite with CaptureOutput {

  test("BitVector to Natural - should convert BitVector to natural number") {
    val result = bv2nat(BitVecLiteral(2, 4))
    assert(result == BigInt(2))
  }

  test("Natural to BitVector - should return number when natural number does fit within specified bit size") {
    val result = nat2bv(8, BigInt(255))
    assert(result == BitVecLiteral(BigInt(255), 8))
  }

  test("BitVector Add - should return the sum of two BitVectors") {
    val result = smt_bvadd(BitVecLiteral(5, 8), BitVecLiteral(13, 8))
    assert(result == BitVecLiteral(18, 8))
  }

  test("BitVector Add - should throw exception when the result is negative") {
    intercept[IllegalArgumentException] {
      val result = smt_bvadd(BitVecLiteral(0, 8), BitVecLiteral(-1, 8))
    }
  }

  test("BitVector Add - should return wrap BitVectors when sum is greater size number of bits") {
    val result = smt_bvadd(BitVecLiteral(255, 8), BitVecLiteral(1, 8))
    assert(result == BitVecLiteral(0, 8))
  }

  test("BitVector Add - should calculate the sum of two numbers that have a high bit count") {
    val result = smt_bvadd(BitVecLiteral(BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE", 16), 128), BitVecLiteral(4, 128))
    assert(result == BitVecLiteral(2, 128))
  }

  // smt_bvmul

  test("BitVector Multiply - should return the product of two BitVectors") {
    val result = smt_bvmul(BitVecLiteral(5, 8), BitVecLiteral(10, 8))
    assert(result == BitVecLiteral(50, 8))
  }

  test("BitVector Multiply - should throw exception when the result is negative") {
    intercept[IllegalArgumentException] {
      val result = smt_bvmul(BitVecLiteral(1, 8), BitVecLiteral(-1, 8))
    }
  }

  test("BitVector Multiply - should return wrap BitVectors when sum is greater size number of bits") {
    val result = smt_bvmul(BitVecLiteral(255, 8), BitVecLiteral(2, 8))
    assert(result == BitVecLiteral(254, 8))
  }
  // smt_bvneg

  test("BitVector Negate - should return the twos compliment of value given") {
    val result = smt_bvneg(BitVecLiteral(2, 8))
    assert(result == BitVecLiteral(254, 8))
  }

  test("BitVector Negate - should return 0 when negating 0") {
    val result = smt_bvneg(BitVecLiteral(0, 8))
    assert(result == BitVecLiteral(0, 8))
  }

  // smt_bvsub

  test("BitVector Subtract - should subtract two numbers ") {
    val result = smt_bvsub(BitVecLiteral(2, 8), BitVecLiteral(1, 8))
    assert(result == BitVecLiteral(1, 8))
  }

  test(
    "BitVector Subtract - should return twos complement when the subtracting two numbers results in a negative value"
  ) {
    val result = smt_bvsub(BitVecLiteral(0, 8), BitVecLiteral(1, 8))
    assert(result == BitVecLiteral(255, 8))
  }

  // smt_bvand

  test("BitVector And - should return a bitwise AND on 2 given numbers") {
    val result = smt_bvand(BitVecLiteral(BigInt("0110", 2), 8), BitVecLiteral(BigInt("0101", 2), 8))
    assert(result == BitVecLiteral(BigInt("0100", 2), 8))
  }

  // smt_bvor

  test("BitVector Or - should return a bitwise OR on 2 given numbers") {
    val result = smt_bvor(BitVecLiteral(BigInt("0110", 2), 8), BitVecLiteral(BigInt("0101", 2), 8))
    assert(result == BitVecLiteral(BigInt("0111", 2), 8))
  }

  // smt_bvnot

  test("BitVector not - should return a bitwise not on a given number") {
    val result = smt_bvnot(BitVecLiteral(BigInt("0110", 2), 4))
    assert(result == BitVecLiteral(BigInt("1001", 2), 4))
  }

  // smt_bvudiv

  test("BitVector unsigned divide - should return the division of two unsigned integers") {
    val result = smt_bvudiv(BitVecLiteral(4, 8), BitVecLiteral(2, 8))
    assert(result == BitVecLiteral(2, 8))
  }

  test("BitVector unsigned divide - should handle device by zero") {
    val result = smt_bvudiv(BitVecLiteral(4, 8), BitVecLiteral(0, 8))
    assert(result == BitVecLiteral(255, 8))
  }

  test("BitVector unsigned divide - should return the divisor when result is a real value") {
    val result = smt_bvudiv(BitVecLiteral(3, 8), BitVecLiteral(2, 8))
    assert(result == BitVecLiteral(1, 8))
  }

  // smt_bvxor

  test("BitVector XOR - should return a bitwise XOR on 2 given numbers") {
    val result = smt_bvxor(BitVecLiteral(BigInt("0110", 2), 8), BitVecLiteral(BigInt("0101", 2), 8))
    assert(result == BitVecLiteral(BigInt("0011", 2), 8))
  }

  // smt_bvnand

  test("BitVector NAND - should return a bitwise NAND on 2 given numbers") {
    val result = smt_bvnand(BitVecLiteral(BigInt("0110", 2), 4), BitVecLiteral(BigInt("0101", 2), 4))
    assert(result == BitVecLiteral(BigInt("1011", 2), 4))
  }

  // smt_bvnor

  test("BitVector NOR - should return a bitwise NOR on 2 given numbers") {
    val result = smt_bvnor(BitVecLiteral(BigInt("0110", 2), 4), BitVecLiteral(BigInt("0101", 2), 4))
    assert(result == BitVecLiteral(BigInt("1000", 2), 4))
  }

  // smt_bvxnor

  test("BitVector XNOR - should return a bitwise XNOR on 2 given numbers") {
    val result = smt_bvxnor(BitVecLiteral(BigInt("0110", 2), 4), BitVecLiteral(BigInt("0101", 2), 4))
    assert(result == BitVecLiteral(BigInt("1100", 2), 4))
  }

  // smt_extract

  test("BitVector Extract - should extract bits from a given number") {
    val result = smt_extract(2, 0, BitVecLiteral(BigInt("111011", 2), 6))
    assert(result == BitVecLiteral(BigInt("011", 2), 3))
  }

  // boogie_extract

  test("Boogie Extract - should extract bits from a given number") {
    val result = boogie_extract(2, 0, BitVecLiteral(BigInt("111011", 2), 6))
    assert(result == BitVecLiteral(BigInt("011", 2), 2))
  }

  // smt_zero_extend

  test("BitVector Zero Extend - should zero extend negative value") {
    val result = smt_zero_extend(8, BitVecLiteral(255, 8))
    assert(result == BitVecLiteral(255, 16))
  }

  test("BitVector Zero Extend - should zero extend positive value") {
    val result = smt_zero_extend(8, BitVecLiteral(32, 8))
    assert(result == BitVecLiteral(32, 16))
  }

  // smt_bvcomp
  test("BitVector Compare - should compare two values") {
    val result = smt_bvcomp(BitVecLiteral(255, 8), BitVecLiteral(255, 8))
    assert(result == BitVecLiteral(1, 1))
  }
  // smt_bveq
  test("BitVector Equal - should return true if two BitVectors are equal") {
    val result = smt_bveq(BitVecLiteral(255, 8), BitVecLiteral(255, 8))
    assert(result)
  }
  test("BitVector Equal - should return false if two BitVectors are not equal") {
    val result = smt_bveq(BitVecLiteral(255, 8), BitVecLiteral(254, 8))
    assert(!result)
  }
  // smt_bvneq
  test("BitVector Not Equal - should return false if two BitVectors are equal") {
    val result = smt_bvneq(BitVecLiteral(255, 8), BitVecLiteral(255, 8))
    assert(!result)
  }
  test("BitVector Not Equal - should return true if two BitVectors are not equal") {
    val result = smt_bvneq(BitVecLiteral(255, 8), BitVecLiteral(254, 8))
    assert(result)
  }
  // smt_bvshl
  test("BitVector Shift Left - should shift bits left") {
    val result = smt_bvshl(BitVecLiteral(1, 8), BitVecLiteral(1, 8))
    assert(result == BitVecLiteral(2, 8))
  }
  // smt_bvlshr
  test("BitVector Shift Right - should shift bits right") {
    val result = smt_bvlshr(BitVecLiteral(2, 8), BitVecLiteral(1, 8))
    assert(result == BitVecLiteral(1, 8))
  }
  test("BitVector Shift Right - should zero on overflow") {
    val result = smt_bvlshr(BitVecLiteral(4104967, 22), BitVecLiteral(3664940, 22))
    assert(result == BitVecLiteral(0, 22))
  }
  // isNegative
  test("is Negative - should return true if the most significant bit is 1") {
    val result = isNegative(BitVecLiteral(128, 8))
    assert(result)
  }
  // smt_bvsdiv
  test("BitVector signed divide - should handle dividing a negative number") {
    val result = smt_bvsdiv(BitVecLiteral(254, 8), BitVecLiteral(2, 8))
    assert(result == BitVecLiteral(255, 8))
  }

  // smt_bvurem
  test("BitVector unsigned divide remainder - should return remainder of an unsigned division") {
    val result = smt_bvurem(BitVecLiteral(7, 8), BitVecLiteral(2, 8))
    assert(result == BitVecLiteral(1, 8))
  }

  // smt_bvsrem

  test("BitVector signed divide remainder - should return remainder of an negative signed division") {
    val result = smt_bvsrem(BitVecLiteral(255, 8), BitVecLiteral(2, 8))
    assert(result == BitVecLiteral(255, 8))
  }

  test("BitVector signed divide remainder - should return remainder of an positive signed division") {
    val result = smt_bvsrem(BitVecLiteral(65, 8), BitVecLiteral(2, 8))
    assert(result == BitVecLiteral(1, 8))
  }

  // smt_bvult

  test("BitVector unsigned less then - should return true if first argument is less than second argument") {
    val result = smt_bvult(BitVecLiteral(254, 8), BitVecLiteral(255, 8))
    assert(result)
  }

  test(
    "BitVector unsigned less then - should return false if first argument is greater than or equal to second argument"
  ) {
    val result = smt_bvult(BitVecLiteral(255, 8), BitVecLiteral(254, 8))
    assert(!result)
  }

  // smt_bvule

  test(
    "BitVector unsigned less then or equal to - should return true if first argument is less equal to second argument"
  ) {
    val result = smt_bvule(BitVecLiteral(254, 8), BitVecLiteral(255, 8))
    assert(result)
  }

  test(
    "BitVector unsigned less then or equal to - should return false if first argument is greater than second argument"
  ) {
    val result = smt_bvule(BitVecLiteral(255, 8), BitVecLiteral(254, 8))
    assert(!result)
  }

  // smt_bvugt
  test(
    "BitVector unsinged greater than - should return true if first argument is greater equal to than second argument"
  ) {
    val result = smt_bvugt(BitVecLiteral(255, 8), BitVecLiteral(254, 8))
    assert(result)
  }

  test(
    "BitVector unsinged greater than - should return false if first argument is less than or equal to second argument"
  ) {
    val result = smt_bvugt(BitVecLiteral(254, 8), BitVecLiteral(255, 8))
    assert(!result)
  }

  // smt_bvuge
  test(
    "BitVector unsinged greater than or equal to - should return true if first argument is greater equal or equal to second argument"
  ) {
    val result = smt_bvuge(BitVecLiteral(255, 8), BitVecLiteral(254, 8))
    assert(result)
  }

  test(
    "BitVector unsinged greater than or equal to - should return false if first argument is less than second argument"
  ) {
    val result = smt_bvuge(BitVecLiteral(254, 8), BitVecLiteral(255, 8))
    assert(!result)
  }

  // smt_bvslt
  test("BitVector signed less than - should return true if first argument is less than second argument") {
    val result = smt_bvslt(BitVecLiteral(254, 8), BitVecLiteral(255, 8))
    assert(result)
  }

  test(
    "BitVector signed less than - should return false if first argument is greater than or equal to second argument"
  ) {
    val result = smt_bvslt(BitVecLiteral(254, 8), BitVecLiteral(254, 8))
    assert(!result)
  }

  // smt_bvsle
  test(
    "BitVector signed less than or equal to - should return true if first argument is less than or equal to second argument"
  ) {
    val result = smt_bvsle(BitVecLiteral(254, 8), BitVecLiteral(255, 8))
    assert(result)
  }

  test(
    "BitVector signed less than or equal to - should return false if first argument is greater than second argument"
  ) {
    val result = smt_bvsle(BitVecLiteral(255, 8), BitVecLiteral(254, 8))
    assert(!result)
  }

  // smt_bvsgt
  test("BitVector signed greater than - should return true if first argument is greater than second argument") {
    val result = smt_bvsgt(BitVecLiteral(255, 8), BitVecLiteral(254, 8))
    assert(result)
  }

  test("BitVector signed greater than - should return false if first argument is less than second argument") {
    val result = smt_bvsgt(BitVecLiteral(254, 8), BitVecLiteral(255, 8))
    assert(!result)
  }

  // smt_bvsge
  test(
    "BitVector signed greater than or equal to - should return true if first argument is greater than or equal to second argument"
  ) {
    val result = smt_bvsge(BitVecLiteral(255, 8), BitVecLiteral(254, 8))
    assert(result)
  }

  test(
    "BitVector signed greater than or equal to - should return false if first argument is less than second argument"
  ) {
    val result = smt_bvsge(BitVecLiteral(254, 8), BitVecLiteral(255, 8))
    assert(!result)
  }
  // smt_bvashr
  test("BitVector Arithmetic shift right - should return shift right a positive number") {
    val result = smt_bvashr(BitVecLiteral(2, 8), BitVecLiteral(1, 8))
    assert(result == BitVecLiteral(1, 8))
  }

  test("BitVector Arithmetic shift right - should return shift right a negative number") {
    val result = smt_bvashr(BitVecLiteral(254, 8), BitVecLiteral(1, 8))
    assert(result == BitVecLiteral(255, 8))
  }

  // smt_sign_extend
  test("BitVector signed extend - should return a value with the sign bit extended to new size") {
    val result = smt_sign_extend(8, BitVecLiteral(BigInt("10010110", 2), 8))
    assert(result == BitVecLiteral(BigInt("1111111110010110", 2), 16))
  }

  test("SMT - ConCat - should combine two numbers into a single number") {
    val result =
      smt_concat(BitVecLiteral(BigInt("AAAABBBBCCCCDDDD", 16), 64), BitVecLiteral(BigInt("EEEEFFFF", 16), 32))
    Logger.debug(result.value.toString(16))
    assert(result == BitVecLiteral(BigInt("AAAABBBBCCCCDDDDEEEEFFFF", 16), 96))
  }
  test("SMT - ConCat - should combine two numbers into a single number, with first number being 0") {
    val result = smt_concat(BitVecLiteral(BigInt("0", 16), 64), BitVecLiteral(BigInt("EEEEFFFF", 16), 32))
    Logger.debug(result.value.toString(16))
    assert(result == BitVecLiteral(BigInt("EEEEFFFF", 16), 96))
  }
  test("SMT - ConCat - should combine two numbers into a single number, with last number being 0") {
    val result = smt_concat(BitVecLiteral(BigInt("AAAABBBBCCCCDDDD", 16), 64), BitVecLiteral(BigInt("0", 16), 32))
    Logger.debug(result.value.toString(16))
    assert(result == BitVecLiteral(BigInt("aaaabbbbccccdddd00000000", 16), 96))
  }

}
