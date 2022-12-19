import analysis.util.*
import astnodes.*
import org.scalatest.funsuite.AnyFunSuite

import scala.runtime.stdLibPatches.Predef.assert

class BitVectorAnalysisTests extends AnyFunSuite {
  test("bvadd1") {
    val result = bvadd(Literal(5, 8), Literal(13, 8))
    assert(result == Literal(18, 8))
  }
  test("bvadd2") {
    val result = bvadd(Literal(5, 4), Literal(13, 4))
    assert(result == Literal(2, 4))
  }
  test("bvadd3") {
    val result = bvadd(Literal(BigInt("4722366482869645213696"), 128),
      Literal(BigInt("38685626227668133590597632"), 128))
    assert(result == Literal(BigInt("38690348594151003235811328"), 128))
  }
  test("bvadd4") {
    val result = bvadd(Literal(BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE", 16), 128), Literal(4, 128))
    assert(result == Literal(2, 128))
  }

  test("concat1") {
    val result = concat(Literal(BigInt("AAAABBBBCCCCDDDD", 16), 64), Literal(BigInt("EEEEFFFF", 16), 32))
    assert(result == Literal(BigInt("AAAABBBBCCCCDDDDEEEEFFFF", 16), 96))
  }

  // test some small stuff, overflow, 1/0, 128-bit
  // test all signed etc. cases, division edge cases
  // test extract oddities
  // test shift oddities

}