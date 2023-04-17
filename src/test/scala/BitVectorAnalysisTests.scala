import analysis.util._
import ir._
import org.scalatest.funsuite.AnyFunSuite

import scala.runtime.stdLibPatches.Predef.assert

class BitVectorAnalysisTests extends AnyFunSuite {
  test("bvadd1") {
    val result = smt_bvadd(BitVecLiteral(5, 8), BitVecLiteral(13, 8))
    assert(result == BitVecLiteral(18, 8))
  }
  test("bvadd2") {
    val result = smt_bvadd(BitVecLiteral(5, 4), BitVecLiteral(13, 4))
    assert(result == BitVecLiteral(2, 4))
  }
  test("bvadd3") {
    val result = smt_bvadd(BitVecLiteral(BigInt("4722366482869645213696"), 128),
      BitVecLiteral(BigInt("38685626227668133590597632"), 128))
    assert(result == BitVecLiteral(BigInt("38690348594151003235811328"), 128))
  }
  test("bvadd4") {
    val result = smt_bvadd(BitVecLiteral(BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE", 16), 128), BitVecLiteral(4, 128))
    assert(result == BitVecLiteral(2, 128))
  }

  test("concat1") {
    val result = smt_concat(BitVecLiteral(BigInt("AAAABBBBCCCCDDDD", 16), 64), BitVecLiteral(BigInt("EEEEFFFF", 16), 32))
    assert(result == BitVecLiteral(BigInt("AAAABBBBCCCCDDDDEEEEFFFF", 16), 96))
  }

  // test some small stuff, overflow, 1/0, 128-bit
  // test all signed etc. cases, division edge cases
  // test extract oddities
  // test shift oddities

}