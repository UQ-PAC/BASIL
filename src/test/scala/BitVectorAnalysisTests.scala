import analysis.util.*
import bap.*
import org.scalatest.funsuite.AnyFunSuite

import scala.runtime.stdLibPatches.Predef.assert

class BitVectorAnalysisTests extends AnyFunSuite {
//  test("bvadd1") {
//    val result = smt_bvadd(BAPLiteral(5, 8), BAPLiteral(13, 8))
//    assert(result == BAPLiteral(18, 8))
//  }
//  test("bvadd2") {
//    val result = smt_bvadd(BAPLiteral(5, 4), BAPLiteral(13, 4))
//    assert(result == BAPLiteral(2, 4))
//  }
//  test("bvadd3") {
//    val result = smt_bvadd(BAPLiteral(BigInt("4722366482869645213696"), 128),
//      BAPLiteral(BigInt("38685626227668133590597632"), 128))
//    assert(result == BAPLiteral(BigInt("38690348594151003235811328"), 128))
//  }
//  test("bvadd4") {
//    val result = smt_bvadd(BAPLiteral(BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE", 16), 128), BAPLiteral(4, 128))
//    assert(result == BAPLiteral(2, 128))
//  }
//
//  test("concat1") {
//    val result = smt_concat(BAPLiteral(BigInt("AAAABBBBCCCCDDDD", 16), 64), BAPLiteral(BigInt("EEEEFFFF", 16), 32))
//    assert(result == BAPLiteral(BigInt("AAAABBBBCCCCDDDDEEEEFFFF", 16), 96))
//  }
//
//  // test some small stuff, overflow, 1/0, 128-bit
//  // test all signed etc. cases, division edge cases
//  // test extract oddities
//  // test shift oddities

}