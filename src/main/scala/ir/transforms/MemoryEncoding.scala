package ir.transforms

import boogie.*
import ir.cilvisitor.*
import ir.*

class MemoryEncodingTransform() extends CILVisitor {
  // first m bits of pointer are for offset
  private val m = 32
  
  // Helpful variables to have
  private val r0 = BVariable("R0", BitVecBType(64), Scope.Global)
  private val r0_gamma = BVariable("Gamma_R0", BitVecBType(64), Scope.Global)
  private val offset = BVariable("offset", BitVecBType(64), Scope.Local)
  
  // Counter of allocations for getting a fresh id on new allocation
  private val me_alloc_counter = BVariable("me_alloc_counter", IntBType, Scope.Global)

  // Object is a mapping from pointer to allocation id
  private val me_object = BMapVar("me_object", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
  private val me_object_gamma = BMapVar("Gamma_me_object", MapBType(BitVecBType(64), BoolBType), Scope.Global)

  // Position is a mapping from pointer to its offset
  private val me_position = BMapVar("me_position", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
  private val me_position_gamma = VMapVar("Gamma_me_position", MapBType(BitVecBType(64), BoolBType), Scope.Global)
  
  private def transform_malloc(p: Procedure) = {
    p.requires = p.requires ++ List(
      // Cant malloc 0 or less bytes
      BinaryBExpr(BVUGT, r0, BitVecBLiteral(0, 64)),
      BinaryBExpr(EQ, r0_gamma, TrueBLiteral)
    )

    p.ensures = p.ensures ++ List(
      // Alloc count is bumped up by 1 after every allocation
      BinaryBExpr(EQ, me_alloc_counter, BinaryBExpr(IntADD, Old(me_alloc_counter), IntBLiteral(BigInt(1)))),

      // forall offset: bv64 :: (0 <= offset < 2^(64-M) /\ offset < R0) => me_object[offset] = me_alloc_counter
      ForAll(
        List(offset),
        BinaryBExpr(BoolIMPLIES,
          BinaryBExpr(BoolAND,
            BinaryBExpr(BoolAND,
              BinaryBExpr(BVULE, BitVecBLiteral(0,64), offset),
              BinaryBExpr(BVULT, offset, r0)
            ),
            BinaryBExpr(BVULT, offset, BitVecBLiteral(scala.math.BigInt(2).pow(64-m), 64))
          ),
          BinaryBExpr(EQ,
            MapAccess(me_object, offset),
            Old(MapAccess(me_object, offset))
          )
        )
      )
    )
  }

  override def vproc(p: Procedure) = {
    p.procName match {
      case "malloc" => transform_malloc(p)
      case _ => { }
    }

    SkipChildren()
  }
}
