package ir.transforms

import boogie.*
import ir.cilvisitor.*
import ir.*

class MemoryEncodingTransform() extends CILVisitor {
  // first m bits of pointer are for offset
  private val m = 32
  
  // Helpful variables to have
  private val r0 = BVariable("R0", BitVecBType(64), Scope.Global)
  private val r0_gamma = BVariable("Gamma_R0", BoolBType, Scope.Global)
  private val offset = BVariable("offset", BitVecBType(64), Scope.Local)
  
  // Counter of allocations for getting a fresh id on new allocation
  private val me_alloc_counter = BVariable("me_alloc_counter", IntBType, Scope.Global)

  // Object is a mapping from pointer to allocation id
  private val me_object = BMapVar("me_object", MapBType(BitVecBType(64), IntBType), Scope.Global)
  private val me_object_gamma = BMapVar("Gamma_me_object", MapBType(BitVecBType(64), BoolBType), Scope.Global)

  // Position is a mapping from pointer to its offset
  private val me_position = BMapVar("me_position", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Global)
  private val me_position_gamma = BMapVar("Gamma_me_position", MapBType(BitVecBType(64), BoolBType), Scope.Global)

  // Live is a mapping from allocation id to liveness
  // 0=Dead, 1=Live, 2=Fresh
  // if live, find allocation size in me_live_vals
  private val me_live = BMapVar("me_live", MapBType(IntBType, BitVecBType(8)), Scope.Global)
  private val me_live_gamma = BMapVar("Gamma_me_live", MapBType(IntBType, BoolBType), Scope.Global)
  private val me_live_val = BMapVar("me_live_val", MapBType(IntBType, BitVecBType(64)), Scope.Global)
  private val me_live_val_gamma = BMapVar("Gamma_me_live_val", MapBType(IntBType, BoolBType), Scope.Global)
  
  private def transform_malloc(p: Procedure) = {
    p.requires = p.requires ++ List(
      // Cant malloc 0 or less bytes
      BinaryBExpr(BVUGT, r0, BitVecBLiteral(0, 64)),
      BinaryBExpr(EQ, r0_gamma, TrueBLiteral)
    )

    p.ensures = p.ensures ++ List(
      // Alloc count is bumped up by 1 after every allocation
      BinaryBExpr(EQ, me_alloc_counter, BinaryBExpr(IntADD, Old(me_alloc_counter), IntBLiteral(BigInt(1)))),

      // Updates object mapping for all bytes in our allocated object
      // forall offset: bv64 :: (0 <= offset < 2^(64-M) /\ offset < R0) => me_object[Old(R0) + offset] = Old(me_alloc_counter)
      ForAll(
        List(offset),
        BinaryBExpr(BoolIMPLIES,
          BinaryBExpr(BoolAND,
            BinaryBExpr(BoolAND,
              BinaryBExpr(BVULE, BitVecBLiteral(0,64), offset),
              BinaryBExpr(BVULT, offset, Old(r0))
            ),
            BinaryBExpr(BVULT, offset, BitVecBLiteral(scala.math.BigInt(2).pow(64-m), 64))
          ),
          BinaryBExpr(EQ,
            MapAccess(me_object, BinaryBExpr(BVADD, r0, offset)),
            Old(me_alloc_counter)
          )
        )
      ),

      // Updates position mapping for all bytes in our allocated position
      // forall offset: bv64 :: (0 <= offset < 2^(64-M) /\ offset < R0) => me_position[Old(R0) + offset] = offset
      ForAll(
        List(offset),
        BinaryBExpr(BoolIMPLIES,
          BinaryBExpr(BoolAND,
            BinaryBExpr(BoolAND,
              BinaryBExpr(BVULE, BitVecBLiteral(0,64), offset),
              BinaryBExpr(BVULT, offset, Old(r0))
            ),
            BinaryBExpr(BVULT, offset, BitVecBLiteral(scala.math.BigInt(2).pow(64-m), 64))
          ),
          BinaryBExpr(EQ,
            MapAccess(me_position, BinaryBExpr(BVADD, r0, offset)),
            offset
          )
        )
      ),

      // Ensures the object was fresh in the old live mapping
      BinaryBExpr(EQ,
        Old(MapAccess(me_live, Old(me_alloc_counter))),
        BitVecBLiteral(2,8)
      ),

      // Immediately make it Live now that its allocated
      BinaryBExpr(EQ,
        MapAccess(me_live, Old(me_alloc_counter)),
        BitVecBLiteral(1,8)
      ),

      // And give it the associated allocation size
      BinaryBExpr(EQ,
        MapAccess(me_live_val, Old(me_alloc_counter)),
        Old(r0)
      )
    )
  }

    // valid function:
      // BinaryBExpr(EQ, BValid(me_live, me_live_val, me_object, me_position, BitVecBLiteral(0,64), BitVecBLiteral(0,64)), TrueBLiteral)
    
  override def vprog(p: Program) = {
    // TODO: datatype theory would clean this up a bit in future. Something like this:
    // https://github.com/boogie-org/boogie/blob/master/Test/datatypes/is-cons.bpl

   
    DoChildren()
  }

  override def vproc(p: Procedure) = {
    p.procName match {
      case "malloc" => transform_malloc(p)
      case _ => { }
    }

    SkipChildren()
  }
}
