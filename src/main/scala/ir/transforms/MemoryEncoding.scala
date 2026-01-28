package ir.transforms

import boogie.*
import ir.cilvisitor.*
import ir.*

class MemoryEncodingTransform(ctx: IRContext) extends CILVisitor {
  // first m bits of pointer are for offset
  private val m = 32
  
  // Helpful variables to have
  private val r0 = BVariable("R0", BitVecBType(64), Scope.Global)
  private val r0_gamma = BVariable("Gamma_R0", BoolBType, Scope.Global)
  private val i = BVariable("i", BitVecBType(64), Scope.Local)
  private val j = BVariable("j", BitVecBType(64), Scope.Local)
  private val o = BVariable("o", IntBType, Scope.Local)
  
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

  // Mapping for global pointers
  private val me_global = BMapVar("me_global", MapBType(BitVecBType(64), BoolBType), Scope.Global)

  private val gamma_mem = BMapVar("Gamma_mem", MapBType(BitVecBType(64), BoolBType), Scope.Global);
  private val mem = BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global);

  private var global_addresses = ctx.symbols.flatMap(s => Range(s.value.intValue, s.value.intValue + s.size)).toSet

  private def transform_free(p: Procedure) = {
    p.modifies ++= Set(
      GlobalVar("me_live", MapType(IntType, BitVecType(8))),
    );

    val obj = MapAccess(me_object, r0);

    p.requires = p.requires ++ List(
      // Pointer must at base of allocation for free (no offset)
      BinaryBExpr(EQ, MapAccess(me_position, r0), BitVecBLiteral(0,64)),
      // Pointer must be live to free
      BinaryBExpr(EQ, MapAccess(me_live, obj), BitVecBLiteral(1,8)),
      // The pointer being freed must not be global
      UnaryBExpr(BoolNOT, MapAccess(me_global, r0))
    )

    p.ensures = p.ensures ++ List(
      // Sets the old live value to dead
      ForAll(
        List(o),
        BinaryBExpr(BoolAND,
          BinaryBExpr(BoolIMPLIES,
            BinaryBExpr(EQ, o, obj),
            BinaryBExpr(EQ, MapAccess(me_live, o), BitVecBLiteral(2, 8)),
          ),
          BinaryBExpr(BoolIMPLIES,
            BinaryBExpr(NEQ, o, obj),
            BinaryBExpr(EQ, MapAccess(me_live, o), Old(MapAccess(me_live, o))),
          )
        ),
        List(
          List(MapAccess(me_live, o))
        )
      ),
    )
  }
  
  private def transform_malloc(p: Procedure) = {
    p.modifies ++= Set(
      GlobalVar("me_alloc_counter", IntType),
      GlobalVar("me_live", MapType(IntType, BitVecType(8))),
      GlobalVar("me_live_val", MapType(IntType, BitVecType(64))),
      GlobalVar("me_position", MapType(BitVecType(64), BitVecType(64))),
      GlobalVar("me_object", MapType(BitVecType(64), IntType)),
      GlobalVar("R0", BitVecType(64)),
      // SharedMemory("mem", 64, 8)
    );
    
    p.requires = p.requires ++ List(
      // Cant malloc 0 or less bytes
      BinaryBExpr(BVUGT, r0, BitVecBLiteral(0, 64)),
      BinaryBExpr(EQ, r0_gamma, TrueBLiteral),
    )

    p.ensures = p.ensures ++ List(
      BinaryBExpr(EQ, r0_gamma, TrueBLiteral),

      // Alloc count is bumped up by 1 after every allocation
      // me_alloc_counter := Old(me_alloc_counter) + 1
      BinaryBExpr(EQ, me_alloc_counter, BinaryBExpr(IntADD, Old(me_alloc_counter), IntBLiteral(BigInt(1)))),

      // Ensure that there is not an overflow
      BinaryBExpr(BVUGT, BinaryBExpr(BVADD, r0, Old(r0)), r0),

      // Updates object mapping for all bytes in our allocated object and keeps others the same
      // forall i: bv64 ::
      //      (r0 <= i /\ i <  r0 + Old(r0)) => object[i] == Old(me_alloc_counter)
      //   /\ (r0 >  i \/ i >= r0 + Old(r0)) => object[i] == Old(object[i])
      ForAll(
        List(i),
        BinaryBExpr(BoolAND,
          BinaryBExpr(BoolIMPLIES,
            BinaryBExpr(BoolAND,
              BinaryBExpr(BVULE, r0, i),
              BinaryBExpr(BVULT, i, BinaryBExpr(BVADD, r0, Old(r0)))
            ),
            BinaryBExpr(EQ, MapAccess(me_object, i), Old(me_alloc_counter)),
          ),
          BinaryBExpr(BoolIMPLIES,
            BinaryBExpr(BoolOR,
              BinaryBExpr(BVUGT, r0, i),
              BinaryBExpr(BVUGE, i, BinaryBExpr(BVADD, r0, Old(r0)))
            ),
            BinaryBExpr(EQ, MapAccess(me_object, i), Old(MapAccess(me_object, i)))
          ),
        ),
        List(List(
          MapAccess(me_object, i),
        ))
      ),

      // Updates position mapping for all bytes in our allocated position and keeps others the same
      // forall i: bv64 ::
      //      (r0 <= i /\ i <  r0 + Old(r0)) => position[i] == i - r0
      //   /\ (r0 >  i \/ i >= r0 + Old(r0)) => position[i] == Old(position[i])
      ForAll(
        List(i),
        BinaryBExpr(BoolAND,
          BinaryBExpr(BoolIMPLIES,
            BinaryBExpr(EQ,
              MapAccess(me_object, r0),
              MapAccess(me_object, i)
            ),
            BinaryBExpr(EQ, MapAccess(me_position, i), BinaryBExpr(BVSUB, i, r0))
          ),
          BinaryBExpr(BoolIMPLIES,
            BinaryBExpr(NEQ,
              MapAccess(me_object, r0),
              MapAccess(me_object, i)
            ),
            BinaryBExpr(EQ, MapAccess(me_position, i), Old(MapAccess(me_position, i)))
          )
        ),
        List(List(
          MapAccess(me_position, i),
        ))
      ),

      // Guarantee the object is live and has correct liveness
      // live = old(live)( Old(me_alloc_counter) -= 1)
      // live = old(live_val)( Old(me_alloc_counter) == Old(r0))
      ForAll(
        List(o),
        BinaryBExpr(BoolAND,
          BinaryBExpr(BoolIMPLIES,
            BinaryBExpr(EQ, o, Old(me_alloc_counter)),
            BinaryBExpr(BoolAND,
              // Make live
              BinaryBExpr(EQ, MapAccess(me_live, o), BitVecBLiteral(1, 8)),
              // Update live allocation size
              BinaryBExpr(EQ, MapAccess(me_live_val, o), Old(r0)),
            )
          ),
          BinaryBExpr(BoolIMPLIES,
            BinaryBExpr(NEQ, o, Old(me_alloc_counter)),
            BinaryBExpr(BoolAND,
              BinaryBExpr(EQ, MapAccess(me_live, o), Old(MapAccess(me_live, o))),
              BinaryBExpr(EQ, MapAccess(me_live_val, o), Old(MapAccess(me_live_val, o))),
            )
          ),
        ),
        List(
          List(MapAccess(me_live, o)),
          List(MapAccess(me_live_val, o))
        )
      ),

      // Ensure that the full allocation of R0 is in fresh memory
      // and that it is not overlapping a global var
      ForAll(
        List(i),
        BinaryBExpr(BoolIMPLIES,
          // For all pointers which in the new state have the same object as r0 (aka belong to this allocation)
          BinaryBExpr(EQ, MapAccess(me_object, r0), MapAccess(me_object, i)),
          BinaryBExpr(BoolAND,
            // They must have previously been fresh in the old state
            BinaryBExpr(EQ, Old(MapAccess(me_live, Old(MapAccess(me_object, i)))), BitVecBLiteral(2,8)),
            // And they must not be a global variable
            UnaryBExpr(BoolNOT, Old(MapAccess(me_global, i))),
          )
        ),
        List(
          List(MapAccess(me_object, i)),
          List(MapAccess(me_global, i))
        )
      ),
    )
  }
 
  private def transform_main(p: Procedure) = {
    p.requires = p.requires ++ List(
      // Allocation counter starts at 1
      BinaryBExpr(EQ, me_alloc_counter, IntBLiteral(1)),

      // All objects start fresh ...
      ForAll(
        List(o),
        BinaryBExpr(EQ,
          MapAccess(me_live, o),
          BitVecBLiteral(2, 8)
        ),
        List(List(MapAccess(me_live, o)))
      ),

      // ... with id 0
      ForAll(
        List(i),
        BinaryBExpr(EQ,
          MapAccess(me_object, i),
          IntBLiteral(0)
        ),
        List(List(MapAccess(me_object, i)))
      ),

      ForAll(
        List(i),
        BinaryBExpr(EQ,
          MapAccess(me_global, i),
          BinaryBExpr(BVULE, i, BitVecBLiteral(global_addresses.max, 64)),
        ),
        List(List(MapAccess(me_global, i)))
      ),

      // And have a true gamma for all malloc candidates initially
      ForAll(
        List(i),
        BinaryBExpr(BoolIMPLIES,
          UnaryBExpr(BoolNOT, MapAccess(me_global, i)),
          BinaryBExpr(EQ, MapAccess(gamma_mem, i), TrueBLiteral)
        ),
        List(List(MapAccess(gamma_mem, i)))
      ),

      // // TODO: replace me_unallocated with a set to clean this all up
      // ForAll(
      //   List(i),
      //   BinaryBExpr(BoolIMPLIES,
      //     unallocated_addresses.map(v => {
      //       BinaryBExpr(NEQ, i, BitVecBLiteral(v, 64))
      //     }).fold(TrueBLiteral)((acc, v) => {
      //       BinaryBExpr(BoolAND, acc, v)
      //     }),
      //     BinaryBExpr(EQ,
      //       MapAccess(me_unallocated, i),
      //       FalseBLiteral
      //     )
      //   ),
      //   List(List(MapAccess(me_unallocated, i)))
      // )
    )
    // ++ unallocated_addresses.map(v => {
    //   // All ctx provided symbols are unallocated (true) otherwise false
    //   BinaryBExpr(EQ,
    //     MapAccess(me_unallocated, BitVecBLiteral(v, 64)),
    //     TrueBLiteral
    //   )
    // })
  }

  override def vprog(p: Program) = {
    // TODO: datatypes would clean liveness up a bit in future. Something like this:
    // https://github.com/boogie-org/boogie/blob/master/Test/datatypes/is-cons.bpl
   
    DoChildren()
  }

  override def vproc(p: Procedure) = {
    p.procName match {
      case "malloc" => transform_malloc(p)
      case "free" => transform_free(p)
      case "main" => transform_main(p)
      case _ => { }
    }

    SkipChildren()
  }
}
