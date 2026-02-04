package ir.transforms

import boogie.*
import ir.cilvisitor.*
import ir.*

class MemoryEncodingTransform(ctx: IRContext) extends CILVisitor {
  // // first m bits of pointer are for offset
  // private val m = 32
  
  // // Helpful variables to have
  // private val r0 = BVariable("R0", BitVecBType(64), Scope.Global)
  // private val r0_gamma = BVariable("Gamma_R0", BoolBType, Scope.Global)
  // private val i = BVariable("i", BitVecBType(64), Scope.Local)
  // private val j = BVariable("j", BitVecBType(64), Scope.Local)
  // private val o = BVariable("o", IntBType, Scope.Local)
  
  // // Counter of allocations for getting a fresh id on new allocation
  // private val me_alloc_counter = BVariable("me_alloc_counter", IntBType, Scope.Global)

  // // Object is a mapping from pointer to allocation id
  // private val me_object = BMapVar("me_object", MapBType(BitVecBType(64), IntBType), Scope.Global)
  // private val me_object_gamma = BMapVar("Gamma_me_object", MapBType(BitVecBType(64), BoolBType), Scope.Global)

  // // Position is a mapping from pointer to its offset
  // private val me_position = BMapVar("me_position", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Global)
  // private val me_position_gamma = BMapVar("Gamma_me_position", MapBType(BitVecBType(64), BoolBType), Scope.Global)

  // // Live is a mapping from allocation id to liveness
  // // 0=Dead, 1=Live, 2=Fresh
  // // if live, find allocation size in me_live_vals
  // private val me_live = BMapVar("me_live", MapBType(IntBType, BitVecBType(8)), Scope.Global)
  // private val me_live_gamma = BMapVar("Gamma_me_live", MapBType(IntBType, BoolBType), Scope.Global)
  // private val me_live_val = BMapVar("me_live_val", MapBType(IntBType, BitVecBType(64)), Scope.Global)
  // private val me_live_val_gamma = BMapVar("Gamma_me_live_val", MapBType(IntBType, BoolBType), Scope.Global)

  // // Mapping for global pointers
  // private val me_global = BMapVar("me_global", MapBType(BitVecBType(64), BoolBType), Scope.Global)

  // private val gamma_mem = BMapVar("Gamma_mem", MapBType(BitVecBType(64), BoolBType), Scope.Global)
  // private val mem = BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)

  // private var global_addresses = ctx.symbols.flatMap(s => Range(s.value.intValue, s.value.intValue + s.size)).toSet

  // private def R(n: Int) = BVariable(s"R$n", BitVecBType(64), Scope.Global)
  // private def Gamma_R(n: Int) = BVariable(s"Gamma_R$n", BoolBType, Scope.Global)

  private def r(n: Int) = Register(s"R$n",64)
  private def gamma_r(n: Int) = GlobalVar(s"Gamma_R$n", BoolType)

  private val me_global = GlobalVar("me_global", MapType(BitVecType(64), BoolType))
  private val me_allocation = GlobalVar("me_allocation", MapType(BitVecType(64), IntType))
  private val me_offset = GlobalVar("me_offset", MapType(BitVecType(64), BitVecType(64)))

  // Liveness/Temporal bound:
  // 0 = fresh, 1 = live, 2 = dead
  private val me_live = GlobalVar("me_live", MapType(IntType, BitVecType(2)))

  // Spatial bound:
  private val me_start = GlobalVar("me_start", MapType(IntType, BitVecType(64)))
  private val me_end = GlobalVar("me_end", MapType(IntType, BitVecType(64)))
  private val me_size = GlobalVar("me_size", MapType(IntType, BitVecType(64)))

  private val mem = SharedMemory("mem", 64, 8)

  private val i = LocalVar("i", BitVecType(64))

  private def implies_else(cond: Expr, true_br: Expr, false_br: Expr) = BinaryExpr(
    BoolAND,
    BinaryExpr(BoolIMPLIES,
      cond,
      true_br
    ),
    BinaryExpr(BoolIMPLIES,
      UnaryExpr(BoolNOT, cond),
      false_br
    )
  )

  private def in_bounds(lower: Expr, upper: Expr, n: Expr) = BinaryExpr(
    BoolAND,
    BinaryExpr(BVULE, lower, n),
    BinaryExpr(BVULT, n, upper)
  )

  private def valid(addr: Expr, n: Expr) = FApplyExpr(
    "valid",
    Seq(
      me_live,
      me_size,
      me_allocation,
      me_global,
      addr,
      n
    ),
    BoolType
  )

  private def addr_to_allocation(addr: Expr) = FApplyExpr(
    "addr_to_allocation",
    Seq(
      me_allocation,
      addr,
    ),
    IntType
  )

  private def addr_to_offset(addr: Expr) = FApplyExpr(
    "addr_to_offset",
    Seq(
      me_offset,
      addr,
    ),
    BitVecType(64)
  )

  private def addr_is_global(addr: Expr) = FApplyExpr(
    "addr_is_global",
    Seq(
      me_global,
      addr
    ),
    BoolType
  )

  private def obj_to_size(obj: Expr) = FApplyExpr(
    "obj_size",
    Seq(
      me_size,
      obj
    ),
    BitVecType(64)
  )

  private def obj_is_fresh(obj: Expr) = FApplyExpr(
    "obj_is_fresh",
    Seq(
      me_live,
      obj,
    ),
    BoolType
  )

  private def obj_is_alive(obj: Expr) = FApplyExpr(
    "obj_is_alive",
    Seq(
      me_live,
      obj,
    ),
    BoolType
  )

  private def obj_is_dead(obj: Expr) = FApplyExpr(
    "obj_is_dead",
    Seq(
      me_live,
      obj,
    ),
    BoolType
  )

  private def read_mem(addr: Expr) = FApplyExpr(
    "read_mem",
    Seq(
      mem,
      addr
    ),
    BitVecType(8)
  )

  private def read_gamma_mem(addr: Expr) = FApplyExpr(
    "read_gama_mem",
    Seq(
      mem,
      addr
    ),
    BoolType
  )

  private def transform_memset(p: Procedure) = {
    // r0: Start address
    // r1: Character
    // r2: Bytes to write
    
    p.modifies ++= Set(
      mem,
    )

    // p.requires = p.requires ++ List(
    //   BValid(me_live, me_live_val, me_object, me_position, me_global, ret, n)
    // )

    p.requiresExpr ++= List(
      valid(r(0), r(2))
    )

    // p.ensures = p.ensures ++ List(
    //   // return value is equal to input destination
    //   ForAll(
    //     List(i.toBoogie),
    //     BinaryBExpr(EQ,
    //       MapAccess(mem.toBoogie,i.toBoogie),
    //       IfThenElse(
    //         BinaryBExpr(BoolAND,
    //           BinaryBExpr(BVULE, r(0).toBoogie, i.toBoogie),
    //           BinaryBExpr(BVULT, i.toBoogie, BinaryBExpr(BVADD, r(0).toBoogie, Old(r(2).toBoogie)))
    //         ),
    //         r(1).toBoogie,
    //         Old(MapAccess(mem.toBoogie, i.toBoogie)),
    //       ),
    //     ),
    //     List(List(MapAccess(mem.toBoogie, i.toBoogie)))
    //   ),
    // )

    p.ensuresExpr ++= List(
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(List(i),
          implies_else(
            in_bounds(
              r(0),
              BinaryExpr(BVADD, r(0), r(2)),
              i
            ),
            BinaryExpr(EQ,
              read_mem(i),
              Extract(8,0,r(1)),
            ),
            BinaryExpr(EQ,
              read_mem(i),
              OldExpr(read_mem(i))
            )
          )
        ),
        triggers = List(List(read_mem(i)))
      )
    )
  }

  private def transform_memcpy(p: Procedure) = {
    // r0: Destination start address
    // r1: Source start address
    // r2: Bytes to copy
    
    // val dest = R(0)
    // val gamma_dest = Gamma_R(0)

    // val src = R(1)
    // val gamma_src = Gamma_R(1)

    // val n = R(2)
    // val gamma_n = Gamma_R(2)
    p.modifies ++= Set(
      mem
    )

    // p.requires = p.requires ++ List(
    //   BinaryBExpr(EQ, gamma_n, TrueBLiteral),
    //   BValid(me_live, me_live_val, me_object, me_position, me_global, src, n),
    //   BValid(me_live, me_live_val, me_object, me_position, me_global, dest, n),
    // )

    p.requires ++= List(
      BinaryBExpr(EQ, r(2).toGamma, TrueBLiteral),
    )

    p.requiresExpr ++= List(
      // BinaryExpr(EQ, gamma_r(2), TrueLiteral),
      valid(r(0), r(2)),
      valid(r(1), r(2)),
    )

    // p.ensures = p.ensures ++ List(
    //   // return value is equal to input... this is a bit silly but its how memcpy is defined
    //   BinaryBExpr(EQ, dest, Old(dest)),
    //   BinaryBExpr(EQ, gamma_dest, Old(gamma_dest)),

    //   ForAll(
    //     List(i),
    //     BinaryBExpr(EQ, MapAccess(mem, i),
    //       IfThenElse(
    //         BinaryBExpr(BoolAND,
    //           BinaryBExpr(BVULE, dest, i),
    //           BinaryBExpr(BVULT, i, BinaryBExpr(BVADD, dest, n))
    //         ),
    //         MapAccess(mem, BinaryBExpr(BVADD, MapAccess(me_position, i), src)),
    //         Old(MapAccess(mem, i))
    //       ),
    //     ),
    //     List(List(MapAccess(mem,i)))
    //   ),

    //   ForAll(
    //     List(i),
    //     BinaryBExpr(EQ, MapAccess(gamma_mem, i),
    //       IfThenElse(
    //         BinaryBExpr(BoolAND,
    //           BinaryBExpr(BVULE, dest, i),
    //           BinaryBExpr(BVULT, i, BinaryBExpr(BVADD, dest, n))
    //         ),
    //         MapAccess(gamma_mem, BinaryBExpr(BVADD, MapAccess(me_position, i), src)),
    //         Old(MapAccess(gamma_mem, i))
    //       ),
    //     ),
    //     List(List(MapAccess(gamma_mem,i)))
    //   ),
    // )

    val cond = in_bounds(r(0), BinaryExpr(BVADD, r(0), r(2)), i)
    p.ensuresExpr ++= List(
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(List(i),
          implies_else(
            cond,
            BinaryExpr(EQ,
              read_mem(i),
              read_mem(BinaryExpr(BVADD, addr_to_offset(i), r(1)))
            ),
            BinaryExpr(EQ,
              read_mem(i),
              OldExpr(read_mem(i))
            )
          )
        ),
        triggers = List(List(read_mem(i)))
      ),
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(List(i),
          implies_else(
            cond,
            BinaryExpr(EQ,
              read_gamma_mem(i),
              read_gamma_mem(BinaryExpr(BVADD, addr_to_offset(i), r(1)))
            ),
            BinaryExpr(EQ,
              read_gamma_mem(i),
              OldExpr(read_gamma_mem(i))
            )
          )
        ),
        triggers = List(List(read_gamma_mem(i)))
      )
    )
  }

  private def transform_strlen(p: Procedure) = {
    p.modifies ++= Set(
      GlobalVar("R0", BitVecType(64)),
    )

    // val dest = BinaryBExpr(BVADD, Old(r0), r0)

    // p.requires = p.requires ++ List(
    //   // First byte is valid, cant reason about full size in precond
    //   BValid(me_live, me_live_val, me_object, me_position, me_global, r0, BitVecBLiteral(1,64)),

    //   // Exists some 0 in the allocation:
    //   Exists(
    //     List(i),
    //     BinaryBExpr(BoolAND,
    //       BinaryBExpr(EQ, MapAccess(me_object, i), MapAccess(me_object, r0)),
    //       BinaryBExpr(EQ, MapAccess(mem, i), BitVecBLiteral(0, 8))
    //     ),
    //   )
    // )

    // p.ensures = p.ensures ++ List(
    //   BinaryBExpr(EQ, r0_gamma, TrueBLiteral),
    //   ForAll(
    //     List(i),
    //     BinaryBExpr(BoolIMPLIES,
    //       BinaryBExpr(BoolAND,
    //         BinaryBExpr(BVULE, Old(r0), i),
    //         BinaryBExpr(BVULT, i, dest)
    //       ),
    //       BinaryBExpr(NEQ, MapAccess(mem, i), BitVecBLiteral(0,8)),
    //     ),
    //   ),
    //   BinaryBExpr(EQ,
    //     MapAccess(me_object, dest),
    //     MapAccess(me_object, Old(r0))
    //   ),
    //   BinaryBExpr(EQ,
    //     MapAccess(mem, dest),
    //     BitVecBLiteral(0,8)
    //   ),
    //   BValid(me_live, me_live_val, me_object, me_position, me_global, Old(r0), r0),
    //   BinaryBExpr(BVULE, Old(r0), dest)
    // )
  }

  private def transform_free(p: Procedure) = {
    p.modifies ++= Set(
      GlobalVar("me_live", MapType(IntType, BitVecType(8))),
    )

    // val obj = MapAccess(me_object, r0)

    // p.requires = p.requires ++ List(
    //   // Pointer must at base of allocation for free (no offset)
    //   BinaryBExpr(EQ, MapAccess(me_position, r0), BitVecBLiteral(0,64)),
    //   // Pointer must be live to free
    //   BinaryBExpr(EQ, MapAccess(me_live, obj), BitVecBLiteral(1,8)),
    //   // The pointer being freed must not be global
    //   UnaryBExpr(BoolNOT, MapAccess(me_global, r0)),
    //   // The pointer being freed must be fully high
    //   ForAll(
    //     List(i),
    //     BinaryBExpr(BoolIMPLIES,
    //       BinaryBExpr(EQ, MapAccess(me_object, i), MapAccess(me_object, r0)),
    //       BinaryBExpr(EQ, MapAccess(gamma_mem, i), TrueBLiteral)
    //     )
    //   )
    // )

    // p.ensures = p.ensures ++ List(
    //   // Sets the old live value to dead
    //   ForAll(
    //     List(o),
    //     BinaryBExpr(EQ,
    //       MapAccess(me_live, o),
    //       IfThenElse(
    //         BinaryBExpr(EQ, o, obj),
    //         BitVecBLiteral(2, 8),
    //         Old(MapAccess(me_live, o)),
    //       )
    //     ),
    //     List(
    //       List(MapAccess(me_live, o))
    //     )
    //   ),
    // )
  }
  
  private def transform_malloc(p: Procedure) = {
    p.modifies ++= Set(
      GlobalVar("me_alloc_counter", IntType),
      GlobalVar("me_live", MapType(IntType, BitVecType(8))),
      GlobalVar("me_live_val", MapType(IntType, BitVecType(64))),
      GlobalVar("me_position", MapType(BitVecType(64), BitVecType(64))),
      GlobalVar("me_object", MapType(BitVecType(64), IntType)),
      GlobalVar("R0", BitVecType(64)),
    )
    
    // p.requires = p.requires ++ List(
    //   // Cant malloc 0 or less bytes
    //   BinaryBExpr(BVUGT, r0, BitVecBLiteral(0, 64)),
    //   BinaryBExpr(EQ, r0_gamma, TrueBLiteral),
    // )

    // p.ensures = p.ensures ++ List(
    //   BinaryBExpr(EQ, r0_gamma, TrueBLiteral),

    //   // Alloc count is bumped up by 1 after every allocation
    //   // me_alloc_counter := Old(me_alloc_counter) + 1
    //   BinaryBExpr(EQ, me_alloc_counter, BinaryBExpr(IntADD, Old(me_alloc_counter), IntBLiteral(BigInt(1)))),

    //   // Ensure that there is not an overflow
    //   BinaryBExpr(BVUGT, BinaryBExpr(BVADD, r0, Old(r0)), r0),

    //   // Updates object mapping for all bytes in our allocated object and keeps others the same
    //   ForAll(
    //     List(i),
    //     BinaryBExpr(EQ,
    //       MapAccess(me_object, i),
    //       IfThenElse(
    //         BinaryBExpr(BoolAND,
    //           BinaryBExpr(BVULE, r0, i),
    //           BinaryBExpr(BVULT, i, BinaryBExpr(BVADD, r0, Old(r0)))
    //         ),
    //         Old(me_alloc_counter),
    //         Old(MapAccess(me_object, i))
    //       ),
    //     ),
    //     List(List(MapAccess(me_object, i)))
    //   ),

    //   // Update position mapping
    //   ForAll(
    //     List(i),
    //     IfThenElse(
    //       BinaryBExpr(EQ,
    //         MapAccess(me_object, r0),
    //         MapAccess(me_object, i)
    //       ),
    //       BinaryBExpr(EQ, MapAccess(me_position, i), BinaryBExpr(BVSUB, i, r0)),
    //       BinaryBExpr(EQ, MapAccess(me_position, i), Old(MapAccess(me_position, i)))
    //     ),
    //     List(List(MapAccess(me_position, i)))
    //   ),

    //   // Guarantee the object is live and has correct liveness
    //   ForAll(
    //     List(o),
    //     IfThenElse(
    //       BinaryBExpr(EQ, o, Old(me_alloc_counter)),
    //       BinaryBExpr(BoolAND,
    //         BinaryBExpr(EQ, MapAccess(me_live, o), BitVecBLiteral(1, 8)),
    //         BinaryBExpr(EQ, MapAccess(me_live_val, o), Old(r0)),
    //       ),
    //       BinaryBExpr(BoolAND,
    //         BinaryBExpr(EQ, MapAccess(me_live, o), Old(MapAccess(me_live, o))),
    //         BinaryBExpr(EQ, MapAccess(me_live_val, o), Old(MapAccess(me_live_val, o))),
    //       )
    //     ),
    //     List(
    //       List(MapAccess(me_live, o)),
    //       List(MapAccess(me_live_val, o))
    //     )
    //   ),

    //   // Ensure that the full allocation of R0 is in fresh memory
    //   // and that it is not overlapping a global var
    //   ForAll(
    //     List(i),
    //     BinaryBExpr(BoolIMPLIES,
    //       BinaryBExpr(EQ, MapAccess(me_object, r0), MapAccess(me_object, i)),
    //       BinaryBExpr(BoolAND,
    //         // They must have previously been fresh in the old state
    //         BinaryBExpr(EQ,
    //           Old(MapAccess(me_live, Old(MapAccess(me_object, i)))),
    //           BitVecBLiteral(2,8)
    //         ),
    //         // And they must not be a global variable
    //         UnaryBExpr(BoolNOT, Old(MapAccess(me_global, i))),
    //       )
    //     ),
    //     List(
    //       List(MapAccess(me_object, i)),
    //       List(MapAccess(me_global, i))
    //     )
    //   ),
    // )
  }
 
  private def transform_main(p: Procedure) = {
    // p.requires = p.requires ++ List(
    //   // Allocation counter starts at 1
    //   BinaryBExpr(EQ, me_alloc_counter, IntBLiteral(1)),

    //   // All objects start fresh ...
    //   ForAll(
    //     List(o),
    //     BinaryBExpr(EQ,
    //       MapAccess(me_live, o),
    //       BitVecBLiteral(2, 8)
    //     ),
    //     List(List(MapAccess(me_live, o)))
    //   ),

    //   // ... with id 0
    //   ForAll(
    //     List(i),
    //     BinaryBExpr(EQ,
    //       MapAccess(me_object, i),
    //       IntBLiteral(0)
    //     ),
    //     List(List(MapAccess(me_object, i)))
    //   ),

    //   ForAll(
    //     List(i),
    //     BinaryBExpr(EQ,
    //       MapAccess(me_global, i),
    //       BinaryBExpr(BVULE, i, BitVecBLiteral(global_addresses.max, 64)),
    //     ),
    //     List(List(MapAccess(me_global, i)))
    //   ),
    // )
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
      case "memset" => transform_memset(p)
      case "strlen" => transform_strlen(p)
      case "memcpy" => transform_memcpy(p)
      case _ => { }
    }

    SkipChildren()
  }
}
