package ir.transforms.memoryEncoding

import boogie.*
import ir.cilvisitor.*
import ir.*

private def r(n: Int) = Register(s"R$n", 64).toBoogie
private def gamma_r(n: Int) = Register(s"R$n", 64).toGamma

// // private val me_object_counter = GlobalVar("me_object_counter", IntType)

// // private val me_region = GlobalVar("me_region", MapType(BitVecType(64), IntType))
// // private val me_object = GlobalVar("me_object", MapType(BitVecType(64), IntType))
// // private val me_offset = GlobalVar("me_offset", MapType(BitVecType(64), BitVecType(64)))

// // Liveness/Temporal bound:
// // 0 = fresh, 1 = live, 2 = dead
// private val me_liveness = GlobalVar("me_liveness", MapType(IntType, BitVecType(2)))
// private val me_live = GlobalVar("me_live", MapType(IntType, BitVecType(2)))

// // Spatial bound:
// private val me_start = GlobalVar("me_start", MapType(IntType, BitVecType(64))).toBoogie
// private val me_end = GlobalVar("me_end", MapType(IntType, BitVecType(64))).toBoogie
// private val me_size = GlobalVar("me_size", MapType(IntType, BitVecType(64))).toBoogie

// private val mem = SharedMemory("mem", 64, 8).toBoogie

// private val i = LocalVar("i", BitVecType(64)).toBoogie
// private val o = LocalVar("o", IntType).toBoogie

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

private def R(n: Int) = BVariable(s"R$n", BitVecBType(64), Scope.Global);
private def Gamma_R(n: Int) = BVariable(s"Gamma_R$n", BoolBType, Scope.Global);
private def valid(addr: BExpr, n: BExpr) =
  BFunctionCall("valid", List(me_live, me_live_val, me_object, me_position, me_global, addr, n), BoolBType)

private def eq_if_then_else(eq: BExpr, cond: BExpr, true_br: BExpr, false_br: BExpr) =
  BinaryBExpr(EQ, eq, IfThenElse(cond, true_br, false_br))

private def update_if(eq: BExpr, cond: BExpr, new_val: BExpr) = eq_if_then_else(eq, cond, new_val, Old(eq))

private def in_lb_ub(i: BExpr, lb: BExpr, ub: BExpr) =
  BinaryBExpr(BoolAND, BinaryBExpr(BVULE, lb, i), BinaryBExpr(BVULT, i, ub))

private def within_n(i: BExpr, lb: BExpr, offset: BExpr) =
  BinaryBExpr(BoolAND, BinaryBExpr(BVULE, lb, i), BinaryBExpr(BVULT, i, BinaryBExpr(BVADD, lb, offset)))

private def same_obj(obj: BMapVar, i: BExpr, j: BExpr) = BinaryBExpr(EQ, MapAccess(obj, i), MapAccess(obj, j))

private def mem_eq(map: BMapVar, i: BExpr, j: Int) = BinaryBExpr(EQ, MapAccess(map, i), BitVecBLiteral(j, 8))
private def mem_neq(map: BMapVar, i: BExpr, j: Int) = BinaryBExpr(NEQ, MapAccess(map, i), BitVecBLiteral(j, 8))

class MemoryEncodingTransform(ctx: IRContext) extends CILVisitor {
  private var global_addresses = ctx.symbols.flatMap(s => Range(s.value.intValue, s.value.intValue + s.size)).toSet

  private def transform_memset(p: Procedure) = {
    // r0: Destination to write to
    // r1: character to set
    // r2: number of chars to set

    val c = BVExtract(8, 0, R(1));
    val gamma_c = Gamma_R(1);
    val n = R(2);

    p.modifies ++= Set(SharedMemory("mem", 64, 8));

    p.requires ++= List(valid(r(0), n));

    p.ensures ++= List(
      ForAll(List(i), update_if(MapAccess(mem, i), within_n(i, Old(r(0)), Old(n)), c), List(List(MapAccess(mem, i)))),
      ForAll(
        List(i),
        update_if(MapAccess(gamma_mem, i), within_n(i, Old(r(0)), Old(n)), gamma_c),
        List(List(MapAccess(gamma_mem, i)))
      )
    );
  }

  private def transform_memcpy(p: Procedure) = {
    // r0: Destination to copy to
    // r1: Source to copy from
    // r2: number of chars to copy

    val dest = R(0);
    val gamma_dest = Gamma_R(0);

    val src = R(1);
    val gamma_src = Gamma_R(1);

    val n = R(2);
    val gamma_n = Gamma_R(2);

    p.modifies ++= Set(SharedMemory("mem", 64, 8));

    p.requires ++= List(BinaryBExpr(EQ, gamma_n, TrueBLiteral), valid(src, n), valid(dest, n));

    p.ensures ++= List(
      ForAll(
        List(i),
        update_if(
          MapAccess(mem, i),
          within_n(i, dest, n),
          MapAccess(mem, BinaryBExpr(BVADD, MapAccess(me_position, i), src))
        ),
        List(List(MapAccess(mem, i)))
      ),
      ForAll(
        List(i),
        update_if(
          MapAccess(gamma_mem, i),
          within_n(i, dest, n),
          MapAccess(gamma_mem, BinaryBExpr(BVADD, MapAccess(me_position, i), src))
        ),
        List(List(MapAccess(gamma_mem, i)))
      )
    );
  }

  private def transform_strlen(p: Procedure) = {
    p.modifies ++= Set(GlobalVar("R0", BitVecType(64)));

    val dest = BinaryBExpr(BVADD, Old(r(0)), r(0));

    p.requires = p.requires ++ List(
      // First byte is valid, cant reason about full size in precond
      valid(r(0), BitVecBLiteral(1, 64)),
      // Exists some 0 in the allocation:
      Exists(List(i), BinaryBExpr(BoolAND, same_obj(me_object, i, r(0)), mem_eq(mem, i, 0)))
    );

    p.ensures = p.ensures ++ List(
      BinaryBExpr(EQ, gamma_r(0), TrueBLiteral),
      ForAll(List(i), BinaryBExpr(BoolIMPLIES, within_n(i, Old(r(0)), r(0)), mem_neq(mem, i, 0))),
      same_obj(me_object, dest, Old(r(0))),
      mem_eq(mem, dest, 0),
      valid(Old(r(0)), r(0)),
      BinaryBExpr(BVULE, Old(r(0)), dest)
    );
  }

  private def transform_free(p: Procedure) = {
    p.modifies ++= Set(GlobalVar("me_live", MapType(IntType, BitVecType(8))));

    val obj = MapAccess(me_object, r(0));

    p.requires = p.requires ++ List(
      // Pointer must at base of allocation for free (no offset)
      BinaryBExpr(EQ, MapAccess(me_position, r(0)), BitVecBLiteral(0, 64)),
      // Pointer must be live to free
      BinaryBExpr(EQ, MapAccess(me_live, obj), BitVecBLiteral(1, 8)),
      // The pointer being freed must not be global
      UnaryBExpr(BoolNOT, MapAccess(me_global, r(0))),
      // The pointer being freed must be fully high
      ForAll(
        List(i),
        BinaryBExpr(
          BoolIMPLIES,
          BinaryBExpr(EQ, MapAccess(me_object, i), MapAccess(me_object, r(0))),
          BinaryBExpr(EQ, MapAccess(gamma_mem, i), TrueBLiteral)
        )
      )
    )

    p.ensures = p.ensures ++ List(
      // Sets the old live value to dead
      ForAll(
        List(o),
        BinaryBExpr(
          EQ,
          MapAccess(me_live, o),
          IfThenElse(BinaryBExpr(EQ, o, obj), BitVecBLiteral(2, 8), Old(MapAccess(me_live, o)))
        ),
        List(List(MapAccess(me_live, o)))
      )
    )
  }

  private def transform_malloc(p: Procedure) = {
    p.modifies ++= Set(
      GlobalVar("me_alloc_counter", IntType),
      GlobalVar("me_live", MapType(IntType, BitVecType(8))),
      GlobalVar("me_live_val", MapType(IntType, BitVecType(64))),
      GlobalVar("me_position", MapType(BitVecType(64), BitVecType(64))),
      GlobalVar("me_object", MapType(BitVecType(64), IntType)),
      GlobalVar("R0", BitVecType(64))
    );

    p.requires = p.requires ++ List(
      // Cant malloc 0 or less bytes
      BinaryBExpr(BVUGT, r(0), BitVecBLiteral(0, 64)),
      BinaryBExpr(EQ, gamma_r(0), TrueBLiteral)
    )

    p.ensures = p.ensures ++ List(
      BinaryBExpr(EQ, gamma_r(0), TrueBLiteral),

      // Alloc count is bumped up by 1 after every allocation
      // me_alloc_counter := Old(me_alloc_counter) + 1
      BinaryBExpr(EQ, me_alloc_counter, BinaryBExpr(IntADD, Old(me_alloc_counter), IntBLiteral(BigInt(1)))),

      // Ensure that there is not an overflow
      BinaryBExpr(BVUGT, BinaryBExpr(BVADD, r(0), Old(r(0))), r(0)),

      // Updates object mapping for all bytes in our allocated object and keeps others the same
      ForAll(
        List(i),
        update_if(MapAccess(me_object, i), within_n(i, r(0), Old(r(0))), Old(me_alloc_counter)),
        List(List(MapAccess(me_object, i)))
      ),

      // Update position mapping
      ForAll(
        List(i),
        update_if(MapAccess(me_position, i), same_object(me_object, r(0), i), BinaryBExpr(BVSUB, i, r(0))),
        List(List(MapAccess(me_position, i)))
      ),

      // Guarantee the object is live and has correct liveness
      ForAll(
        List(o),
        IfThenElse(
          BinaryBExpr(EQ, o, Old(me_alloc_counter)),
          BinaryBExpr(
            BoolAND,
            BinaryBExpr(EQ, MapAccess(me_live, o), BitVecBLiteral(1, 8)),
            BinaryBExpr(EQ, MapAccess(me_live_val, o), Old(r(0)))
          ),
          BinaryBExpr(
            BoolAND,
            BinaryBExpr(EQ, MapAccess(me_live, o), Old(MapAccess(me_live, o))),
            BinaryBExpr(EQ, MapAccess(me_live_val, o), Old(MapAccess(me_live_val, o)))
          )
        ),
        List(List(MapAccess(me_live, o)), List(MapAccess(me_live_val, o)))
      ),

      // Ensure that the full allocation of R0 is in fresh memory
      // and that it is not overlapping a global var
      ForAll(
        List(i),
        BinaryBExpr(
          BoolIMPLIES,
          BinaryBExpr(EQ, MapAccess(me_object, r(0)), MapAccess(me_object, i)),
          BinaryBExpr(
            BoolAND,
            // They must have previously been fresh in the old state
            BinaryBExpr(EQ, Old(MapAccess(me_live, Old(MapAccess(me_object, i)))), BitVecBLiteral(2, 8)),
            // And they must not be a global variable
            UnaryBExpr(BoolNOT, Old(MapAccess(me_global, i)))
          )
        ),
        List(List(MapAccess(me_object, i)), List(MapAccess(me_global, i)))
      )
    )
  }

  private def transform_main(p: Procedure) = {
    p.requires = p.requires ++ List(
      // Allocation counter starts at 1
      BinaryBExpr(EQ, me_alloc_counter, IntBLiteral(1)),

      // All objects start fresh ...
      ForAll(List(o), BinaryBExpr(EQ, MapAccess(me_live, o), BitVecBLiteral(2, 8)), List(List(MapAccess(me_live, o)))),

      // ... with id 0
      ForAll(List(i), BinaryBExpr(EQ, MapAccess(me_object, i), IntBLiteral(0)), List(List(MapAccess(me_object, i)))),
      ForAll(
        List(i),
        BinaryBExpr(EQ, MapAccess(me_global, i), BinaryBExpr(BVULE, i, BitVecBLiteral(global_addresses.max, 64))),
        List(List(MapAccess(me_global, i)))
      )
    )
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
      case _ => {}
    }

    SkipChildren()
  }
}

// package ir.transforms.memoryEncoding

// import boogie.*
// import ir.cilvisitor.*
// import ir.*

// private def r(n: Int) = Register(s"R$n", 64)

// // object counter:
// private val me_object_counter = GlobalVar("me_object_counter", IntType)

// private val me_region = GlobalVar("me_region", MapType(BitVecType(64), IntType))
// private val me_object = GlobalVar("me_object", MapType(BitVecType(64), IntType))
// private val me_offset = GlobalVar("me_offset", MapType(BitVecType(64), BitVecType(64)))

// // Liveness/Temporal bound:
// // 0 = fresh, 1 = live, 2 = dead
// private val me_liveness = GlobalVar("me_liveness", MapType(IntType, BitVecType(2)))
// private val me_live = GlobalVar("me_live", MapType(IntType, BitVecType(2)))

// // Spatial bound:
// private val me_start = GlobalVar("me_start", MapType(IntType, BitVecType(64)))
// private val me_end = GlobalVar("me_end", MapType(IntType, BitVecType(64)))
// private val me_size = GlobalVar("me_size", MapType(IntType, BitVecType(64)))

// private val mem = SharedMemory("mem", 64, 8)

// private val i = LocalVar("i", BitVecType(64))
// private val o = LocalVar("o", IntType)

// private def implies_else(cond: Expr, true_br: Expr, false_br: Expr) = BinaryExpr(
//   BoolAND,
//   BinaryExpr(BoolIMPLIES, cond, true_br),
//   BinaryExpr(BoolIMPLIES, UnaryExpr(BoolNOT, cond), false_br)
// )

// private def implies(cond: Expr, true_br: Expr) = BinaryExpr(BoolIMPLIES, cond, true_br)

// private def same_object(p1: Expr, p2: Expr) =
//   BinaryExpr(EQ, addr_to_obj(p1), addr_to_obj(p2))

// private def in_bounds(lower: Expr, upper: Expr, n: Expr) =
//   BinaryExpr(BoolAND, BinaryExpr(BVULE, lower, n), BinaryExpr(BVULT, n, upper))

// // Check if an address + size is a valid region for access
// private def valid(addr: Expr, n: Expr) =
//   FApplyExpr("valid", Seq(me_liveness, me_size, me_object, me_region, me_offset, addr, n), BoolType)

// // Get associated object for address
// private def addr_to_obj(addr: Expr, map: Expr = me_object) = FApplyExpr("addr_to_object", Seq(map, addr), IntType)

// // Get address offset into its object
// private def addr_to_offset(addr: Expr, map: Expr = me_offset) =
//   FApplyExpr("addr_to_offset", Seq(map, addr), BitVecType(64))

// // Get region an object belongs to (e.g. .data, .init, stack, heap)
// // 0: Unspecified for now i guess
// // 1: Heap, safe for object allocation, free, etc
// private def addr_to_region(addr: Expr, map: Expr = me_region) = FApplyExpr("addr_to_region", Seq(map, addr), IntType)

// // Get object size
// private def obj_size(obj: Expr, map: Expr = me_size) = FApplyExpr("obj_size", Seq(map, obj), BitVecType(64))

// private def obj_is_fresh(obj: Expr, map: Expr = me_liveness) =
//   BinaryExpr(EQ, obj_liveness(obj, map = map), BitVecLiteral(0, 2))
// private def obj_is_alive(obj: Expr, map: Expr = me_liveness) =
//   BinaryExpr(EQ, obj_liveness(obj, map = map), BitVecLiteral(1, 2))
// private def obj_is_dead(obj: Expr, map: Expr = me_liveness) =
//   BinaryExpr(EQ, obj_liveness(obj, map = map), BitVecLiteral(2, 2))
// private def obj_liveness(obj: Expr, map: Expr = me_liveness) =
//   FApplyExpr("obj_liveness", Seq(map, obj), BitVecType(2))

// private def read_mem(addr: Expr, map: Expr = mem) = FApplyExpr("read_mem", Seq(map, addr), BitVecType(8))

// class MemoryEncodingTransform(ctx: IRContext, simplify: Boolean) extends CILVisitor {
//   private var global_addresses = ctx.symbols.flatMap(s => Range(s.value.intValue, s.value.intValue + s.size)).toSet

//   private def transform_memset(p: Procedure) = {
//     // r0: Start address
//     // r1: Character
//     // r2: Bytes to write

//     p.modifies ++= Set(mem)

//     p.requiresExpr ++= List(valid(r(0), r(2)))

//     p.ensuresExpr ++= List(
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(i),
//           implies_else(
//             in_bounds(r(0), BinaryExpr(BVADD, r(0), r(2)), i),
//             BinaryExpr(EQ, read_mem(i), Extract(8, 0, r(1))),
//             BinaryExpr(EQ, read_mem(i), read_mem(i, map = OldExpr(mem)))
//           )
//         ),
//         triggers = List(List(read_mem(i)))
//       )
//     )
//   }

//   private def transform_memcpy(p: Procedure) = {
//     // r0: Destination start address
//     // r1: Source start address
//     // r2: Bytes to copy

//     p.modifies ++= Set(mem)

//     p.requires ++= List(BinaryBExpr(EQ, r(2).toGamma, TrueBLiteral))

//     p.requiresExpr ++= List(
//       // BinaryExpr(EQ, gamma_r(2), TrueLiteral),
//       valid(r(0), r(2)),
//       valid(r(1), r(2))
//     )

//     val cond = in_bounds(r(0), BinaryExpr(BVADD, r(0), r(2)), i)

//     p.ensures ++= List(
//       ForAll(
//         List(i.toBoogie),
//         BinaryBExpr(
//           EQ,
//           MapAccess(mem.toGamma, i.toBoogie),
//           IfThenElse(
//             cond.toBoogie,
//             MapAccess(mem.toGamma, BinaryBExpr(BVADD, addr_to_offset(i).toBoogie, r(1).toBoogie)),
//             Old(MapAccess(mem.toGamma, i.toBoogie))
//           )
//         ),
//         List(List(MapAccess(mem.toGamma, i.toBoogie)))
//       )
//     )

//     p.ensuresExpr ++= List(
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(i),
//           implies_else(
//             cond,
//             BinaryExpr(EQ, read_mem(i), read_mem(BinaryExpr(BVADD, addr_to_offset(i), r(1)))),
//             BinaryExpr(EQ, read_mem(i), read_mem(i, map = OldExpr(mem)))
//           )
//         ),
//         triggers = List(List(read_mem(i)))
//       )
//     )
//   }

//   private def transform_strlen(p: Procedure) = {
//     p.modifies ++= (if (simplify) then Set() else Set(r(0)))

//     p.requiresExpr ++= List(
//       valid(r(0), BitVecLiteral(1, 64)),
//       QuantifierExpr(
//         QuantifierSort.exists,
//         LambdaExpr(
//           List(i),
//           BinaryExpr(
//             BoolAND,
//             BinaryExpr(EQ, addr_to_obj(i), addr_to_obj(r(0))),
//             BinaryExpr(EQ, read_mem(i), BitVecLiteral(0, 8))
//           )
//         )
//       )
//     )

//     p.ensures = p.ensures ++ List(BinaryBExpr(EQ, r(0).toGamma, TrueBLiteral))

//     val dest = BinaryExpr(BVADD, OldExpr(r(0)), r(0));
//     p.ensuresExpr ++= List(
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(i),
//           implies(in_bounds(OldExpr(r(0)), dest, i), BinaryExpr(NEQ, read_mem(i), BitVecLiteral(0, 8)))
//         ),
//         triggers = List(List(read_mem(i)))
//       ),
//       BinaryExpr(EQ, read_mem(dest), BitVecLiteral(0, 8)),
//       valid(OldExpr(r(0)), r(0)),
//       BinaryExpr(BVULE, OldExpr(r(0)), dest)
//     )
//   }

//   private def transform_free(p: Procedure) = {
//     p.modifies ++= Set(me_liveness)

//     p.requiresExpr ++= List(
//       // Addr must be at offset 0 to free
//       BinaryExpr(EQ, addr_to_offset(r(0)), BitVecLiteral(0, 64)),
//       // obj must be alive
//       obj_is_alive(addr_to_obj(r(0))),
//       // Addr must be heap allocated
//       BinaryExpr(EQ, addr_to_region(r(0)), IntLiteral(1))
//     )

//     // Freeing requires fully high gamma
//     p.requires ++= List(
//       ForAll(
//         List(i.toBoogie),
//         BinaryBExpr(
//           BoolIMPLIES,
//           same_object(i, r(0)).toBoogie,
//           BinaryBExpr(EQ, MapAccess(mem.toGamma, i.toBoogie), TrueBLiteral)
//         )
//       )
//     )

//     // Ensures liveness is dead for the object otherwise unchanged
//     p.ensuresExpr ++= List(
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(o),
//           implies_else(
//             BinaryExpr(EQ, o, addr_to_obj(r(0))),
//             obj_is_dead(o),
//             BinaryExpr(EQ, obj_liveness(o), obj_liveness(o, map = OldExpr(me_liveness)))
//           )
//         ),
//         triggers = List(List(obj_liveness(o)))
//       )
//     )
//   }

//   private def transform_malloc(p: Procedure) = {
//     p.modifies ++= Set(me_object_counter, me_liveness, me_live, me_offset, me_object) ++ (if (simplify) then Set()
//                                                                                           else Set(r(0)))

//     p.requires ++= List(BinaryBExpr(EQ, r(0).toGamma, TrueBLiteral))

//     p.requiresExpr ++= List(
//       // Cant malloc 0 or less bytes
//       BinaryExpr(BVUGT, r(0), BitVecLiteral(0, 64))
//     )

//     p.ensures ++= List(BinaryBExpr(EQ, r(0).toGamma, TrueBLiteral))

//     p.ensuresExpr ++= List(
//       BinaryExpr(EQ, me_object_counter, BinaryExpr(IntADD, OldExpr(me_object_counter), IntLiteral(1))),
//       BinaryExpr(BVUGT, BinaryExpr(BVADD, r(0), OldExpr(r(0))), r(0)),

//       // All addresses in allocation are updated to the new object
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(i),
//           implies_else(
//             in_bounds(r(0), BinaryExpr(BVADD, r(0), OldExpr(r(0))), i),
//             BinaryExpr(EQ, addr_to_obj(i), OldExpr(me_object_counter)),
//             BinaryExpr(EQ, addr_to_obj(i), addr_to_obj(i, map = OldExpr(me_object)))
//           )
//         ),
//         triggers = List(List(addr_to_obj(i)))
//       ),

//       // All addresses in allocation have offset relative to base
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(i),
//           implies_else(
//             same_object(i, r(0)),
//             BinaryExpr(EQ, addr_to_offset(i), BinaryExpr(BVSUB, i, r(0))),
//             BinaryExpr(EQ, addr_to_offset(i), addr_to_offset(i, map = OldExpr(me_offset)))
//           )
//         ),
//         triggers = List(List(addr_to_offset(i)))
//       ),

//       // Object liveness and size must be updated
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(o),
//           implies_else(
//             BinaryExpr(EQ, o, OldExpr(me_object_counter)),
//             BinaryExpr(BoolAND, obj_is_alive(o), BinaryExpr(EQ, obj_size(o), OldExpr(r(0)))),
//             BinaryExpr(
//               BoolAND,
//               BinaryExpr(EQ, obj_liveness(o), obj_liveness(o, map = OldExpr(me_liveness))),
//               BinaryExpr(EQ, obj_size(o), obj_size(o, map = OldExpr(me_size)))
//             )
//           )
//         ),
//         triggers = List(List(obj_liveness(o), obj_size(o)))
//       ),

//       // Allocated object was fresh prior to allocation,
//       // and it belongs to the heap.
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(i),
//           implies(
//             same_object(i, r(0)),
//             BinaryExpr(
//               BoolAND,
//               obj_is_fresh(addr_to_obj(i, map = OldExpr(me_object)), map = OldExpr(me_liveness)),
//               BinaryExpr(EQ, addr_to_region(i), IntLiteral(1))
//             )
//           )
//         ),
//         triggers = List(List(addr_to_obj(i)), List(addr_to_region(i)))
//       )
//     )
//   }

//   private def transform_main(p: Procedure) = {
//     p.requiresExpr ++= List(
//       BinaryExpr(EQ, me_object_counter, IntLiteral(0)),
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(o),
//           obj_is_fresh(o)
//         ),
//         triggers = List(List(obj_liveness(o)))
//       ),
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(i),
//           BinaryExpr(EQ, addr_to_obj(i), IntLiteral(0))
//         ),
//         triggers = List(List(addr_to_obj(i)))
//       ),
//       QuantifierExpr(
//         QuantifierSort.forall,
//         LambdaExpr(
//           List(i),
//           implies_else(
//             BinaryExpr(BVULE, i, BitVecLiteral(global_addresses.max, 64)),
//             BinaryExpr(EQ, addr_to_region(i), IntLiteral(0)),
//             BinaryExpr(EQ, addr_to_region(i), IntLiteral(1)),
//           )
//         ),
//         triggers = List(List(addr_to_region(i)))
//       ),
//     )
//     // p.requires = p.requires ++ List(
//     //   // Allocation counter starts at 1
//     //   BinaryBExpr(EQ, me_alloc_counter, IntBLiteral(1)),

//     //   // All objects start fresh ...
//     //   ForAll(
//     //     List(o),
//     //     BinaryBExpr(EQ,
//     //       MapAccess(me_live, o),
//     //       BitVecBLiteral(2, 8)
//     //     ),
//     //     List(List(MapAccess(me_live, o)))
//     //   ),

//     //   // ... with id 0
//     //   ForAll(
//     //     List(i),
//     //     BinaryBExpr(EQ,
//     //       MapAccess(me_object, i),
//     //       IntBLiteral(0)
//     //     ),
//     //     List(List(MapAccess(me_object, i)))
//     //   ),

//     //   ForAll(
//     //     List(i),
//     //     BinaryBExpr(EQ,
//     //       MapAccess(me_global, i),
//     //       BinaryBExpr(BVULE, i, BitVecBLiteral(global_addresses.max, 64)),
//     //     ),
//     //     List(List(MapAccess(me_global, i)))
//     //   ),
//     // )
//   }

//   override def vprog(p: Program) = {
//     // TODO: datatypes would clean liveness up a bit in future. Something like this:
//     // https://github.com/boogie-org/boogie/blob/master/Test/datatypes/is-cons.bpl

//     DoChildren()
//   }

//   override def vproc(p: Procedure) = {
//     p.procName match {
//       case "malloc" => transform_malloc(p)
//       case "free" => transform_free(p)
//       case "main" => transform_main(p)
//       case "memset" => transform_memset(p)
//       case "strlen" => transform_strlen(p)
//       case "memcpy" => transform_memcpy(p)
//       case _ => {}
//     }

//     SkipChildren()
//   }
// }

private val me_live_param = BMapVar("live", MapBType(IntBType, BitVecBType(8)), Scope.Parameter)
private val me_live_val_param = BMapVar("live_val", MapBType(IntBType, BitVecBType(64)), Scope.Parameter)
private val me_object_param = BMapVar("object", MapBType(BitVecBType(64), IntBType), Scope.Parameter)
private val me_position_param = BMapVar("position", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Parameter)
private val me_global_param = BMapVar("global", MapBType(BitVecBType(64), BoolBType), Scope.Parameter)
private val addr_param = BVariable("addr", BitVecBType(64), Scope.Parameter)
private val n_param = BVariable("n", BitVecBType(64), Scope.Parameter)

private def validDecl() = {
  val obj = MapAccess(me_object_param, addr_param)
  val size = MapAccess(me_live_val_param, obj)
  val pos = MapAccess(me_position_param, addr_param)

  BFunction(
    "valid",
    List(me_live_param, me_live_val_param, me_object_param, me_position_param, me_global_param, addr_param, n_param),
    BParam(BoolBType),
    Some(
      BinaryBExpr(
        BoolIMPLIES,
        // Only care to reason about an allocated pointer (not globals):
        UnaryBExpr(BoolNOT, MapAccess(me_global_param, addr_param)),
        BinaryBExpr(
          BoolAND,
          BinaryBExpr(EQ, MapAccess(me_live_param, obj), BitVecBLiteral(1, 8)),
          BinaryBExpr(
            BoolAND,
            BinaryBExpr(BVULE, BitVecBLiteral(0, 64), pos),
            BinaryBExpr(BVULE, BinaryBExpr(BVADD, pos, n_param), size)
          )
        )
      )
    ),
    List(BAttribute("extern"))
  )
}

def memoryEncodingDecls(): List[BDeclaration] = {
  List(validDecl())
}

def memoryStoreAsserts(m: MemoryStore): List[BCmd] = {
  List(BAssert(valid(m.index.toBoogie, BitVecBLiteral(m.size / 8, 64))))
}
