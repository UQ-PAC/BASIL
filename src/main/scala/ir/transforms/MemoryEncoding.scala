package ir.transforms.memoryEncoding

import boogie.*
import ir.cilvisitor.*
import ir.*

private def r(n: Int) = Register(s"R$n", 64).toBoogie
private def gamma_r(n: Int) = Register(s"R$n", 64).toGamma

private val i = BVariable("i", BitVecBType(64), Scope.Local)
private val j = BVariable("j", BitVecBType(64), Scope.Local)
private val o = BVariable("o", IntBType, Scope.Local)

// Counter of allocations for getting a fresh id on new allocation
private val me_alloc_counter = GlobalVar("me_alloc_counter", IntType).toBoogie

// Object is a mapping from pointer to allocation id
private val me_object = BMapVar("me_object", MapBType(BitVecBType(64), IntBType), Scope.Global)
private val me_object_gamma = BMapVar("Gamma_me_object", MapBType(BitVecBType(64), BoolBType), Scope.Global)

// Position is a mapping from pointer to its offset into the allocation
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
private val me_region = BMapVar("me_region", MapBType(BitVecBType(64), IntBType), Scope.Global)

private val gamma_mem = BMapVar("Gamma_mem", MapBType(BitVecBType(64), BoolBType), Scope.Global);
private val mem = BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global);

private def valid(addr: BExpr, n: BExpr) =
  BFunctionCall("valid", List(me_live, me_live_val, me_object, me_position, me_region, addr, n), BoolBType)

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

class MemoryEncodingTransform(ctx: IRContext, simplify: Boolean) extends CILVisitor {
  private var global_addresses = ctx.symbols.flatMap(s => Range(s.value.intValue, s.value.intValue + s.size)).toSet

  private def R(n: Int) = BVariable(s"R$n", BitVecBType(64), Scope.Global);
  private def Gamma_R(n: Int) = BVariable(s"Gamma_R$n", BoolBType, Scope.Global);

  private def R_in(n: Int) =
    if (!simplify) then Old(BVariable(s"R$n", BitVecBType(64), Scope.Global))
    else BVariable(s"R$n%s_in", BitVecBType(64), Scope.Local);

  private def R_out(n: Int) =
    if (!simplify) then BVariable(s"R$n", BitVecBType(64), Scope.Global)
    else BVariable(s"R$n%s_out", BitVecBType(64), Scope.Local);

  private def Gamma_R_in(n: Int) =
    if (!simplify) then Old(BVariable(s"Gamma_R$n", BoolBType, Scope.Global))
    else BVariable(s"Gamma_R$n%s_in", BoolBType, Scope.Local);

  private def Gamma_R_out(n: Int) =
    if (!simplify) then BVariable(s"Gamma_R$n", BoolBType, Scope.Global)
    else BVariable(s"Gamma_R$n%s_out", BoolBType, Scope.Local);

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
      // The pointer being freed must belong to the heap
      BinaryBExpr(EQ, MapAccess(me_region, r(0)), IntBLiteral(1)),
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
        update_if(MapAccess(me_position, i), same_obj(me_object, r(0), i), BinaryBExpr(BVSUB, i, r(0))),
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
          same_obj(me_object, r(0), i),
          BinaryBExpr(
            BoolAND,
            // They must have previously been fresh in the old state
            BinaryBExpr(EQ, Old(MapAccess(me_live, Old(MapAccess(me_object, i)))), BitVecBLiteral(2, 8)),
            // And must be on the heap
            BinaryBExpr(EQ, Old(MapAccess(me_region, i)), IntBLiteral(1))
          )
        ),
        List(List(MapAccess(me_object, i)), List(MapAccess(me_region, i)))
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
        eq_if_then_else(
          MapAccess(me_region, i),
          in_lb_ub(i, BitVecBLiteral(0, 64), BitVecBLiteral(global_addresses.max, 64)),
          IntBLiteral(0),
          IntBLiteral(1)
        ),
        List(List(MapAccess(me_region, i)))
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

private val me_live_param = BMapVar("live", MapBType(IntBType, BitVecBType(8)), Scope.Parameter)
private val me_live_val_param = BMapVar("live_val", MapBType(IntBType, BitVecBType(64)), Scope.Parameter)
private val me_object_param = BMapVar("object", MapBType(BitVecBType(64), IntBType), Scope.Parameter)
private val me_position_param = BMapVar("position", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Parameter)
private val me_region_param = BMapVar("region", MapBType(BitVecBType(64), IntBType), Scope.Parameter)
private val addr_param = BVariable("addr", BitVecBType(64), Scope.Parameter)
private val n_param = BVariable("n", BitVecBType(64), Scope.Parameter)

private def validDecl() = {
  val obj = MapAccess(me_object_param, addr_param)
  val size = MapAccess(me_live_val_param, obj)
  val pos = MapAccess(me_position_param, addr_param)

  BFunction(
    "valid",
    List(me_live_param, me_live_val_param, me_object_param, me_position_param, me_region_param, addr_param, n_param),
    BParam(BoolBType),
    Some(
      BinaryBExpr(
        BoolIMPLIES,
        // Only care if heap addr:
        BinaryBExpr(EQ, MapAccess(me_region_param, addr_param), IntBLiteral(0)),
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
