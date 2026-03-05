package ir.transforms.memoryEncoding.flat

import boogie.*
import ir.*
import ir.cilvisitor.*

// object counter:
private val me_object_counter = GlobalVar("me_object_counter", IntType)

private val me_region = GlobalVar("me_region", MapType(BitVecType(64), IntType))
private val me_object = GlobalVar("me_object", MapType(BitVecType(64), IntType))
private val me_offset = GlobalVar("me_offset", MapType(BitVecType(64), BitVecType(64)))

// Liveness/Temporal bound:
// 0 = fresh, 1 = live, 2 = dead
private val me_liveness = GlobalVar("me_liveness", MapType(IntType, BitVecType(2)))
private val me_live = GlobalVar("me_live", MapType(IntType, BitVecType(2)))

// Spatial bound:
private val me_start = GlobalVar("me_start", MapType(IntType, BitVecType(64)))
private val me_end = GlobalVar("me_end", MapType(IntType, BitVecType(64)))
private val me_size = GlobalVar("me_size", MapType(IntType, BitVecType(64)))

private val mem = SharedMemory("mem", 64, 8)

private val i = LocalVar("i", BitVecType(64))
private val o = LocalVar("o", IntType)

private def implies_else(cond: Expr, true_br: Expr, false_br: Expr) = BinaryExpr(
  BoolAND,
  BinaryExpr(BoolIMPLIES, cond, true_br),
  BinaryExpr(BoolIMPLIES, UnaryExpr(BoolNOT, cond), false_br)
)

private def implies(cond: Expr, true_br: Expr) = BinaryExpr(BoolIMPLIES, cond, true_br)

private def same_object(p1: Expr, p2: Expr) =
  BinaryExpr(EQ, addr_to_obj(p1), addr_to_obj(p2))

private def in_bounds(lower: Expr, upper: Expr, n: Expr) =
  BinaryExpr(BoolAND, BinaryExpr(BVULE, lower, n), BinaryExpr(BVULT, n, upper))

// Check if an address + size is a valid region for access
private def valid(addr: Expr, n: Expr) =
  FApplyExpr("valid", Seq(me_liveness, me_size, me_object, me_region, me_offset, addr, n), BoolType)

// Get associated object for address
private def addr_to_obj(addr: Expr, map: Expr = me_object) = FApplyExpr("addr_to_obj", Seq(map, addr), IntType)

// Get address offset into its object
private def addr_to_offset(addr: Expr, map: Expr = me_offset) =
  FApplyExpr("addr_to_offset", Seq(map, addr), BitVecType(64))

// Get region an object belongs to (e.g. .data, .init, stack, heap)
// 0: Unspecified for now i guess
// 1: Heap, safe for object allocation, free, etc
private def addr_to_region(addr: Expr, map: Expr = me_region) = FApplyExpr("addr_to_region", Seq(map, addr), IntType)

// Get object size
private def obj_size(obj: Expr, map: Expr = me_size) = FApplyExpr("obj_size", Seq(map, obj), BitVecType(64))

private def obj_is_fresh(obj: Expr, map: Expr = me_liveness) =
  BinaryExpr(EQ, obj_liveness(obj, map = map), BitVecLiteral(0, 2))
private def obj_is_alive(obj: Expr, map: Expr = me_liveness) =
  BinaryExpr(EQ, obj_liveness(obj, map = map), BitVecLiteral(1, 2))
private def obj_is_dead(obj: Expr, map: Expr = me_liveness) =
  BinaryExpr(EQ, obj_liveness(obj, map = map), BitVecLiteral(2, 2))
private def obj_liveness(obj: Expr, map: Expr = me_liveness) =
  FApplyExpr("obj_liveness", Seq(map, obj), BitVecType(2))

private def read_mem(addr: Expr, map: Expr = mem) = FApplyExpr("read_mem", Seq(map, addr), BitVecType(8))

class FlatTransform(ctx: IRContext, simplify: Boolean) extends CILVisitor {
  private var global_addresses = ctx.symbols.flatMap(s => Range(s.value.intValue, s.value.intValue + s.size)).toSet

  private def r(n: Int) = if simplify then LocalVar(s"R${n}_out", BitVecType(64))
  else Register(s"R$n", 64)

  private def old_r(n: Int) = if simplify then LocalVar(s"R${n}_in", BitVecType(64)) else OldExpr(Register(s"R$n", 64))

  private def old_gamma_r(n: Int) =
    if simplify then LocalVar(s"R${n}_in", BitVecType(64)).toGamma else Old(Register(s"R$n", 64).toGamma)

  // for preconditions, aka no OLD and maps to #Rn_in with simplify
  private def pre_r(n: Int) = if simplify then LocalVar(s"R${n}_in", BitVecType(64))
  else Register(s"R$n", 64)

  private def transform_memset(p: Procedure) = {
    // r0: Start address
    // r1: Character
    // r2: Bytes to write

    p.modifies ++= Set(mem)
    if !simplify then p.modifies ++= Set(Register("R0", 64))

    p.requiresExpr ++= List(valid(pre_r(0), pre_r(2)))

    p.ensures ++= List(
      BinaryBExpr(EQ, r(0).toBoogie, old_r(0).toBoogie),
      ForAll(
        List(i.toBoogie),
        BinaryBExpr(
          EQ,
          MapAccess(mem.toGamma, i.toBoogie),
          IfThenElse(
            in_bounds(old_r(0), BinaryExpr(BVADD, old_r(0), old_r(2)), i).toBoogie,
            old_gamma_r(1),
            Old(MapAccess(mem.toGamma, i.toBoogie))
          )
        ),
        List(List(MapAccess(mem.toGamma, i.toBoogie)))
      )
    )

    p.ensuresExpr ++= List(
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(i),
          implies_else(
            in_bounds(old_r(0), BinaryExpr(BVADD, old_r(0), old_r(2)), i),
            BinaryExpr(EQ, read_mem(i), Extract(8, 0, old_r(1))),
            BinaryExpr(EQ, read_mem(i), read_mem(i, map = OldExpr(mem)))
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

    p.modifies ++= Set(mem)
    if !simplify then p.modifies ++= Set(Register("R0", 64))

    p.requires ++= List(BinaryBExpr(EQ, pre_r(2).toGamma, TrueBLiteral))

    p.requiresExpr ++= List(
      // BinaryExpr(EQ, gamma_r(2), TrueLiteral),
      valid(pre_r(0), pre_r(2)),
      valid(pre_r(1), pre_r(2))
    )

    val cond = in_bounds(pre_r(0), BinaryExpr(BVADD, old_r(0), old_r(2)), i)

    p.ensures ++= List(
      BinaryBExpr(EQ, r(0).toBoogie, old_r(0).toBoogie),
      ForAll(
        List(i.toBoogie),
        BinaryBExpr(
          EQ,
          MapAccess(mem.toGamma, i.toBoogie),
          IfThenElse(
            cond.toBoogie,
            MapAccess(mem.toGamma, BinaryBExpr(BVADD, addr_to_offset(i).toBoogie, old_r(1).toBoogie)),
            Old(MapAccess(mem.toGamma, i.toBoogie))
          )
        ),
        List(List(MapAccess(mem.toGamma, i.toBoogie)))
      )
    )

    p.ensuresExpr ++= List(
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(i),
          implies_else(
            cond,
            BinaryExpr(EQ, read_mem(i), read_mem(BinaryExpr(BVADD, addr_to_offset(i), old_r(1)))),
            BinaryExpr(EQ, read_mem(i), read_mem(i, map = OldExpr(mem)))
          )
        ),
        triggers = List(List(read_mem(i)))
      )
    )
  }

  private def transform_strlen(p: Procedure) = {
    if !simplify then p.modifies ++= Set(Register("R0", 64))

    p.requiresExpr ++= List(
      valid(pre_r(0), BitVecLiteral(1, 64)),
      QuantifierExpr(
        QuantifierSort.exists,
        LambdaExpr(
          List(i),
          BinaryExpr(
            BoolAND,
            BinaryExpr(EQ, addr_to_obj(i), addr_to_obj(pre_r(0))),
            BinaryExpr(EQ, read_mem(i), BitVecLiteral(0, 8))
          )
        )
      )
    )

    p.ensures = p.ensures ++ List(BinaryBExpr(EQ, r(0).toGamma, TrueBLiteral))

    val dest = BinaryExpr(BVADD, old_r(0), r(0));
    p.ensuresExpr ++= List(
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(List(i), implies(in_bounds(old_r(0), dest, i), BinaryExpr(NEQ, read_mem(i), BitVecLiteral(0, 8)))),
        triggers = List(List(read_mem(i)))
      ),
      BinaryExpr(EQ, read_mem(dest), BitVecLiteral(0, 8)),
      valid(old_r(0), r(0)),
      BinaryExpr(BVULE, old_r(0), dest)
    )
  }

  private def transform_free(p: Procedure) = {
    p.modifies ++= Set(me_liveness)

    p.requiresExpr ++= List(
      // Addr must be at offset 0 to free
      BinaryExpr(EQ, addr_to_offset(pre_r(0)), BitVecLiteral(0, 64)),
      // obj must be alive
      obj_is_alive(addr_to_obj(pre_r(0))),
      // Addr must be heap allocated
      BinaryExpr(EQ, addr_to_region(pre_r(0)), IntLiteral(1))
    )

    // Freeing requires fully high gamma
    p.requires ++= List(
      ForAll(
        List(i.toBoogie),
        BinaryBExpr(
          BoolIMPLIES,
          same_object(i, pre_r(0)).toBoogie,
          BinaryBExpr(EQ, MapAccess(mem.toGamma, i.toBoogie), TrueBLiteral)
        )
      )
    )

    // Ensures liveness is dead for the object otherwise unchanged
    p.ensuresExpr ++= List(
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(o),
          implies_else(
            BinaryExpr(EQ, o, addr_to_obj(old_r(0))),
            obj_is_dead(o),
            BinaryExpr(EQ, obj_liveness(o), obj_liveness(o, map = OldExpr(me_liveness)))
          )
        ),
        triggers = List(List(obj_liveness(o)))
      )
    )
  }

  private def transform_malloc(p: Procedure) = {
    p.modifies ++= Set(me_object_counter, me_liveness, me_live, me_offset, me_object)

    if !simplify then p.modifies ++= Set(Register("R0", 64))

    p.requires ++= List(BinaryBExpr(EQ, pre_r(0).toGamma, TrueBLiteral))

    p.requiresExpr ++= List(
      // Cant malloc 0 or less bytes
      BinaryExpr(BVUGT, pre_r(0), BitVecLiteral(0, 64))
    )

    p.ensures ++= List(BinaryBExpr(EQ, r(0).toGamma, TrueBLiteral))

    p.ensuresExpr ++= List(
      BinaryExpr(EQ, me_object_counter, BinaryExpr(IntADD, OldExpr(me_object_counter), IntLiteral(1))),
      BinaryExpr(BVUGT, BinaryExpr(BVADD, r(0), old_r(0)), r(0)),

      // All addresses in allocation are updated to the new object
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(i),
          implies_else(
            in_bounds(r(0), BinaryExpr(BVADD, r(0), old_r(0)), i),
            BinaryExpr(EQ, addr_to_obj(i), OldExpr(me_object_counter)),
            BinaryExpr(EQ, addr_to_obj(i), addr_to_obj(i, map = OldExpr(me_object)))
          )
        ),
        triggers = List(List(addr_to_obj(i)))
      ),

      // All addresses in allocation have offset relative to base
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(i),
          implies_else(
            same_object(i, r(0)),
            BinaryExpr(EQ, addr_to_offset(i), BinaryExpr(BVSUB, i, r(0))),
            BinaryExpr(EQ, addr_to_offset(i), addr_to_offset(i, map = OldExpr(me_offset)))
          )
        ),
        triggers = List(List(addr_to_offset(i)))
      ),

      // Object liveness and size must be updated
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(o),
          implies_else(
            BinaryExpr(EQ, o, OldExpr(me_object_counter)),
            BinaryExpr(BoolAND, obj_is_alive(o), BinaryExpr(EQ, obj_size(o), old_r(0))),
            BinaryExpr(
              BoolAND,
              BinaryExpr(EQ, obj_liveness(o), obj_liveness(o, map = OldExpr(me_liveness))),
              BinaryExpr(EQ, obj_size(o), obj_size(o, map = OldExpr(me_size)))
            )
          )
        ),
        triggers = List(List(obj_liveness(o)), List(obj_size(o)))
      ),

      // Allocated object was fresh prior to allocation,
      // and it belongs to the heap.
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(i),
          implies(
            same_object(i, r(0)),
            BinaryExpr(
              BoolAND,
              obj_is_fresh(addr_to_obj(i, map = OldExpr(me_object)), map = OldExpr(me_liveness)),
              BinaryExpr(EQ, addr_to_region(i), IntLiteral(1))
            )
          )
        ),
        triggers = List(List(addr_to_obj(i)), List(addr_to_region(i)))
      )
    )
  }

  private def transform_main(p: Procedure) = {
    p.requiresExpr ++= List(
      BinaryExpr(EQ, me_object_counter, IntLiteral(1)),
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(List(o), obj_is_fresh(o)),
        triggers = List(List(obj_liveness(o)))
      ),
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(List(i), BinaryExpr(EQ, addr_to_obj(i), IntLiteral(0))),
        triggers = List(List(addr_to_obj(i)))
      ),
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(i),
          implies_else(
            BinaryExpr(BVULE, i, BitVecLiteral(global_addresses.max, 64)),
            BinaryExpr(EQ, addr_to_region(i), IntLiteral(0)),
            BinaryExpr(EQ, addr_to_region(i), IntLiteral(1))
          )
        ),
        triggers = List(List(addr_to_region(i)))
      )
    )
  }

  override def vstmt(s: Statement) = {
    s match {
      case s: MemoryStore => {
        val size = BitVecLiteral(s.size / 8, 64);
        ChangeTo(List(Assert(valid(s.index, size), comment = Some("Requires Valid Memory")), s))
      }
      case s: MemoryLoad => {
        val size = BitVecLiteral(s.size / 8, 64);
        ChangeTo(List(Assert(valid(s.index, size), comment = Some("Requires Valid Memory")), s))
      }
      case _ => {
        DoChildren()
      }
    }
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

    DoChildren()
  }
}

def memoryEncodingDecls(): List[BDeclaration] = {
  val me_object_param = BMapVar("object", MapBType(BitVecBType(64), IntBType), Scope.Parameter)
  val me_offset_param = BMapVar("offset", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Parameter)
  val me_region_param = BMapVar("region", MapBType(BitVecBType(64), IntBType), Scope.Parameter)
  val me_liveness_param = BMapVar("liveness", MapBType(IntBType, BitVecBType(2)), Scope.Parameter)
  val me_size_param = BMapVar("size", MapBType(IntBType, BitVecBType(64)), Scope.Parameter)
  val mem_param = BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter)
  val addr_param = BVariable("addr", BitVecBType(64), Scope.Parameter)
  val n_param = BVariable("n", BitVecBType(64), Scope.Parameter)
  val obj_param = BVariable("obj", IntBType, Scope.Parameter)

  List(
    BFunction(
      "addr_to_obj",
      List(me_object_param, addr_param),
      BVariable("r", IntBType, Scope.Local),
      Some(MapAccess(me_object_param, addr_param))
    ),
    BFunction(
      "addr_to_offset",
      List(me_offset_param, addr_param),
      BVariable("r", BitVecBType(64), Scope.Local),
      Some(MapAccess(me_offset_param, addr_param))
    ),
    BFunction(
      "addr_to_region",
      List(me_region_param, addr_param),
      BVariable("r", IntBType, Scope.Local),
      Some(MapAccess(me_region_param, addr_param))
    ),
    BFunction(
      "obj_liveness",
      List(me_liveness_param, obj_param),
      BVariable("r", BitVecBType(2), Scope.Local),
      Some(MapAccess(me_liveness_param, obj_param))
    ),
    BFunction(
      "obj_size",
      List(me_size_param, obj_param),
      BVariable("r", BitVecBType(64), Scope.Local),
      Some(MapAccess(me_size_param, obj_param))
    ),
    BFunction(
      "read_mem",
      List(mem_param, addr_param),
      BVariable("r", BitVecBType(8), Scope.Local),
      Some(MapAccess(mem_param, addr_param))
    ),
    BFunction(
      "valid",
      List(me_liveness_param, me_size_param, me_object_param, me_region_param, me_offset_param, addr_param, n_param),
      BVariable("r", BoolBType, Scope.Local),
      Some(
        BinaryBExpr(
          BoolIMPLIES,
          BinaryBExpr(
            EQ,
            BFunctionCall("addr_to_region", List(me_region_param, addr_param), IntBType, true),
            IntBLiteral(1)
          ),
          BinaryBExpr(
            BoolAND,
            BinaryBExpr(
              EQ,
              BFunctionCall(
                "obj_liveness",
                List(
                  me_liveness_param,
                  BFunctionCall("addr_to_obj", List(me_object_param, addr_param), IntBType, true)
                ),
                BitVecBType(2),
                true
              ),
              BitVecBLiteral(1, 2)
            ),
            BinaryBExpr(
              BoolAND,
              BinaryBExpr(
                BVULE,
                BitVecBLiteral(0, 64),
                BFunctionCall("addr_to_offset", List(me_offset_param, addr_param), BitVecBType(64), true)
              ),
              BinaryBExpr(
                BVULE,
                BinaryBExpr(
                  BVADD,
                  BFunctionCall("addr_to_offset", List(me_offset_param, addr_param), BitVecBType(64), true),
                  n_param
                ),
                BFunctionCall(
                  "obj_size",
                  List(me_size_param, BFunctionCall("addr_to_obj", List(me_object_param, addr_param), IntBType, true)),
                  BitVecBType(64),
                  true
                )
              )
            )
          )
        )
      )
    )
  )
}
