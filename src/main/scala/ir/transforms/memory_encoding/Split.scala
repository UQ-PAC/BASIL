package ir.transforms.memoryEncoding.split

import boogie.*
import ir.*
import ir.cilvisitor.*
import scala.math.pow

// Final n bytes of "addr" are offset into allocation.
// If set to 0, addresses are treated as raw pointers.
private val offset_size = 32;

// First n bytes of "addr" find the allocation.
// Uses whatever is left over from offset size to fit into 64 bits.
private val base_size = 64 - offset_size;

private val mem_encoding = GlobalVar("mem_encoding", CustomSort("MemEncoding"))
private val mem = SharedMemory("mem", 64, 8)

private def disjoint(addr1: Expr, addr2: Expr): Expr =
  FApplyExpr("me_alloc_disjoint", Seq(addr1, addr2), BoolType)
private def bDisjoint(addr1: BExpr, addr2: BExpr): BExpr =
  BFunctionCall("me_alloc_disjoint", List(addr1, addr2), BoolBType)

private def addrIsHeap(addr: Expr, map: Expr = mem_encoding): Expr =
  FApplyExpr("me_addr_is_heap", Seq(map, addr), BoolType)
private def bAddrIsHeap(addr: BExpr, map: BExpr = mem_encoding.toBoogie): BExpr =
  BFunctionCall("me_addr_is_heap", List(map, addr), BoolBType)

private def allocLive(addr: Expr, map: Expr = mem_encoding): Expr =
  FApplyExpr("me_alloc_live", Seq(map, addr), BitVecType(2))
private def bAllocLive(addr: BExpr, map: BExpr = mem_encoding.toBoogie): BExpr =
  BFunctionCall("me_alloc_live", List(map, addr), BitVecBType(2))

private def validAccess(addr: Expr, size: Expr, map: Expr = mem_encoding): Expr =
  FApplyExpr("me_valid_access", Seq(map, addr, size), BoolType)
private def bValidAccess(addr: BExpr, size: BExpr, map: BExpr = mem_encoding.toBoogie): BExpr =
  BFunctionCall("me_valid_access", List(map, addr, size), BoolBType)

private def addrOffset(addr: Expr): Expr =
  FApplyExpr("me_addr_offset", Seq(addr), BitVecType(64))
private def bAddrOffset(addr: BExpr): BExpr =
  BFunctionCall("me_addr_offset", List(addr), BitVecBType(64))

private def addrBase(addr: Expr): Expr =
  FApplyExpr("me_addr_base", Seq(addr), BitVecType(64))
private def bAddrBase(addr: BExpr): BExpr =
  BFunctionCall("me_addr_base", List(addr), BitVecBType(64))

private def allocSize(addr: Expr, map: Expr = mem_encoding): Expr =
  FApplyExpr("me_alloc_size", Seq(map, addr), BitVecType(64))
private def bAllocSize(addr: BExpr, map: BExpr = mem_encoding.toBoogie): BExpr =
  BFunctionCall("me_alloc_size", List(map, addr), BitVecBType(64))

private def liveUpdate(idx: Expr, value: Expr, map: Expr = mem_encoding, old_map: Expr = OldExpr(mem_encoding)): Expr =
  FApplyExpr("me_alloc_live_update", Seq(old_map, map, idx, value), BoolType)
private def bLiveUpdate(
  idx: BExpr,
  value: BExpr,
  map: BExpr = mem_encoding.toBoogie,
  old_map: BExpr = Old(mem_encoding.toBoogie)
): BExpr =
  BFunctionCall("me_alloc_live_update", List(old_map, map, idx, value), BoolBType)

private def sizeUpdate(idx: Expr, value: Expr, map: Expr = mem_encoding, old_map: Expr = OldExpr(mem_encoding)): Expr =
  FApplyExpr("me_alloc_size_update", Seq(old_map, map, idx, value), BoolType)
private def bSizeUpdate(
  idx: BExpr,
  value: BExpr,
  map: BExpr = mem_encoding.toBoogie,
  old_map: BExpr = Old(mem_encoding.toBoogie)
): BExpr =
  BFunctionCall("me_alloc_size_update", List(old_map, map, idx, value), BoolBType)

private def isHeapPreserve(map: Expr = mem_encoding, old_map: Expr = OldExpr(mem_encoding)): Expr =
  FApplyExpr("me_addr_is_heap_preserve", Seq(old_map, map), BoolType)
private def bIsHeapPreserve(map: BExpr = mem_encoding.toBoogie, old_map: BExpr = Old(mem_encoding.toBoogie)): BExpr =
  BFunctionCall("me_addr_is_heap_preserve", List(old_map, map), BoolBType)

// private def mePreserve(map: Expr = mem_encoding, old_map: Expr = OldExpr(mem_encoding)): Expr =
//   FApplyExpr("me_preserve", Seq(old_map, map), BoolType)

    // BFunction(
    //   "me_preserve",
    //   List(mem_encoding_in, mem_encoding_out),
    //   bool_r_param,
    //   Some(BinaryBExpr(BoolAND,
    //     BinaryBExpr(BoolAND,
    //       BinaryBExpr(EQ, alloc_live_access_out, alloc_live_access_in),
    //       BinaryBExpr(EQ, alloc_size_access_out, alloc_size_access_in)
    //     ),
    //     BinaryBExpr(EQ, addr_is_heap_access_out, addr_is_heap_access_in)
    //   )),
    //   attributes = List(BAttribute("inline", Some("true")))
    // ),
private def in_bounds(lower: Expr, upper: Expr, n: Expr) =
  BinaryExpr(BoolAND, BinaryExpr(BVULE, lower, n), BinaryExpr(BVULT, n, upper))

private val i = LocalVar("i", BitVecType(64))

private val fresh_bv = BitVecLiteral(0, 2);
private val live_bv = BitVecLiteral(1, 2);
private val dead_bv = BitVecLiteral(2, 2);

private def implies_else(cond: Expr, true_br: Expr, false_br: Expr) = BinaryExpr(
  BoolAND,
  BinaryExpr(BoolIMPLIES, cond, true_br),
  BinaryExpr(BoolIMPLIES, UnaryExpr(BoolNOT, cond), false_br)
)

class SplitTransform(ctx: IRContext, simplify: Boolean) extends CILVisitor {
  private var global_addresses = ctx.symbols.flatMap(s => Range(s.value.intValue, s.value.intValue + s.size)).toSet

  private def r(n: Int) = if simplify then LocalVar(s"R${n}_out", BitVecType(64))
  else Register(s"R$n", 64)

  private def old_r(n: Int) = if simplify then LocalVar(s"R${n}_in", BitVecType(64)) else OldExpr(Register(s"R$n", 64))

  private def old_gamma_r(n: Int) =
    if simplify then LocalVar(s"R${n}_in", BitVecType(64)).toGamma else Old(Register(s"R$n", 64).toGamma)

  // for preconditions, aka no OLD and maps to #Rn_in with simplify
  private def pre_r(n: Int) = if simplify then LocalVar(s"R${n}_in", BitVecType(64))
  else Register(s"R$n", 64)

  private def transform_malloc(p: Procedure) = {
    p.modifies ++= Set(mem_encoding)
    if !simplify then p.modifies ++= Set(Register("R0", 64))

    p.requires ++= List(BinaryBExpr(EQ, pre_r(0).toGamma, TrueBLiteral))

    p.requiresExpr ++= List(
      // Cant malloc 0 or less bytes
      BinaryExpr(BVUGT, pre_r(0), BitVecLiteral(0, 64)),
      // Malloc at most 2^offset_size - 1 bytes
      BinaryExpr(BVULE, pre_r(0), BitVecLiteral((BigInt(pow(2, offset_size).toLong) - 1), 64))
    )

    p.ensures ++= List(BinaryBExpr(EQ, r(0).toGamma, TrueBLiteral))

    p.ensuresExpr ++= List(
      BinaryExpr(BVUGT, BinaryExpr(BVADD, r(0), old_r(0)), r(0)),
      // Size mapping for returned pointer matches input size:
      sizeUpdate(r(0), old_r(0)),
      // Returned address is a base address:
      BinaryExpr(EQ, addrOffset(r(0)), BitVecLiteral(0, 64)),
      BinaryExpr(EQ, addrBase(r(0)), r(0)),
      // Returned address is on formerly fresh space:
      BinaryExpr(EQ, allocLive(r(0), map = OldExpr(mem_encoding)), fresh_bv),
      // Returned address is on now live space:
      liveUpdate(r(0), live_bv),
      // All other lives are unchanged:
      // Address is on heap:
      addrIsHeap(r(0), map = OldExpr(mem_encoding)),
      isHeapPreserve()
    )
  }

  private def transform_free(p: Procedure) = {
    p.modifies ++= Set(mem_encoding)

    p.requiresExpr ++= List(
      // Only free a heap value
      addrIsHeap(pre_r(0)),
      // Free only at base of region:
      BinaryExpr(EQ, addrOffset(pre_r(0)), BitVecLiteral(0, 64)),
      // Object must be alive:
      BinaryExpr(EQ, allocLive(pre_r(0)), live_bv)
    )

    // Freeing requires fully high gamma
    p.requires ++= List(
      ForAll(
        List(i.toBoogie),
        BinaryBExpr(
          BoolIMPLIES,
          in_bounds(addrBase(pre_r(0)), BinaryExpr(BVADD, addrBase(pre_r(0)), allocSize(pre_r(0))), i).toBoogie,
          BinaryBExpr(EQ, MapAccess(mem.toGamma, i.toBoogie), TrueBLiteral)
        )
      )
    )

    // Ensures liveness is dead for the object
    p.ensuresExpr ++= List(liveUpdate(pre_r(0), dead_bv), sizeUpdate(pre_r(0), BitVecLiteral(0, 64)), isHeapPreserve())
  }

  private def transform_main(p: Procedure) = {
    p.requires ++= List()

    val x = SharedMemorySplit("bv_bv_mem", 64, 64, 8);
    p.requiresExpr ++= List(
      BinaryExpr(EQ, x, x),
      BinaryExpr(
        EQ,
        BinaryExpr(BVAND, BitVecLiteral(0xff00, 64), BitVecLiteral(0xff00, 64)),
        BitVecLiteral(0xff00, 64)
      ),
      BinaryExpr(
        EQ,
        BinaryExpr(BVSHL, BitVecLiteral(0xffffffffL, 64), BitVecLiteral(32, 64)),
        BinaryExpr(BVSHL, BitVecLiteral(0xffffffffL, 64), BitVecLiteral(32, 64))
      ),
      // Cheaty way to carve out some heap pointers that wont conflict:
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(i),
          implies_else(
            BinaryExpr(BVULE, i, BitVecLiteral(global_addresses.max, 64)),
            UnaryExpr(BoolNOT, addrIsHeap(i)),
            addrIsHeap(i)
          )
        ),
        triggers = List(
          List(addrIsHeap(i)),
          // These are probably unecessary:
          List(allocLive(i)),
          List(allocSize(i)),
          List(addrBase(i)),
          List(addrOffset(i))
        )
      ),
      // Ensure all heap addresses are initially fresh:
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(List(i), BinaryExpr(BoolIMPLIES, addrIsHeap(i), BinaryExpr(EQ, allocLive(i), fresh_bv))),
        triggers = List(List(allocLive(i)))
      ),
      QuantifierExpr(
        QuantifierSort.forall,
        LambdaExpr(
          List(i),
          BinaryExpr(BoolIMPLIES, UnaryExpr(BoolNOT, addrIsHeap(i)), BinaryExpr(EQ, allocLive(i), dead_bv))
        ),
        triggers = List(List(allocLive(i)))
      )
    )
  }

  override def vstmt(s: Statement) = {
    s match {
      case s: MemoryStore => {
        val size = BitVecLiteral(s.size / 8, 64);
        ChangeTo(List(Assert(validAccess(s.index, size), comment = Some("Requires Valid Memory")), s))
      }
      case s: MemoryLoad => {
        val size = BitVecLiteral(s.size / 8, 64);
        ChangeTo(List(Assert(validAccess(s.index, size), comment = Some("Requires Valid Memory")), s))
      }
      case _ => {
        DoChildren()
      }
    }
  }

  override def vproc(p: Procedure) = {
    p.procName match {
      case "malloc" => transform_malloc(p)
      case "free" => transform_free(p)
      case "main" => transform_main(p)
      // case "memset" => transform_memset(p)
      // case "strlen" => transform_strlen(p)
      // case "memcpy" => transform_memcpy(p)
      case _ => {}
    }

    DoChildren()
  }
}

def memoryEncodingDecls(): List[BDeclaration] = {
  val r_alloc_live_param = BMapVar("r_alloc_live", MapBType(BitVecBType(64), BitVecBType(2)), Scope.Parameter)
  val r_alloc_size_param = BMapVar("r_alloc_size", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Parameter)
  val mem_param = BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter)

  val mem_encoding_param = BVariable("mem_encoding", CustomBType("MemEncoding"), Scope.Parameter)
  val mem_encoding_in = BVariable("mem_encoding_in", CustomBType("MemEncoding"), Scope.Parameter)
  val mem_encoding_out = BVariable("mem_encoding_out", CustomBType("MemEncoding"), Scope.Parameter)

  val alloc_live_param = BMapVar("alloc_live", MapBType(BitVecBType(64), BitVecBType(2)), Scope.Parameter)
  val alloc_size_param = BMapVar("alloc_size", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Parameter)
  val addr_is_heap_param = BMapVar("addr_is_heap", MapBType(BitVecBType(64), BoolBType), Scope.Parameter)

  // TODO: this is hacky, but the way bmapvar is structured makes this really hard to fix properly. See BFieldAccessExpr which works except when we need to return a mapvalue.
  val alloc_live_access =
    BMapVar("mem_encoding->#alloc_live", MapBType(BitVecBType(64), BitVecBType(2)), Scope.Parameter)
  val alloc_size_access =
    BMapVar("mem_encoding->#alloc_size", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Parameter)
  val addr_is_heap_access =
    BMapVar("mem_encoding->#addr_is_heap", MapBType(BitVecBType(64), BoolBType), Scope.Parameter)

  val alloc_live_access_in =
    BMapVar("mem_encoding_in->#alloc_live", MapBType(BitVecBType(64), BitVecBType(2)), Scope.Parameter)
  val alloc_size_access_in =
    BMapVar("mem_encoding_in->#alloc_size", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Parameter)
  val addr_is_heap_access_in =
    BMapVar("mem_encoding_in->#addr_is_heap", MapBType(BitVecBType(64), BoolBType), Scope.Parameter)

  val alloc_live_access_out =
    BMapVar("mem_encoding_out->#alloc_live", MapBType(BitVecBType(64), BitVecBType(2)), Scope.Parameter)
  val alloc_size_access_out =
    BMapVar("mem_encoding_out->#alloc_size", MapBType(BitVecBType(64), BitVecBType(64)), Scope.Parameter)
  val addr_is_heap_access_out =
    BMapVar("mem_encoding_out->#addr_is_heap", MapBType(BitVecBType(64), BoolBType), Scope.Parameter)

  val addr_param = BVariable("addr", BitVecBType(64), Scope.Parameter)
  val addr_param2 = BVariable("addr2", BitVecBType(64), Scope.Parameter)
  val offset_param = BVariable("offset", BitVecBType(64), Scope.Parameter)
  val base_param = BVariable("base", BitVecBType(64), Scope.Parameter)

  val size_param = BVariable("size", BitVecBType(64), Scope.Parameter)
  val live_param = BVariable("live", BitVecBType(2), Scope.Parameter)
  val bool_r_param = BVariable("r", BoolBType, Scope.Parameter)

  List(
    BDataTypeDecl(
      "MemEncoding",
      List(BDataTypeConstructor("MemEncoding", List(alloc_live_param, alloc_size_param, addr_is_heap_param)))
    ),
    BFunction(
      "me_addr_offset",
      List(addr_param),
      offset_param,
      // Some(BinaryBExpr(BVCONCAT, BitVecBLiteral(0, 64 - offset_size), BVExtract(offset_size, 0, addr_param))),
      Some(BinaryBExpr(BVAND, addr_param, BitVecBLiteral(0xffffffffL, 64))),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_addr_base",
      List(addr_param),
      base_param,
      // Some(BinaryBExpr(BVCONCAT, BVExtract(64, 64 - base_size, addr_param), BitVecBLiteral(0, 64 - base_size))),
      Some(BinaryBExpr(BVAND, addr_param, BinaryBExpr(BVSHL, BitVecBLiteral(0xffffffffL, 64), BitVecBLiteral(32, 64)))),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_preserve",
      List(mem_encoding_in, mem_encoding_out),
      bool_r_param,
      Some(BinaryBExpr(BoolAND,
        BinaryBExpr(BoolAND,
          BinaryBExpr(EQ, alloc_live_access_out, alloc_live_access_in),
          BinaryBExpr(EQ, alloc_size_access_out, alloc_size_access_in)
        ),
        BinaryBExpr(EQ, addr_is_heap_access_out, addr_is_heap_access_in)
      )),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_alloc_disjoint",
      List(addr_param, addr_param2),
      bool_r_param,
      Some(BinaryBExpr(NEQ, bAddrBase(addr_param), bAddrBase(addr_param2))),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_addr_is_heap",
      List(mem_encoding_param, addr_param),
      bool_r_param,
      Some(MapAccess(addr_is_heap_access, addr_param)),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_alloc_size",
      List(mem_encoding_param, addr_param),
      size_param,
      Some(MapAccess(alloc_size_access, bAddrBase(addr_param))),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_alloc_live",
      List(mem_encoding_param, addr_param),
      live_param,
      Some(MapAccess(alloc_live_access, bAddrBase(addr_param))),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_alloc_live_update",
      List(mem_encoding_in, mem_encoding_out, addr_param, live_param),
      bool_r_param,
      Some(BinaryBExpr(EQ, alloc_live_access_out, MapUpdate(alloc_live_access_in, addr_param, live_param))),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_alloc_size_update",
      List(mem_encoding_in, mem_encoding_out, addr_param, size_param),
      bool_r_param,
      Some(BinaryBExpr(EQ, alloc_size_access_out, MapUpdate(alloc_size_access_in, addr_param, size_param))),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_addr_is_heap_preserve",
      List(mem_encoding_in, mem_encoding_out),
      bool_r_param,
      Some(BinaryBExpr(EQ, addr_is_heap_access_out, addr_is_heap_access_in)),
      attributes = List(BAttribute("inline", Some("true")))
    ),
    BFunction(
      "me_valid_access",
      List(mem_encoding_param, addr_param, size_param),
      bool_r_param,
      Some(
        BinaryBExpr(
          BoolIMPLIES,
          bAddrIsHeap(addr_param, map = mem_encoding_param),
          BinaryBExpr(
            BoolAND,
            BinaryBExpr(EQ, bAllocLive(addr_param, map = mem_encoding_param), live_bv.toBoogie),
            BinaryBExpr(
              BVULE,
              BinaryBExpr(BVADD, bAddrOffset(addr_param), size_param),
              bAllocSize(addr_param, map = mem_encoding_param)
            )
          )
        )
      ),
      attributes = List(BAttribute("inline", Some("true")))
    )
  )
}
