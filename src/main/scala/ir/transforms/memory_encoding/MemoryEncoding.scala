package ir.transforms.memoryEncoding

import util.MemoryEncodingRepresentation
import boogie.*
import ir.*

trait MemoryEncoding {
  val repr: MemoryEncodingRepresentation
  val simplify: Boolean

  // Type used to represent abstract allocation ids
  val allocationType: IRType
  val allocationBType: BType
  def allocationLiteral(n: Int): Expr

  val addressSize: Int = 64
  val addressType: IRType = BitVecType(addressSize)
  val addressBType: BType = BitVecBType(addressSize)
  def addressLiteral(n: Int): Expr = BitVecLiteral(n, addressSize)

  val liveType: IRType = BitVecType(2)
  val liveBType: BType = BitVecBType(2)
  val deadLiteral: Expr = BitVecLiteral(0, 2)
  val liveBLiteral: Expr = BitVecLiteral(1, 2)
  val freshLiteral: Expr = BitVecLiteral(2, 2)

  val memEncodingType: IRType = CustomSort("MemEncoding")
  val memEncodingBType: BType = CustomBType("MemEncoding")
  val memEncodingBDataTypeDecl: BDataTypeDecl = BDataTypeDecl(
    "MemEncoding",
    memEncodingBDataTypeBody
    // List(BDataTypeConstructor("MemEncoding", List(alloc_live_param, alloc_size_param, addr_is_heap_param)))
  )
  val memEncodingBDataTypeBody: List[BDataTypeConstructor] = List()

  // Necessary boogie declarations for the memory encoding
  def bDeclarations: List[BDeclaration] = {
    List(
      memEncodingBDataTypeDecl,
      allocSizeFunc,
      allocBaseFunc,
      addrAllocFunc,
      allocLiveFunc,
      addrOffsetFunc,
      addrIsHeapFunc,
      allocSizeUpdateFunc,
      allocLiveUpdateFunc,
      canAllocateFunc,
      allocateFunc,
      initHeapFunc
    )
  }

  // Helpful variables for function arguments/outputs
  val pMemEncoding = BVariable("mem_encoding", memEncodingBType, Scope.Parameter)
  val pAddr = BVariable("addr", addressBType, Scope.Parameter)
  val pAlloc = BVariable("alloc", allocationBType, Scope.Parameter)
  val pLive = BVariable("live", liveBType, Scope.Parameter)
  val pSize = BVariable("size", addressBType, Scope.Parameter)
  val pLb = BVariable("lb", addressBType, Scope.Parameter)
  val pUb = BVariable("ub", addressBType, Scope.Parameter)

  val rMemEncoding = BVariable("r", memEncodingBType, Scope.Parameter)
  val rAddr = BVariable("r", addressBType, Scope.Parameter)
  val rAlloc = BVariable("r", allocationBType, Scope.Parameter)
  val rLive = BVariable("r", liveBType, Scope.Parameter)
  val rBool = BVariable("r", BoolBType, Scope.Parameter)
  val r64 = BVariable("r", BitVecBType(64), Scope.Parameter)

  // Globals too
  val gMemEncoding = GlobalVar("mem_encoding", CustomSort("MemEncoding"))

  // me_alloc_size maps an allocation to its size.
  def allocSizeFunc: BFunction = BFunction("me_alloc_size", List(pMemEncoding, pAlloc), rAddr, allocSizeBody)
  def allocSizeBody: Option[BExpr] = None
  def allocSizeCall(alloc: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_alloc_size", Seq(me, alloc), addressType)
  def allocSizeBCall(alloc: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_alloc_size", List(me, alloc), addressBType)

  // me_alloc_base maps an allocation to its base address.
  def allocBaseFunc: BFunction = BFunction("me_alloc_base", List(pMemEncoding, pAlloc), rAddr, allocBaseBody)
  def allocBaseBody: Option[BExpr] = None
  def allocBaseCall(alloc: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_alloc_base", Seq(me, alloc), addressType)
  def allocBaseBCall(alloc: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_alloc_base", List(me, alloc), addressBType)

  // me_addr_alloc maps an address to its allocation id.
  // Allocation id must be of type allocationType
  def addrAllocFunc: BFunction = BFunction("me_addr_alloc", List(pMemEncoding, pAddr), rAlloc, addrAllocBody)
  def addrAllocBody: Option[BExpr] = None
  def addrAllocCall(addr: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_addr_alloc", Seq(me, addr), allocationType)
  def addrAllocBCall(addr: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_addr_alloc", List(me, addr), allocationBType)

  // me_alloc_live maps an allocess to its liveness.
  def allocLiveFunc: BFunction = BFunction("me_alloc_live", List(pMemEncoding, pAlloc), rLive, allocLiveBody)
  def allocLiveBody: Option[BExpr] = None
  def allocLiveCall(alloc: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_alloc_live", Seq(me, alloc), liveType)
  def allocLiveBCall(alloc: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_alloc_live", List(me, alloc), liveBType)

  // me_addr_offset maps an address to its offset within its allocation.
  def addrOffsetFunc: BFunction = BFunction("me_addr_offset", List(pMemEncoding, pAddr), rAddr, addrOffsetBody)
  def addrOffsetBody: Option[BExpr] = None
  def addrOffsetCall(addr: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_addr_offset", Seq(me, addr), addressType)
  def addrOffsetBCall(addr: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_addr_offset", List(me, addr), addressBType)

  // me_addr_is_heap maps an address to a boolean identifying if it belongs to the heap.
  def addrIsHeapFunc: BFunction = BFunction("me_addr_is_heap", List(pMemEncoding, pAddr), rBool, addrIsHeapBody)
  def addrIsHeapBody: Option[BExpr] = None

  def addrIsHeapCall(addr: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_addr_is_heap", Seq(me, addr), BoolType)
  def addrIsHeapBCall(addr: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_addr_is_heap", List(me, addr), BoolBType)

  // me_alloc_size_update returns an updated memory encoding with the size of the given allocation updated.
  def allocSizeUpdateFunc: BFunction =
    BFunction("me_alloc_size_update", List(pMemEncoding, pAlloc, pSize), rMemEncoding, allocSizeUpdateBody)
  def allocSizeUpdateBody: Option[BExpr] = None
  def allocSizeUpdateCall(alloc: Expr, size: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_alloc_size_update", Seq(me, alloc, size), memEncodingType)
  def allocSizeUpdateBCall(alloc: BExpr, size: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_alloc_size_update", List(me, alloc, size), memEncodingBType)

  // me_alloc_live_update returns an updated memory encoding with the liveness of the given allocation updated.
  def allocLiveUpdateFunc: BFunction =
    BFunction("me_alloc_live_update", List(pMemEncoding, pAlloc, pLive), rMemEncoding, allocLiveUpdateBody)
  def allocLiveUpdateBody: Option[BExpr] = None
  def allocLiveUpdateCall(alloc: Expr, live: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_alloc_live_update", Seq(me, alloc, live), memEncodingType)
  def allocLiveUpdateBCall(alloc: BExpr, live: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_alloc_live_update", List(me, alloc, live), memEncodingBType)

  // me_can_allocate tests whether an address + size pair may be allocated.
  // Useful in simulating out of memory conditions.
  def canAllocateFunc: BFunction =
    BFunction("me_can_allocate", List(pMemEncoding, pAddr, pSize), rBool, canAllocateBody)
  def canAllocateBody: Option[BExpr] = None
  def canAllocateCall(addr: Expr, size: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_can_allocate", Seq(me, addr, size), BoolType)
  def canAllocateBCall(addr: BExpr, size: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_can_allocate", List(me, addr, size), BoolBType)

  // me_allocate updates the memory encoding and constrains its arguments such that there is a valid
  // allocation at pAddr with size pSize.
  def allocateFunc: BFunction =
    BFunction("me_allocate", List(pMemEncoding, pAddr, pSize), rMemEncoding, allocateBody)
  def allocateBody: Option[BExpr] = None
  def allocateCall(addr: Expr, size: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_allocate", Seq(me, addr, size), memEncodingType)
  def allocateBCall(addr: BExpr, size: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_allocate", List(me, addr, size), memEncodingBType)

  // me_init_heap makes all assertions to initialize the heap within the given lb and ub addresses
  def initHeapFunc: BFunction = BFunction("me_init_heap", List(pMemEncoding, pLb, pUb), rBool, initHeapBody)
  def initHeapBody: Option[BExpr] = None
  def initHeapCall(lb: Expr, ub: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_init_heap", Seq(me, lb, ub), BoolType)
  def initHeapBCall(lb: BExpr, ub: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_init_heap", List(me, lb, ub), BoolBType)

  // me_valid_access ensures accessing an address + size is memory safe:
  // - An allocation exists at the address (or teh address is not on the heap)
  // - The accessed region is fully within bounds of the allocation
  // - The allocation is live
  def validAccessFunc: BFunction =
    BFunction("me_valid_access", List(pMemEncoding, pAddr, pSize), rBool, validAccessBody)
  def validAccessBody: Option[BExpr] = None
  def validAccessCall(addr: Expr, size: Expr, me: Expr = gMemEncoding): Expr =
    FApplyExpr("me_valid_access", Seq(me, addr, size), BoolType)
  def validAccessBCall(addr: BExpr, size: BExpr, me: BExpr = gMemEncoding.toBoogie): BExpr =
    BFunctionCall("me_valid_access", List(me, addr, size), BoolBType)
}

// class SplitEncoding(override val repr: MemoryEncodingRepresentation, override val simplify: Boolean) {}
