package analysis

import analysis.BitVectorEval.isNegative
import ir.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Replaces the region access with the calculated memory region.
 */
class RegionInjector(domain: mutable.Set[CFGPosition],
                     program: Program,
                     constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                     mmm: MemoryModelMap,
                     reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
                     globalOffsets: Map[BigInt, BigInt]) {
  private val stackPointer = Register("R31", 64)

  def nodeVisitor(): Unit = {
    for (elem <- domain) {localTransfer(elem)}
    program.initialMemory = transformMemorySections(program.initialMemory)
    program.readOnlyMemory = transformMemorySections(program.readOnlyMemory)
  }

  /** Default implementation of eval.
   */
  def eval(expr: Expr, cmd: Command): Expr = {
    expr match
      case literal: Literal => literal // ignore literals
      case Extract(end, start, body) =>
        Extract(end, start, eval(body, cmd))
      case UninterpretedFunction(name, params, returnType) =>
        val newParams = params.map { p => eval(p, cmd) }
        UninterpretedFunction(name, newParams, returnType)
      case Repeat(repeats, body) =>
        Repeat(repeats, eval(body, cmd))
      case ZeroExtend(extension, body) =>
        ZeroExtend(extension, eval(body, cmd))
      case SignExtend(extension, body) =>
        SignExtend(extension, eval(body, cmd))
      case UnaryExpr(op, arg) =>
        UnaryExpr(op, eval(arg, cmd))
      case BinaryExpr(op, arg1, arg2) =>
        BinaryExpr(op, eval(arg1, cmd), eval(arg2, cmd))
      case MemoryLoad(mem, index, endian, size) =>
        // TODO: index should be replaced region
        MemoryLoad(renameMemory(mem, cmd), eval(index, cmd), endian, size)
      case variable: Variable => variable // ignore variables
  }

  def nodeToRegion(n: CFGPosition): Set[MemoryRegion] = {
    var returnRegions = Set.empty[MemoryRegion]
    n match {
      case directCall: DirectCall =>
        returnRegions = returnRegions + mmm.getHeap(directCall).asInstanceOf[MemoryRegion]
      case _ =>
        returnRegions = returnRegions ++ mmm.getStack(n).asInstanceOf[Set[MemoryRegion]] ++ mmm.getData(n).asInstanceOf[Set[MemoryRegion]]
    }
    returnRegions
  }

  def renameMemory(mem: Memory, cmd : Command): Memory = {
    val regions = nodeToRegion(cmd)
    if (regions.size == 1) {
      Logger.debug(s"Mem CMD is: ${cmd}")
      Logger.debug(s"Region found for mem: ${regions.head}")
      regions.head match {
        case stackRegion: StackRegion =>
          return StackMemory(stackRegion.regionIdentifier, mem.addressSize, mem.valueSize)
        case dataRegion: DataRegion =>
          return SharedMemory(dataRegion.regionIdentifier, mem.addressSize, mem.valueSize)
        case _ =>
      }
    } else if (regions.size > 1) {
      val hasToBeDefined = mmm.getMergedName(regions)
      Logger.debug(s"Multiple regions found for cmd ${cmd} regions size is ${regions.size}")
      regions.head match {
        case stackRegion: StackRegion =>
          return StackMemory(hasToBeDefined, mem.addressSize, mem.valueSize)
        case dataRegion: DataRegion =>
          return SharedMemory(hasToBeDefined, mem.addressSize, mem.valueSize)
        case _ =>
      }
    } else {
      Logger.debug(s"No region found for cmd ${cmd} regions size is ${regions.size}")
    }
    mem
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition): Unit = n match {
    case assign: Assign =>
      assign.rhs = eval(assign.rhs, assign)
    case mAssign: MemoryAssign =>
      mAssign.mem = renameMemory(mAssign.mem, mAssign)
      mAssign.index = eval(mAssign.index, mAssign)
      mAssign.value = eval(mAssign.value, mAssign)
    case assert: Assert =>
      assert.body = eval(assert.body, assert)
    case assume: Assume =>
      assume.body = eval(assume.body, assume)
    case _ => // ignore other kinds of nodes
  }

  def transformMemorySections(memorySegment: ArrayBuffer[MemorySection]): ArrayBuffer[MemorySection] = {
    val newArrayBuffer = ArrayBuffer.empty[MemorySection]
    for (mem <- memorySegment) {
      val regions = mmm.findDataObject(mem.address)
      if (regions.size == 1) {
        newArrayBuffer += MemorySection(regions.head.regionIdentifier, mem.address, mem.size, mem.bytes)
        Logger.debug(s"Region ${regions.get.regionIdentifier} found for memory section ${mem.address}")
      } else {
        newArrayBuffer += mem
        Logger.debug(s"No region found for memory section ${mem.address}")
      }
    }
    newArrayBuffer
  }
}