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

  /**
   * In expressions that have accesses within a region, we need to relocate
   * the base address to the actual address using the relocation table.
   * MUST RELOCATE because MMM iterate to find the lowest address
   * TODO: May need to iterate over the relocation table to find the actual address
   *
   * @param address
   * @param globalOffsets
   * @return BitVecLiteral: the relocated address
   */
  def relocatedBase(address: BigInt, globalOffsets: Map[BigInt, BigInt]): BitVecLiteral = {
    val tableAddress = globalOffsets.getOrElse(address, address)
    // this condition checks if the address is not layered and returns if it is not
    if (tableAddress != address && !globalOffsets.contains(tableAddress)) {
      return BitVecLiteral(address, 64)
    }
    BitVecLiteral(tableAddress, 64)
  }

  /**
   * Used to reduce an expression that may be a sub-region of a memory region.
   * Pointer reduction example:
   * R2 = R31 + 20
   * Mem[R2 + 8] <- R1
   *
   * Steps:
   * 1) R2 = R31 + 20         <- ie. stack access (assume R31 = stackPointer)
   * ↓
   * R2 = StackRegion("stack_1", 20)
   *
   * 2) Mem[R2 + 8] <- R1     <- ie. memStore
   * ↓
   * (StackRegion("stack_1", 20) + 8) <- R1
   * ↓
   * MMM.get(20 + 8) <- R1
   *
   * @param binExpr
   * @param n
   * @return Set[MemoryRegion]: a set of regions that the expression may be pointing to
   */
  def reducibleToRegion(binExpr: BinaryExpr, n: Command): Set[MemoryRegion] = {
    var reducedRegions = Set.empty[MemoryRegion]
    binExpr.arg1 match {
      case variable: Variable =>
        val b = evaluateExpression(binExpr, constantProp(n))
          if (b.isDefined) {
          val region = mmm.findDataObject(b.get.value)
          reducedRegions = reducedRegions ++ region
        }
        if (reducedRegions.nonEmpty) {
          return reducedRegions
        }
        val ctx = getUse(variable, n, reachingDefs)
        for (i <- ctx) {
          if (i != n) { // handles loops (ie. R19 = R19 + 1) %00000662 in jumptable2
            val regions = i.rhs match {
              case loadL: MemoryLoad =>
                val foundRegions = exprToRegion(loadL.index, i)
                val toReturn = mutable.Set[MemoryRegion]().addAll(foundRegions)
                for {
                  f <- foundRegions
                } {
                  // TODO: Must enable this (probably need to calculate those contents beforehand)
//                  if (memoryRegionContents.contains(f)) {
//                    memoryRegionContents(f).foreach {
//                      case b: BitVecLiteral =>
//                      //                        val region = mmm.findDataObject(b.value)
//                      //                        if (region.isDefined) {
//                      //                          toReturn.addOne(region.get)
//                      //                        }
//                      case r: MemoryRegion =>
//                        toReturn.addOne(r)
//                        toReturn.remove(f)
//                    }
//                  }
                }
                toReturn.toSet
              case _: BitVecLiteral =>
                Set.empty[MemoryRegion]
              case _ =>
                //println(s"Unknown expression: ${i}")
                //println(ctx)
                exprToRegion(i.rhs, i)
            }
            val result = evaluateExpression(binExpr.arg2, constantProp(n))
            if (result.isDefined) {
              val b = result.get
              for {
                r <- regions
              } {
                r match {
                  case stackRegion: StackRegion =>
                    //println(s"StackRegion: ${stackRegion.start}")
                    //println(s"BitVecLiteral: ${b}")
                    //if (b.size == stackRegion.start.size) { TODO: Double check why this is needed
                    val nextOffset = bitVectorOpToBigIntOp(binExpr.op, stackRegion.start, b.value)
                    reducedRegions ++= exprToRegion(BinaryExpr(binExpr.op, stackPointer, BitVecLiteral(nextOffset, 64)), n)
                  //}
                  case dataRegion: DataRegion =>
                    //val nextOffset = BinaryExpr(binExpr.op, relocatedBase(dataRegion.start, globalOffsets), b)
                    val nextOffset = bitVectorOpToBigIntOp(binExpr.op, dataRegion.start, b.value)
                    reducedRegions ++= exprToRegion(BitVecLiteral(nextOffset, 64), n)
                  case _ =>
                }
              }
            }
          }
        }
      case _ =>
    }
    reducedRegions
  }

  /**
   * Finds a region for a given expression using MMM results
   *
   * @param expr
   * @param n
   * @return Set[MemoryRegion]: a set of regions that the expression may be pointing to
   */
  def exprToRegion(expr: Expr, n: Command): Set[MemoryRegion] = {
    var res = Set[MemoryRegion]()
    mmm.popContext()
    mmm.pushContext(IRWalk.procedure(n).name)
    expr match { // TODO: Stack detection here should be done in a better way or just merged with data
      case binOp: BinaryExpr if binOp.arg1 == stackPointer =>
        val b = evaluateExpression(binOp.arg2, constantProp(n))
        if (b.isDefined) {
          if (isNegative(b.get)) {
            val region = mmm.findStackObject(0)
            if (region.isDefined) {
              res = res + region.get
            }
          }
          val region = mmm.findStackObject(b.get.value)
          if (region.isDefined) {
            res = res + region.get
          }
        }
      case binaryExpr: BinaryExpr =>
        res ++= reducibleToRegion(binaryExpr, n)
      case v: Variable if v == stackPointer =>
        res ++= mmm.findStackObject(0)
      case v: Variable =>
        val b = evaluateExpression(expr, constantProp(n))
        if (b.isDefined) {
          Logger.debug("BitVecLiteral: " + b)
          val region = mmm.findDataObject(b.get.value)
          if (region.isDefined) {
            res += region.get
          }
        }
        if (res.isEmpty) {
          val ctx = getDefinition(v, n, reachingDefs)
          for (i <- ctx) {
            i.rhs match {
              case be: BinaryExpr =>
                res = res ++ exprToRegion(eval(i.rhs, i), n)
              case _ =>
            }
          }
        }

        if (res.isEmpty) { // may be passed as param
          val ctx = getUse(v, n, reachingDefs)
          for (i <- ctx) {
            i.rhs match {
              case load: MemoryLoad => // treat as a region
                res ++= exprToRegion(load.index, i)
              case binaryExpr: BinaryExpr =>
                res ++= reducibleToRegion(binaryExpr, i)
              case _ => // also treat as a region (for now) even if just Base + Offset without memLoad
                res ++= exprToRegion(i.rhs, i)
            }
          }
        }
      case load: MemoryLoad => // treat as a region
        res ++= exprToRegion(load.index, n)
      case _ =>
        val b = evaluateExpression(expr, constantProp(n))
        if (b.isDefined) {
          Logger.debug("BitVecLiteral: " + b)
          val region = mmm.findDataObject(b.get.value)
          if (region.isDefined) {
            res += region.get
          }
        }
    }
    res
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
        MemoryLoad(renameMemory(mem, index, cmd), eval(index, cmd), endian, size)
      case variable: Variable => variable // ignore variables
  }

  def renameMemory(mem: Memory, expr: Expr, cmd : Command): Memory = {
    val regions = exprToRegion(eval(expr, cmd), cmd)
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
      throw RuntimeException("Multiple regions found for memory")
//      mmm.mergeRegions(regions) match {
//        case stackRegion: StackRegion =>
//          return StackMemory(stackRegion.regionIdentifier, mem.addressSize, mem.valueSize)
//        case dataRegion: DataRegion =>
//          return SharedMemory(dataRegion.regionIdentifier, mem.addressSize, mem.valueSize)
//        case _ =>
//      }
    } else {
      Logger.debug(s"Mem CMD is: ${cmd}")
      Logger.debug(s"No region found for expr ${expr} regions size is ${regions.size}")
    }
    mem
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition): Unit = n match {
    case assign: Assign =>
      assign.rhs = eval(assign.rhs, assign)
    case mAssign: MemoryAssign =>
      mAssign.mem = renameMemory(mAssign.mem, mAssign.index, mAssign)
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