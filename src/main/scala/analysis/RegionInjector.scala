package analysis

import analysis.BitVectorEval.isNegative
import ir.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Replaces the region access with the calculated memory region.
 */

class MergedRegion(var name: String, val subregions: mutable.Set[MemoryRegion])

class RegionInjector(program: Program,
                     constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                     mmm: MemoryModelMap,
                     reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
                    ) {
  private val stackPointer = Register("R31", 64)

  val accessToRegion = mutable.Map[Statement, Set[MemoryRegion]]()
  val loadToMemory = mutable.Map[Statement, Memory]()
  val mergedRegions = mutable.Map[MemoryRegion, MergedRegion]()

  def nodeVisitor(): Unit = {
    // visit reachable procedures
    val queue = mutable.Queue[Procedure]()
    val visited = mutable.Set[Procedure]()
    queue.enqueue(program.mainProcedure)

    while (queue.nonEmpty) {
      val procedure = queue.dequeue()
      for {
        b <- procedure.blocks
        s <- b.statements
      } {
        visitStatement(s)
      }
      visited.add(procedure)
      for (call <- procedure.calls) {
        if (!queue.contains(call) && !visited.contains(call)) {
          queue.enqueue(call)
        }
      }
    }

    for (access <- accessToRegion.keys) {
      val regions = accessToRegion(access)
      if (regions.isEmpty) {
        //throw Exception("no regions found for " + access)
      } else {
        mergeRegions(regions)
      }
    }

    // rename all regions
    renameMemory()

    transformMemorySections(program)
  }

  def mergeRegions(regions: Set[MemoryRegion]): Unit = {
    // TODO need to check that all regions are the same type
    val oldMergedRegions = regions.flatMap(r => mergedRegions.get(r))
    if (oldMergedRegions.nonEmpty) {
      // TODO rename in sensible deterministic way
      val oldRegion = oldMergedRegions.head
      for (o <- oldMergedRegions.tail) {
        oldRegion.subregions.addAll(o.subregions)
      }
      oldRegion.subregions.addAll(regions)
      for (o <- oldRegion.subregions) {
        mergedRegions(o) = oldRegion
      }
    } else {
      // TODO give sensible deterministic name
      val mergedRegion = MergedRegion(regions.head.regionIdentifier, mutable.Set())
      mergedRegion.subregions.addAll(regions)
      for (m <- mergedRegion.subregions) {
        mergedRegions(m) = mergedRegion
      }
    }
  }

  def renameMemory(): Unit = {
    for (access <- accessToRegion.keys) {
      // all regions associated with an access should have same merged region so no need to check others
      val regions = accessToRegion(access)
      if (regions.nonEmpty) {
        val regionsHead = regions.head
        val mergedRegion = mergedRegions(regionsHead)

        access match {
          case store: MemoryAssign =>
            val newMemory = replaceMemory(store.mem, regionsHead, mergedRegion)
            store.mem = newMemory
          case _ =>
            val newMemory = replaceMemory(loadToMemory(access), regionsHead, mergedRegion)
            val renamer = RegionRenamer(newMemory)
            renamer.visitStatement(access)
        }
      }

    }
  }

  def replaceMemory(memory: Memory, region: MemoryRegion, mergedRegion: MergedRegion): Memory = {
    region match {
      case _: StackRegion =>
        StackMemory(mergedRegion.name, memory.addressSize, memory.valueSize)
      case _: DataRegion =>
        SharedMemory(mergedRegion.name, memory.addressSize, memory.valueSize)
      case _: HeapRegion =>
        SharedMemory(mergedRegion.name, memory.addressSize, memory.valueSize)
    }
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
      BitVecLiteral(address, 64)
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
        for (i <- ctx if i != n) { // handles loops (ie. R19 = R19 + 1)
          val regions = i.rhs match {
            case loadL: MemoryLoad =>
              val foundRegions = exprToRegion(loadL.index, i)
              val toReturn = mutable.Set[MemoryRegion]().addAll(foundRegions)
              for (f <- foundRegions) {
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
            regions.foreach {
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
                res = res ++ exprToRegion(i.rhs, n)
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


  def visitExpr(expr: Expr, cmd: Statement): Unit = {
    expr match {
      case Extract(_, _, body) =>
        visitExpr(body, cmd)
      case UninterpretedFunction(_, params, _) =>
        params.foreach {
          p => visitExpr(p, cmd)
        }
      case Repeat(_, body) =>
        visitExpr(body, cmd)
      case ZeroExtend(_, body) =>
        visitExpr(body, cmd)
      case SignExtend(_, body) =>
        visitExpr(body, cmd)
      case UnaryExpr(_, arg) =>
        visitExpr(arg, cmd)
      case BinaryExpr(_, arg1, arg2) =>
        visitExpr(arg1, cmd)
        visitExpr(arg2, cmd)
      case m: MemoryLoad =>
        val regions = exprToRegion(m.index, cmd)
        accessToRegion(cmd) = regions
        loadToMemory(cmd) = m.mem
      case _ =>
    }
  }

  def visitStatement(n: Statement): Unit = n match {
    case assign: Assign =>
      visitExpr(assign.rhs, assign)
    case m: MemoryAssign =>
      val regions = exprToRegion(m.index, m)
      accessToRegion(m) = regions
    case assert: Assert =>
      visitExpr(assert.body, assert)
    case assume: Assume =>
      visitExpr(assume.body, assume)
    case _ => // ignore other kinds of nodes
  }

  // replace memory renamer with something that creates map from access to region
  // then handle all the merging required
  // then do the renaming
  // then get regions per procedure, handle initial memory with those

  def transformMemorySections(program: Program): Unit = {
    val dataRegions = mergedRegions.keys.collect { case d: DataRegion => d }

    for (region <- dataRegions) {
      program.initialMemoryLookup(region.start) match {
        case Some(section) =>
          val bytes = section.getBytes(region.start, region.size)
          // should probably check that region is entirely contained within section but shouldn't happen in practice?
          val newSection = MemorySection(region.regionIdentifier, region.start, region.size, bytes, section.readOnly, Some(mergedRegions(region)))
          program.usedMemory(region.start) = newSection
        case None =>
      }
    }
  }
}

class RegionRenamer(memory: Memory) extends Visitor {
  override def visitMemory(node: Memory): Memory = memory
}