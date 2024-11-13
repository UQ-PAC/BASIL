package analysis

import ir.eval.BitVectorEval.isNegative
import ir.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Replaces the region access with the calculated memory region.
 */

class MergedRegion(var name: String, val subregions: mutable.Set[MemoryRegion])

class RegionInjector(program: Program, mmm: MemoryModelMap) {
  private val accessToRegion = mutable.Map[Statement, Set[MemoryRegion]]()
  val mergedRegions: mutable.Map[MemoryRegion, MergedRegion] = mutable.Map()

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
          case store: MemoryStore =>
            val newMemory = replaceMemory(store.mem, regionsHead, mergedRegion)
            store.mem = newMemory
          case load: MemoryLoad =>
            val newMemory = replaceMemory(load.mem, regionsHead, mergedRegion)
            load.mem = newMemory
          case _ =>
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

  private def statementToRegions(n: Statement): Set[MemoryRegion] = {
    mmm.getStack(n) ++ mmm.getData(n)
  }

  def visitStatement(n: Statement): Unit = n match {
    case m: MemoryStore =>
      val regions = statementToRegions(m)
      accessToRegion(m) = regions
    case m: MemoryLoad =>
      val regions = statementToRegions(n)
      accessToRegion(n) = regions
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
          val size = region.size.toInt
          val bytes = section.getBytes(region.start, size)
          // should probably check that region is entirely contained within section but shouldn't happen in practice?
          val newSection = MemorySection(region.regionIdentifier, region.start, size, bytes, section.readOnly, Some(mergedRegions(region)))
          program.usedMemory(region.start) = newSection
        case None =>
      }
    }
  }

  def getMergedRegion(address: BigInt): Option[MergedRegion] = {
    val region = mmm.findDataObject(address)
    if (region.isDefined && mergedRegions.contains(region.get)) {
      Some(mergedRegions(region.get))
    } else {
      None
    }
  }
}

class RegionRenamer(memory: Memory) extends Visitor {
  override def visitMemory(node: Memory): Memory = memory
}
