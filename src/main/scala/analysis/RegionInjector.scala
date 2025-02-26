package analysis

import analysis.data_structure_analysis.{AddressRange, Cell, Graph, Slice}
import ir.*
import util.Logger

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Replaces the region access with the calculated memory region.
 */

trait MergedRegion {
  var name: String = ""
}

class MergedRegionMRA(private val nameIn: String, val subregions: mutable.Set[MemoryRegion]) extends MergedRegion {
  name = nameIn
}

class MergedRegionDSA(private val nameIn: String, val cell: Cell, val stack: Boolean) extends MergedRegion {
  name = nameIn
}

trait RegionInjector {
  val program: Program
  def getMergedRegion(address: BigInt, size: Int): Option[MergedRegion]
  def injectRegions(): Unit

  def sharedRegions(): Iterable[MergedRegion]
}

class RegionInjectorMRA(override val program: Program, mmm: MemoryModelMap) extends RegionInjector {
  private val mergedRegions: mutable.Map[MemoryRegion, MergedRegionMRA] = mutable.Map()
  private val accessToRegion = mutable.Map[Statement, Set[MemoryRegion]]()

  def injectRegions(): Unit = {
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
        // throw Exception("no regions found for " + access)
      } else {
        mergeRegions(regions)
      }
    }

    // rename all regions
    renameMemory()

    transformMemorySections(program)
  }

  private def mergeRegions(regions: Set[MemoryRegion]): Unit = {
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
      val mergedRegion = MergedRegionMRA(regions.head.regionIdentifier, mutable.Set())
      mergedRegion.subregions.addAll(regions)
      for (m <- mergedRegion.subregions) {
        mergedRegions(m) = mergedRegion
      }
    }
  }

  private def renameMemory(): Unit = {
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

  private def replaceMemory(memory: Memory, region: MemoryRegion, mergedRegion: MergedRegion): Memory = {
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
    mmm.nodeToRegion(n)
  }

  private def visitStatement(n: Statement): Unit = n match {
    case m: MemoryStore =>
      val regions = statementToRegions(m)
      accessToRegion(m) = regions
    case m: MemoryLoad =>
      val regions = statementToRegions(m)
      accessToRegion(m) = regions
    case _ => // ignore other kinds of nodes
  }

  // replace memory renamer with something that creates map from access to region
  // then handle all the merging required
  // then do the renaming
  // then get regions per procedure, handle initial memory with those

  private def transformMemorySections(program: Program): Unit = {
    val dataRegions = mergedRegions.keys.collect { case d: DataRegion => d }

    for (region <- dataRegions) {
      program.initialMemoryLookup(region.start) match {
        case Some(section) =>
          val size = region.size.toInt
          val bytes = section.getBytes(region.start, size)
          // should probably check that region is entirely contained within section but shouldn't happen in practice?
          val newSection = MemorySection(
            region.regionIdentifier,
            region.start,
            size,
            bytes,
            section.readOnly,
            Some(mergedRegions(region))
          )
          program.usedMemory(region.start) = newSection
        case None =>
      }
    }
  }

  override def getMergedRegion(address: BigInt, size: Int): Option[MergedRegionMRA] = {
    val region = mmm.findDataObject(address)
    if (region.isDefined) {
      if (mergedRegions.contains(region.get)) {
        Some(mergedRegions(region.get))
      } else if (region.get.size >= (size / 8)) {
        val newRegion = MergedRegionMRA(region.get.regionIdentifier, mutable.Set(region.get))
        mergedRegions(region.get) = newRegion
        Some(newRegion)
      } else {
        throw Exception(
          s"MMM returned region for $address with size ${region.get.size} bytes which does not match requested size ${size / 8} bytes"
        )
      }
    } else {
      throw Exception(s"failed to find region with address $address of size ${size / 8} bytes")
    }
  }

  override def sharedRegions(): Iterable[MergedRegion] = {
    mergedRegions.collect { case (_: DataRegion | _: HeapRegion, region: MergedRegion) =>
      region
    }
  }
}

class RegionInjectorDSA(override val program: Program, DSATopDown: mutable.Map[Procedure, Graph])
    extends RegionInjector {
  private val mergedRegions: mutable.Map[Cell, MergedRegionDSA] = mutable.Map()

  private var sharedMemoryCounter = 0
  private var stackCounter = 0

  def injectRegions(): Unit = {
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
        s match {
          case store: MemoryStore =>
            val dsg = DSATopDown(procedure)
            val cell = dsg.find(dsg.accessIndexToSlice(store)).cell
            val mergedRegion = if (mergedRegions.contains(cell)) {
              mergedRegions(cell)
            } else {
              val region = createRegion(cell)
              mergedRegions(cell) = region
              region
            }
            val newMemory = replaceMemory(store.mem, mergedRegion)
            store.mem = newMemory
          case load: MemoryLoad =>
            val dsg = DSATopDown(procedure)
            val cell = dsg.find(dsg.accessIndexToSlice(load)).cell
            val mergedRegion = if (mergedRegions.contains(cell)) {
              mergedRegions(cell)
            } else {
              val region = createRegion(cell)
              mergedRegions(cell) = region
              region
            }
            val newMemory = replaceMemory(load.mem, mergedRegion)
            load.mem = newMemory
          case _ =>
        }
      }
      visited.add(procedure)
      for (call <- procedure.calls) {
        if (!queue.contains(call) && !visited.contains(call)) {
          queue.enqueue(call)
        }
      }
    }
    transformMemorySections(program)
  }

  private def createRegion(cell: Cell): MergedRegionDSA = {
    val stack = cell.node.get.flags.stack
    val name = if (stack) {
      stackCounter += 1
      s"stack$$${stackCounter}"
    } else {
      sharedMemoryCounter += 1
      s"mem$$${sharedMemoryCounter}"
    }
    MergedRegionDSA(name, cell, stack)
  }

  private def replaceMemory(memory: Memory, mergedRegion: MergedRegionDSA): Memory = {
    if (mergedRegion.stack) {
      StackMemory(mergedRegion.name, memory.addressSize, memory.valueSize)
    } else {
      SharedMemory(mergedRegion.name, memory.addressSize, memory.valueSize)
    }
  }

  private def transformMemorySections(program: Program): Unit = {
    // if any addressRange entries point to a mergedRegion: grab the bytes

    val dsg = DSATopDown(program.mainProcedure)

    // need to check if nodes match too

    dsg.globalMapping.foreach { (range, field) =>
      val cell = dsg.find(field.node.getCell(field.offset))
      if (mergedRegions.contains(cell)) {
        program.initialMemoryLookup(range.start) match {
          case Some(section) =>
            val size = cell.largestAccessedSize
            val bytes = section.getBytes(range.start, size)
            // should probably check that region is entirely contained within section but shouldn't happen in practice?
            val newSection = MemorySection(
              mergedRegions(cell).name,
              range.start,
              size,
              bytes,
              section.readOnly,
              Some(mergedRegions(cell))
            )
            program.usedMemory(range.start) = newSection
          case None =>
        }
      }
    }
  }

  override def getMergedRegion(address: BigInt, size: Int): Option[MergedRegionDSA] = {
    val dsg = DSATopDown(program.mainProcedure)

    val cells = dsg.globalMapping.keys.flatMap { range =>
      if (address >= range.start && (address < range.end || (range.start == range.end && range.end == address))) {
        val node = dsg.globalMapping(range).node
        val offset = address - range.start
        val cell = node.cells.get(offset)
        cell.map(dsg.find)
      } else {
        None
      }
    }

    if (cells.isEmpty || cells.size > 1) {
      throw Exception(s"failed to find region with address $address of size ${size / 8} bytes")
    } else if (!mergedRegions.contains(cells.head)) {
      val region = createRegion(cells.head)
      mergedRegions(cells.head) = region
      Some(region)
    } else {
      mergedRegions.get(cells.head)
    }
  }

  override def sharedRegions(): Iterable[MergedRegion] = {
    mergedRegions.values.filter(region => !region.stack)
  }

}
