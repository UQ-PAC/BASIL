package ir.transforms

import analysis.data_structure_analysis.*
import ir.cilvisitor.{CILVisitor, ChangeTo, DoChildren, SkipChildren, VisitAction}
import ir.*
import util.Counter

import scala.collection.mutable

class MemoryTransform(dsa: Map[Procedure, IntervalGraph], globals: Map[IntervalNode, IntervalNode]) extends CILVisitor {
  val counter: Counter = Counter()
  val memVals = mutable.Map[IntervalCell, String]()
  val revEdges: Map[Procedure, Map[IntervalCell, Set[IntervalCell]]] = Map.empty
//    dsa.map((proc, graph) => (proc, IntervalDSA.getPointers(graph)))
  val interProcCells: Map[IntervalCell, Set[IntervalCell]] = Map.empty // computeRelations()

  def isGlobal(flag: DSFlag): Boolean = {
    flag.global && !flag.stack && !flag.heap
  }

  def isLocal(flag: DSFlag): Boolean = {
    !flag.global && flag.stack && !flag.heap
  }

  def scalarName(index: IntervalCell, proc: Option[Procedure] = None) = {
    proc match
      case Some(value) =>
        s"Stack_${index.interval.move(i => i - index.node.bases(Stack(value)).head)}".replace("-", "n")
      case None => s"Global_${index.interval.move(i => i - index.node.bases(Global).head)}"
  }

  override def vstmt(e: Statement) = {
    val proc = e.parent.parent
    if dsa.contains(proc) then
      e match
        case load: MemoryLoad =>
          val indices = dsa(proc).exprToCells(load.index).map(dsa(proc).get).toSeq
          if indices.size == 1 then {
            assert(indices.map(_.getPointee).toSet.size == 1, s"$proc, ${indices.map(_.getPointee).size}, $load")
            val index = indices.head
            val flag = index.node.flags
            val value = index.getPointee
            if isGlobal(flag) && !index.node.isCollapsed then
              ChangeTo(List(LocalAssign(load.lhs, Register(scalarName(index), load.size), load.label)))
            else if isLocal(flag) && !index.node.isCollapsed && !flag.escapes && index.node.bases.contains(Stack(proc))
            then
              ChangeTo(
                List(LocalAssign(load.lhs, LocalVar(scalarName(index, Some(proc)), load.lhs.getType), load.label))
              )
            else if !flag.escapes || isGlobal(flag) then
              val memName =
                if isGlobal(flag) then "Global"
                else if isLocal(flag) then "Stack"
                else
                  memVals.getOrElseUpdate(
                    globals.getOrElse(index.node, index.node).get(index.interval),
                    s"mem_${counter.next()}"
                  )
              val newMem = SharedMemory(memName, load.mem.addressSize, load.mem.valueSize)
              val newLoad = MemoryLoad(load.lhs, newMem, load.index, load.endian, load.size, load.label)
              ChangeTo(List(newLoad))
            else SkipChildren()
          } else SkipChildren()

        case store: MemoryStore =>
          val indices = dsa(proc).exprToCells(store.index).map(dsa(proc).get).toSeq
          if indices.size == 1 then {
            assert(indices.map(_.getPointee).toSet.size == 1)
            val index = indices.head
            val flag = index.node.flags
            val content = index.getPointee
            if isGlobal(flag) && !index.node.isCollapsed then
              ChangeTo(List(MemoryAssign(Register(scalarName(index), store.size), store.value, store.label)))
            else if isLocal(flag) && !index.node.isCollapsed && !flag.escapes && index.node.bases.contains(Stack(proc))
            then
              ChangeTo(
                List(
                  LocalAssign(LocalVar(scalarName(index, Some(proc)), store.value.getType), store.value, store.label)
                )
              )
            else if !flag.escapes || isGlobal(flag) then
              val memName =
                if isGlobal(flag) then "Global"
                else if isLocal(flag) then "Stack"
                else
                  memVals.getOrElseUpdate(
                    globals.getOrElse(index.node, index.node).get(index.interval),
                    s"mem_${counter.next()}"
                  )
              val newMem = SharedMemory(memName, store.mem.addressSize, store.mem.valueSize)
              val newStore = MemoryStore(newMem, store.index, store.value, store.endian, store.size, store.label)
              ChangeTo(List(newStore))
            else // ignore the case where the address escapes
              SkipChildren()
          } else SkipChildren()
        case _ => SkipChildren()
    else SkipChildren()
  }
}
