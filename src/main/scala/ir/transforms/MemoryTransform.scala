package ir.transforms

import analysis.data_structure_analysis.{DSFlag, IntervalCell, IntervalDSA, IntervalGraph, isPlaceHolder}
import ir.cilvisitor.{CILVisitor, ChangeTo, DoChildren, SkipChildren, VisitAction}
import ir.{LocalAssign, LocalVar, Memory, MemoryAssign, MemoryLoad, MemoryStore, Procedure, Register, SharedMemory, Statement}

class MemoryTransform(dsa: Map[Procedure, IntervalGraph]) extends CILVisitor {

  val revEdges = dsa.map((proc, graph) => (proc, IntervalDSA.getPointers(graph)))

  def hasUniquePointer(cell: IntervalCell): Boolean = {
    revEdges(cell.node.graph.proc).get(cell) match
      case Some(value) =>  value.size == 1
      case None => true // skipped due to assignment of non-address
  }

  def joinFlags(pointers: Iterable[IntervalCell]): DSFlag = {
    val flag = DSFlag()
    pointers.foreach(c => flag.join(c.node.flags))
    flag
  }

  def cellsToName(cells: IntervalCell*): String = {
    cells.flatMap(i =>
        i.node.bases.keySet
          .filterNot(isPlaceHolder)
          .map(base => (base, i.node.get(i.interval).interval.move(f => f - i.node.bases(base))))
      )
      .mkString("|")
  }

  def isGlobal(flag: DSFlag): Boolean = {
    flag.global && !flag.stack && !flag.heap
  }

  def isLocal(flag: DSFlag): Boolean = {
    !flag.global && flag.stack && !flag.heap
  }

  override def vstmt(e: Statement) = {
    val proc = e.parent.parent
    if dsa.contains(proc) then
      e match
        case load: MemoryLoad =>
          val indices = dsa(e.parent.parent).exprToCells(load.index).toSeq
          if indices.nonEmpty then {
            assert(indices.forall(_.hasPointee))
            assert(indices.map(_.getPointee).size == 1, s"${indices.map(_.getPointee).size}, $load")
            val flag = joinFlags(indices)
            val value = indices.map(_.getPointee).head
            val varName = cellsToName(indices:_*)

            if isGlobal(flag) && hasUniquePointer(value) then
              ChangeTo(List(LocalAssign(load.lhs, Register(varName, load.size), load.label)))
            else if isLocal(flag) && hasUniquePointer(value) then
              ChangeTo(List(LocalAssign(load.lhs, LocalVar(varName, load.lhs.getType), load.label)))
            else
              val newMem = SharedMemory(cellsToName(value), value.interval.size.getOrElse(0), load.mem.valueSize)
              val newLoad = MemoryLoad(load.lhs, newMem, load.index, load.endian, load.size,  load.label)
              ChangeTo(List(newLoad))
          } else SkipChildren()


        case store: MemoryStore =>
          val indices = dsa(e.parent.parent).exprToCells(store.index).toSeq
          if indices.nonEmpty then {
            assert(indices.forall(_.hasPointee))
            assert(indices.map(_.getPointee).size == 1)
            assert(indices.size == 1)
            val flag = joinFlags(indices)
            val content = indices.map(_.getPointee).head
            val varName = cellsToName(indices:_*)
            if isGlobal(flag) && hasUniquePointer(content) then
              ChangeTo(List(MemoryAssign(Register(varName, store.size), store.value, store.label)))
            else if isLocal(flag) && hasUniquePointer(content)then
              ChangeTo(List(LocalAssign(LocalVar(varName, store.value.getType), store.value, store.label, false)))
            else
              val newMem = SharedMemory(cellsToName(content), content.interval.size.getOrElse(0), store.mem.valueSize)
              val newStore = MemoryStore(newMem, store.index, store.value, store.endian, store.size, store.label)
              ChangeTo(List(newStore))
          } else SkipChildren()
        case _ => SkipChildren()
    else SkipChildren()
  }
}
