package ir.transforms

import analysis.data_structure_analysis.{DSFlag, IntervalCell, IntervalDSA, IntervalGraph, IntervalNode, isPlaceHolder}
import ir.cilvisitor.{CILVisitor, ChangeTo, DoChildren, SkipChildren, VisitAction}
import ir.*

import scala.collection.mutable

class MemoryTransform(dsa: Map[Procedure, IntervalGraph]) extends CILVisitor {
  val revEdges: Map[Procedure, Map[IntervalCell, Set[IntervalCell]]] =
    dsa.map((proc, graph) => (proc, IntervalDSA.getPointers(graph)))
  val interProcCells = computeRelations()

  def computeRelations(): Map[IntervalCell, Set[IntervalCell]] = {
    val cellMapping = mutable.Map[IntervalCell, Set[IntervalCell]]()
    dsa.foreach((proc, graph) =>
      val callerDSG = dsa(proc)
      proc.foreach {
        case dc: DirectCall if dsa.contains(dc.target) =>
          val calleeDSG = dsa(dc.target)
          dc.actualParams.foreach((formal, actual) =>
            val cells1 = callerDSG.exprToCells(actual)
            val cells2 = calleeDSG.exprToCells(formal)
            cells1.foreach(cell => cellMapping.update(cell, cellMapping.getOrElse(cell, Set.empty) ++ cells2))
            cells2.foreach(cell => cellMapping.update(cell, cellMapping.getOrElse(cell, Set.empty) ++ cells1))
          )
          dc.outParams.foreach((formal, actual) =>
            val cells1 = callerDSG.exprToCells(actual)
            val cells2 = calleeDSG.exprToCells(formal)
            cells1.foreach(cell => cellMapping.update(cell, cellMapping.getOrElse(cell, Set.empty) ++ cells2))
            cells2.foreach(cell => cellMapping.update(cell, cellMapping.getOrElse(cell, Set.empty) ++ cells1))
          )
        case _ =>
      }
    )
    cellMapping.toMap
  }

  def hasUniquePointer(cell: IntervalCell): Boolean = {
    val proc = cell.node.graph.proc
    revEdges(proc).get(cell) match
      case Some(value) if value.size > 1 => false
      case Some(value) if interProcCells.contains(cell) =>
        var seenPointers = value
        var seenCells = Set(cell)
        var num = value.size
        val queue = mutable.Queue().enqueueAll(interProcCells(cell))
        while queue.nonEmpty && num <= 1 do
          val eq = queue.dequeue()
          val pointers = revEdges(eq.node.graph.proc)
            .getOrElse(eq, Set.empty)
            .diff(seenPointers)
            .filterNot(p => interProcCells.getOrElse(p, Set.empty).exists(seenPointers.contains))
          num += pointers.size

        num <= 1
      case _ => true

  }

  def joinFlags(pointers: Iterable[IntervalCell]): DSFlag = {
    val flag = DSFlag()
    pointers.foreach(c => flag.join(c.node.flags))
    flag
  }

  def cellsToName(cells: IntervalCell*): String = {
    cells
      .flatMap(i =>
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
          val indices = dsa(proc).exprToCells(load.index).map(dsa(proc).get).toSeq
          if indices.nonEmpty && indices.forall(_.hasPointee) then {
            assert(indices.map(_.getPointee).size == 1, s"${indices.map(_.getPointee).size}, $load")
            val flag = joinFlags(indices)
            val value = indices.map(_.getPointee).head
            val varName = cellsToName(indices: _*)

            if isGlobal(flag) && hasUniquePointer(value) then
              ChangeTo(List(LocalAssign(load.lhs, Register(varName, load.size), load.label)))
            else if isLocal(flag) && hasUniquePointer(value) then
              ChangeTo(List(LocalAssign(load.lhs, LocalVar(varName, load.lhs.getType), load.label)))
            else
              val newMem = SharedMemory(cellsToName(value), value.interval.size.getOrElse(0), load.mem.valueSize)
              val newLoad = MemoryLoad(load.lhs, newMem, load.index, load.endian, load.size, load.label)
              ChangeTo(List(newLoad))
          } else SkipChildren()

        case store: MemoryStore =>
          val indices = dsa(proc).exprToCells(store.index).map(dsa(proc).get).toSeq
          if indices.nonEmpty && indices.forall(_.hasPointee) then {
            assert(indices.map(_.getPointee).size == 1)
            val flag = joinFlags(indices)
            val content = indices.map(_.getPointee).head
            val varName = cellsToName(indices: _*)
            if isGlobal(flag) && hasUniquePointer(content) then
              ChangeTo(List(MemoryAssign(Register(varName, store.size), store.value, store.label)))
            else if isLocal(flag) && hasUniquePointer(content) then
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
