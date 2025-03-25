package ir.transforms

import analysis.data_structure_analysis.{DSFlag, IntervalGraph, isPlaceHolder}
import ir.cilvisitor.{CILVisitor, ChangeTo, SkipChildren}
import ir.{LocalAssign, LocalVar, MemoryAssign, MemoryLoad, MemoryStore, Procedure, Register, Statement}

class MemoryTransform(dsa: Map[Procedure, IntervalGraph]) extends CILVisitor {
  override def vstmt(e: Statement) = {
    if dsa.contains(e.parent.parent) then
      e match
        case load: MemoryLoad =>
          val indices = dsa(e.parent.parent).exprToCells(load.index)
          if indices.nonEmpty then {
            assert(indices.forall(_.hasPointee))
            assert(indices.map(_.getPointee).size == 1, s"${indices.map(_.getPointee).size}, $load")
            val flag = DSFlag()
            indices.foreach(c => flag.join(c.node.flags))
            val value = indices.map(_.getPointee).head
            val varName = indices
              .flatMap(i =>
                i.node.bases.keySet
                  .filterNot(isPlaceHolder)
                  .map(base => (base, i.node.get(i.interval).interval.move(f => f - i.node.bases(base))))
              )
              .mkString("|")
            val rhs =
              if flag.global || flag.heap then Register(varName, load.size)
              else LocalVar(varName, load.lhs.getType)
            val localAssign = LocalAssign(load.lhs, rhs, load.label, false)
            ChangeTo(List(localAssign))
          } else SkipChildren()
        case store: MemoryStore =>
          val indices = dsa(e.parent.parent).exprToCells(store.index)
          if indices.nonEmpty then {
            assert(indices.forall(_.hasPointee))
            assert(indices.map(_.getPointee).size == 1)
            assert(indices.size == 1)
            val flag = DSFlag()
            indices.foreach(c => flag.join(c.node.flags))
            val content = indices.map(_.getPointee).head
            val varName = indices
              .flatMap(i =>
                i.node.bases.keySet
                  .filterNot(isPlaceHolder)
                  .map(base => (base, i.node.get(i.interval).interval.move(f => f - i.node.bases(base))))
              )
              .mkString("|")
            val assign = if flag.global || flag.heap then
              val lhs = Register(varName, store.size)
              MemoryAssign(lhs, store.value, store.label)
            else
              val lhs = LocalVar(varName, store.value.getType)
              LocalAssign(lhs, store.value, store.label, false)
            ChangeTo(List(assign))
          } else SkipChildren()
        case _ => SkipChildren()
    else SkipChildren()
  }
}
