package ir.transforms

import analysis.data_structure_analysis.{IntervalGraph, isPlaceHolder}
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
            val value = indices.map(_.getPointee).head
            val varName = value.node.bases.keySet.filterNot(isPlaceHolder).toString() + value.interval
            val rhs =
              if value.node.flags.global || value.node.flags.heap then Register(varName, load.size)
              else LocalVar(varName, load.lhs.getType)
            val localAssign = LocalAssign(load.lhs, rhs, load.label)
            ChangeTo(List(localAssign))
          } else SkipChildren()
        case store: MemoryStore =>
          val indices = dsa(e.parent.parent).exprToCells(store.index)
          if indices.nonEmpty then {
            assert(indices.forall(_.hasPointee))
            assert(indices.map(_.getPointee).size == 1)
            val content = indices.map(_.getPointee).head
            val varName = content.node.bases.keySet.filterNot(isPlaceHolder).toString() + content.interval
            val assign = if content.node.flags.global || content.node.flags.heap then
              val lhs = Register(varName, store.size)
              MemoryAssign(lhs, store.value, store.label)
            else
              val lhs = LocalVar(varName, store.value.getType)
              LocalAssign(lhs, store.value, store.label)
            ChangeTo(List(assign))
          } else SkipChildren()
        case _ => SkipChildren()
    else SkipChildren()
  }
}
