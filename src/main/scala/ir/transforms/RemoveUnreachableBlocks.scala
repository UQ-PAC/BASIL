package ir.transforms

import ir.*
import ir.cilvisitor.*

import analysis.{LatticeSet}

/**
 * Find blocks which are mentioned syntactically within a given procedure.
 *
 * Includes the entry block, and overapproximates in the presence of indirect calls.
 */
class FindGotoTargets extends CILVisitor {

  var keep: LatticeSet[Block] = LatticeSet.Bottom()

  override def vproc(p: Procedure) = {
    keep = keep ++ p.entryBlock
    DoChildren()
  }

  override def vstmt(s: Statement) = {
    s match {
      case IndirectCall(_, _) => keep = LatticeSet.Top()
      case _ => ()
    }
    SkipChildren()
  }

  override def vjump(s: Jump) = {
    s match {
      case GoTo(targs, _) => keep = keep ++ targs
      case _ => ()
    }
    SkipChildren()
  }
}

/**
 * Very coarse transform pass to remove obviously unreachable blocks.
 *
 * This only removes in the most simple cases.
 * Does NOT remove:
 * - mutually-recursive unreachable blocks,
 * - blocks only reachable from unreachable blocks.
 * - any blocks in the presence of indirect calls.
 */
object RemoveUnreachableBlocks {
  def apply(p: Procedure) = {
    val vis = FindGotoTargets()
    visit_proc(vis, p)
    val keepBlocks = vis.keep
    p.blocks.toList.filterNot(keepBlocks.contains).foreach(p.removeBlocks)
  }
}
