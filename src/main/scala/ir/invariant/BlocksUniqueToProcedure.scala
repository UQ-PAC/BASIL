package ir.invariant
import ir.*
import ir.cilvisitor.*
import util.Logger
import scala.collection.mutable

private class BVis extends CILVisitor {
  var proc: Procedure = null
  var block: Block = null
  val gotoViolations = mutable.Set[GoTo]()
  val blockMultiProcViolations = mutable.Set[Block]()

  override def vproc(p: Procedure) = {
    proc = p
    DoChildren()
  }

  override def vblock(b: Block) = {
    block = b
    if (b.parent != proc) {
      blockMultiProcViolations.add(b)
    }
    DoChildren()
  }

  override def vstmt(s: Statement) = {
    SkipChildren()
  }

  override def vjump(j: Jump) = {
    j match {
      case g @ GoTo(targets, _) if !(targets.forall(t => t.parent == proc)) => gotoViolations.add(g)
      case _ => ()
    }

    SkipChildren()
  }
}

def blockUniqueLabels(p: Program): Boolean = {
  p.procedures.forall(blockUniqueLabels)
}

def blockUniqueLabels(p: Procedure): Boolean = {
  val blockNames = mutable.Set[String]()
  var passed = true

  for (block <- p.blocks) {
    if (blockNames.contains(block.label)) {
      passed = false
      Logger.error("Duplicate block name: " + block.label)
    }
    blockNames.add(block.label)
  }
  passed
}

def blocksUniqueToEachProcedure(p: Program): Boolean = {
  val v = BVis()
  visit_prog(v, p)
  for (g <- v.gotoViolations) {
    Logger.error(s"$g has target outside parent procedure ${g.parent.parent.name}")
  }

  for (b <- v.blockMultiProcViolations) {
    Logger.error(s"block ${b.label} is referenced in multiple procedures.")
  }

  v.gotoViolations.isEmpty && v.blockMultiProcViolations.isEmpty
}
