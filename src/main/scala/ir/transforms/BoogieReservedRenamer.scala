package ir.transforms
import ir.*

import cilvisitor.*

/** Prevents strings in 'reserved' from being used as the name of anything by adding a '#' to the start. Useful for
  * avoiding Boogie's reserved keywords.
  */
class BoogieReservedRenamer(reserved: Set[String]) extends CILVisitor {

  override def vprog(node: Program) = {
    for (section <- node.usedMemory.values) {
      section.region match {
        case Some(region) if reserved.contains(region.name) =>
          region.name = s"#${region.name}"
        case _ =>
      }
    }
    DoChildren()
  }

  override def vlvar(v: Variable) = v match {
    case (node: LocalVar) if (reserved.contains(node.varName)) =>
      ChangeTo(node.copy(varName = s"#${node.varName}"))
    case _ => SkipChildren()
  }
  override def vrvar(v: Variable) = v match {
    case (node: LocalVar) if (reserved.contains(node.varName)) =>
      ChangeTo(node.copy(varName = s"#${node.varName}"))
    case _ => SkipChildren()
  }

  override def vmem(m: Memory) = m match {
    case (node: StackMemory) if (reserved.contains(node.name)) =>
      ChangeTo(node.copy(name = s"#${node.name}"))
    case (node: SharedMemory) if (reserved.contains(node.name)) =>
      ChangeTo(node.copy(name = s"#${node.name}"))
    case _ => SkipChildren()
  }

  override def vproc(node: Procedure) = {
    if (reserved.contains(node.procName)) {
      node.procName = s"#${node.procName}"
    }
    DoChildren()
  }

}
