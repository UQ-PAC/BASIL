package ir.transforms

import ir.*

import scala.collection.mutable

import cilvisitor.*

class StackSubstituter extends CILVisitor {
// FIXME: does this break for programs with loops? need to calculate a fixed-point?
  private val stackPointer = Register("R31", 64)
  private val stackMemory = StackMemory("stack", 64, 8)
  val stackRefs: mutable.Set[Variable] = mutable.Set(stackPointer)

  private def isStackPtr(v: Variable) = {
    (v match {
      case l: LocalVar if l.varName == "R31" => true
      case r: Variable if r.name == "R31" => true
      case _ => false
    }) || stackRefs.contains(v)
  }

  override def vproc(p: Procedure) = {
    stackRefs.clear()
    stackRefs.add(stackPointer)
    stackRefs.add(LocalVar("R31_in", BitVecType(64)))
    stackRefs.add(LocalVar("R31", BitVecType(64)))
    DoChildren()
  }

  override def vstmt(s: Statement) = s match {
    case LocalAssign(lhs, rhs, _) => {
      val assignments = Seq((lhs, rhs))
      for ((lhs, rhs) <- assignments) {
        if (rhs.variables.exists(isStackPtr)) {
          stackRefs.add(lhs)
        } else if (stackRefs.contains(lhs) && lhs.name != stackPointer.name) {
          stackRefs.remove(lhs)
        }
      }
      SkipChildren()
    }
    case node: MemoryLoad => {
      // replace mem with stack in load if index contains stack references
      if (node.index.variables.exists(isStackPtr)) {
        node.mem = stackMemory
      }

      if (stackRefs.contains(node.lhs) && node.lhs != stackPointer) {
        stackRefs.remove(node.lhs)
      }

      SkipChildren()
    }
    case node: MemoryStore if (node.index.variables.exists(isStackPtr)) => {
      node.mem = stackMemory
      SkipChildren()
    }
    case node @ Assert(FApplyExpr("valid", se, _, _), _, _) => {
      // Memory Encoding validity checks shouldn't occur on stack memory
      if se(5).variables.exists(isStackPtr) then {
        ChangeTo(List())
      } else {
        SkipChildren()
      }
    }
    case node @ Assert(FApplyExpr("me_valid_access", se, _, _), _, _) => {
      // Memory Encoding validity checks shouldn't occur on stack memory
      if se(1).variables.exists(isStackPtr) then {
        ChangeTo(List())
      } else {
        SkipChildren()
      }
    }
    case _ => SkipChildren()
  }
}
