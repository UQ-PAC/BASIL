package ir.invariant

import ir.*
import util.Logger

import scala.collection.mutable

def readUninitialised(p: Procedure): Boolean = {
  ir.transforms.reversePostOrder(p)

  val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
  worklist.addAll(p.blocks)

  var init = Set[Variable]() ++ p.formalInParam
  var readUninit = List[(Command, Set[Variable])]()

  def check(a: Command) = {
    val free = freeVarsPos(a).filter(_.isInstanceOf[LocalVar]) -- init
    if (free.size > 0) {
      readUninit = (a, free) :: readUninit
    }
  }

  while (worklist.nonEmpty) {
    val b = worklist.dequeue()
    b.statements.foreach {
      case a: Assign => {
        check(a)
        init = init ++ a.assignees
      }
      case o => {
        check(o)
      }
    }
    check(b.jump)
  }

  if (readUninit.size > 0) {
    Logger.error(p.name)
    val msg = readUninit
      .map { case (s, vars) =>
        s"   ${vars.mkString(", ")} uninitialised in statement $s"
      }
      .mkString("\n")
    Logger.error(msg)
    true
  } else {
    false
  }
}

def readUninitialised(p: Program): Boolean = {
  val r = p.procedures.map(readUninitialised)
  r.forall(x => !x)
}
