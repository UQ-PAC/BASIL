package ir.invariant

import ir.*
import util.Logger

import scala.collection.mutable

class ReadUninitialised() {

  var init = Set[Variable]()
  var readUninit = List[(Command, Set[Variable])]()

  final def check(a: Command) = {
    val free = freeVarsPos(a).filter(_.isInstanceOf[LocalVar]) -- init
    if (free.size > 0) {
      readUninit = (a, free) :: readUninit
    }
  }

  final def readUninitialised(b: Iterable[Statement]): Boolean = {
    val i = readUninit.size
    b.foreach {
      case a: Assign => {
        check(a)
        init = init ++ a.assignees
      }
      case o => {
        check(o)
      }
    }
    i != readUninit.size
  }

  final def readUninitialised(b: Block): Boolean = {
    val i = readUninit.size
    readUninitialised(b.statements)
    check(b.jump)
    i != readUninit.size
  }

  final def getResult(): Option[String] = {
    if (readUninit.size > 0) {
      val msg = readUninit
        .map { case (s, vars) =>
          s"   ${vars.mkString(", ")} uninitialised in statement $s in block ${s.parent}"
        }
        .mkString("\n")
      Some(msg)
    } else {
      None
    }
  }

  final def readUninitialised(p: Procedure): Boolean = {
    init = init ++ p.formalInParam

    ir.transforms.reversePostOrder(p)

    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
    worklist.addAll(p.blocks)

    while (worklist.nonEmpty) {
      val b = worklist.dequeue()
      readUninitialised(b)
    }
    getResult().map(e => Logger.error(p.name + "\n" + e)).isDefined
  }

}

def readUninitialised(p: Program): Boolean = {
  val r = p.procedures.map(p => ReadUninitialised().readUninitialised(p))
  r.forall(x => !x)
}
