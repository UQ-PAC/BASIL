package analysis

import ir.{Block, CFGPosition, Call, Command, DirectCall, GoTo, IndirectCall, IntraProcIRCursor, Jump, Procedure, Program, Statement}

import scala.collection.mutable

class IDECache(program: Program) {
  val entryExitMap: BiMap[Procedure, Command] = new BiMap[Procedure, Command]
  val callReturnMap: BiMap[DirectCall, GoTo] = new BiMap[DirectCall, GoTo]
  val callees: mutable.Map[DirectCall, Procedure] = mutable.Map[DirectCall, Procedure]()
  val callers: mutable.Map[Procedure, Set[DirectCall]] = mutable.Map[Procedure, Set[DirectCall]]()
  val afterCall: mutable.Map[Command, Set[GoTo]] = mutable.Map[Command, Set[GoTo]]()
  val retExit: mutable.Map[GoTo, Command] = mutable.Map[GoTo, Command]()
  private var traversed: Set[CFGPosition] = Set()

  def cache() = {
    program.procedures.foreach(
      p => traverse(p)
    )

    callReturnMap.domain.foreach(
      dc => 
        callees.addOne(dc, dc.target)
        val oldCallers = callers.getOrElse(dc.target, Set())
        callers.put(dc.target, oldCallers + dc)
    )

    entryExitMap.forwardMap.foreach(
      (entry, exit) =>
        val afterCalls: mutable.Set[GoTo] = mutable.Set()
        callees.foreach(
          (call, proc) =>
            if (entry == proc) then
              val ret = callReturnMap(call)
              retExit.addOne(ret, exit)
              afterCalls.add(ret)
        )
        afterCall.put(exit, afterCalls.toSet)
    )
  }

  private def traverse(pos: CFGPosition): Unit = {
    if (traversed.contains(pos)) {
      return
    } else {
      traversed = traversed + pos
    }

    pos match
      case c: Command =>
        if (IntraProcIRCursor.succ(c).isEmpty) {
          entryExitMap.addOne(c.parent.parent, c)
        }
        else
          c match
            // A direct call that we care about (it had a return GoTo and the function called is not empty)
            case directCall: DirectCall if directCall.returnTarget.isDefined && directCall.target.blocks.nonEmpty =>
              callReturnMap.addOne(directCall, IntraProcIRCursor.succ(directCall).head.asInstanceOf[GoTo])
            case _ =>
            IntraProcIRCursor.succ(c).foreach(traverse)

      case p => IntraProcIRCursor.succ(p).foreach(traverse)
  }
}
