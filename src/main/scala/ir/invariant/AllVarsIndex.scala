package ir.invariant
import ir._
import util.Logger

def reachableEntry(p: Program) = {

  var old = Set[Procedure]()
  var entry = Set(p.mainProcedure)

  while (old != entry) {
    old = entry
    entry = entry ++ entry.flatMap(_.calls)
  }

  entry
}

/** The DSA local block filling should set all variables to have index > 0 if the flow is correct. Variables with index
  * 0 will be uninitialised.
  */
def allVariablesAssignedIndex(p: Program): Boolean = {
  val detached = p.procedures.map(getDetachedBlocks(_)).toSet
  val badBlocks = detached.flatMap(_.reachableFromBlockEmptyPred)

  val detachedHeaders = detached.flatMap(_.blocksEmptyPred)
  if (detachedHeaders.nonEmpty) {
    Logger.warn(s"Blocks unreachable from procedure entry: ${detachedHeaders.map(_.label)}")
  }

  val reachable = reachableEntry(p)

  val fails = p.collect {
    case c: Command if !(badBlocks.contains(c.parent)) && reachable.contains(c.parent.parent) =>
      freeVarsPos(c)
        .filterNot(v =>
          v match {
            case l: LocalVar if (!l.name.endsWith("_in")) => l.index > 0
            case _ => true
          }
        )
        .map(v => (c, v))
        .toSet
  }.flatten
  for ((command, variable) <- fails) {
    Logger.error(s"Unindexed variable $variable in ${command.parent.parent.name}::${command.parent.label}")
  }
  fails.isEmpty
}
