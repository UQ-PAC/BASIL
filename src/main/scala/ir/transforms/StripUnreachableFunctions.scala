package ir.transforms
import ir.*
import collection.mutable

// This shouldn't be run before indirect calls are resolved
def stripUnreachableFunctions(p: Program, depth: Int = Int.MaxValue): Unit = {
  val procedureCalleeNames = p.procedures.map(f => f -> f.calls).toMap

  val toVisit: mutable.LinkedHashSet[(Int, Procedure)] = mutable.LinkedHashSet((0, p.mainProcedure))
  var reachableFound = true
  val reachableNames = mutable.HashMap[Procedure, Int]()
  while (toVisit.nonEmpty) {
    val next = toVisit.head
    toVisit.remove(next)

    if (next._1 <= depth) {

      def addName(depth: Int, name: Procedure): Unit = {
        val oldDepth = reachableNames.getOrElse(name, Integer.MAX_VALUE)
        reachableNames.put(next._2, if depth < oldDepth then depth else oldDepth)
      }
      addName(next._1, next._2)

      val callees = procedureCalleeNames(next._2)

      toVisit.addAll(callees.diff(reachableNames.keySet).map(c => (next._1 + 1, c)))
      callees.foreach(c => addName(next._1 + 1, c))
    }
  }
  assert(invariant.cfgCorrect(p))
  val removed = p.procedures.filterNot(f => reachableNames.keySet.contains(f)).toSet
  // p.procedures = p.procedures.filter(f => reachableNames.keySet.contains(f.name))
  for (proc <- removed) {
    p.removeProcedure(proc)
  }

  for (elem <- p.procedures.filter(c => c.calls.exists(s => removed.contains(s)))) {
    // last layer is analysed only as specifications so we remove the body for anything that calls
    // a function we have removed

    elem.clearBlocks()
    assert(elem.entryBlock.isEmpty)
    assert(elem.returnBlock.isEmpty)

  }
  assert(invariant.blocksUniqueToEachProcedure(p))
  assert(invariant.cfgCorrect(p))

}
