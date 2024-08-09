package ir.transforms
import ir._
import collection.mutable

// This shouldn't be run before indirect calls are resolved
def stripUnreachableFunctions(p: Program, depth: Int = Int.MaxValue): Unit = {
  val procedureCalleeNames = p.procedures.map(f => f.name -> f.calls.map(_.name)).toMap

  val toVisit: mutable.LinkedHashSet[(Int, String)] = mutable.LinkedHashSet((0, p.mainProcedure.name))
  var reachableFound = true
  val reachableNames = mutable.HashMap[String, Int]()
  while (toVisit.nonEmpty) {
    val next = toVisit.head
    toVisit.remove(next)

    if (next._1 <= depth) {

      def addName(depth: Int, name: String): Unit = {
        val oldDepth = reachableNames.getOrElse(name, Integer.MAX_VALUE)
        reachableNames.put(next._2, if depth < oldDepth then depth else oldDepth)
      }
      addName(next._1, next._2)

      val callees = procedureCalleeNames(next._2)

      toVisit.addAll(callees.diff(reachableNames.keySet).map(c => (next._1 + 1, c)))
      callees.foreach(c => addName(next._1 + 1, c))
    }
  }
  p.procedures = p.procedures.filter(f => reachableNames.keySet.contains(f.name))

  for (elem <- p.procedures.filter(c => c.calls.exists(s => !p.procedures.contains(s)))) {
    // last layer is analysed only as specifications so we remove the body for anything that calls
    // a function we have removed

    elem.clearBlocks()
  }
}
