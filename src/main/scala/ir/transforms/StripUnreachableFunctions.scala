package ir.transforms

import ir.*
import util.Logger
import util.assertion.*

import collection.mutable

// This shouldn't be run before indirect calls are resolved
def stripUnreachableFunctions(p: Program, depth: Int): Unit = {
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
  debugAssert(invariant.cfgCorrect(p))
  val removed = p.procedures.filterNot(f => reachableNames.keySet.contains(f)).toSet
  // p.procedures = p.procedures.filter(f => reachableNames.keySet.contains(f.name))
  for (proc <- removed) {
    p.removeProcedure(proc)
  }

  for (elem <- p.procedures.filter(c => c.calls.exists(s => removed.contains(s)))) {
    // last layer is analysed only as specifications so we remove the body for anything that calls
    // a function we have removed

    elem.clearBlocks()
    debugAssert(elem.entryBlock.isEmpty)
    debugAssert(elem.returnBlock.isEmpty)

  }
  debugAssert(invariant.blocksUniqueToEachProcedure(p))
  debugAssert(invariant.cfgCorrect(p))

}

def getStripUnreachableFunctionsTransform(depth: Int): Transform =
  Transform(
    "StripUnreachableFunctions",
    (ctx, man) => {
      val before = ctx.program.procedures.size
      stripUnreachableFunctions(ctx.program, depth)
      Logger.info(
        s"[!] Removed ${before - ctx.program.procedures.size} functions (${ctx.program.procedures.size} remaining)"
      )

      /* Fixme: Since refactoring RunUtils, the following code runs when this transform is invoked by the
      loadAndTranslate function, whereas it used to only run when invoked by the prepareForTranslation function. I don't
      know if this is problematic. */
      val dupProcNames = ctx.program.procedures.groupBy(_.name).filter((_, p) => p.size > 1).toList.flatMap(_(1))
      assert(dupProcNames.isEmpty)

      ctx.program.procedures.foreach(p =>
        p.blocks.foreach(b => {
          b.jump match {
            case GoTo(targs, _) if targs.isEmpty =>
              Logger.warn(s"block ${b.label} in subroutine ${p.name} has no outgoing edges")
            case _ => ()
          }
        })
      )
      man.ClobberAll
    },
    notice = "Stripping Unreachable"
  )
