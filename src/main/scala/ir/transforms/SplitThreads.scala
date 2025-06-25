package ir.transforms

import analysis.{DataRegion, FlatElement, MemoryRegion, RegisterWrapperEqualSets, getSSADefinition}
import ir._

import scala.collection.mutable

def splitThreads(
  program: Program,
  pointsTo: Map[RegisterWrapperEqualSets, Set[RegisterWrapperEqualSets | MemoryRegion]],
  reachingDefs: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
): Unit = {

  // iterate over all commands - if call is to pthread_create, look up
  program.foreach {
    case d: DirectCall if d.target.procName == "pthread_create" =>
      // R2 should hold the function pointer of the function that begins the thread
      // look up R2 value using points to results
      val R2 = Register("R2", 64)
      val R2Wrapper = RegisterWrapperEqualSets(R2, getSSADefinition(R2, d, reachingDefs))
      val threadTargets = pointsTo(R2Wrapper)

      if (threadTargets.size > 1) {
        // currently can't handle case where the thread created is ambiguous
        throw Exception("can't handle thread creation with more than one possible target")
      }

      if (threadTargets.size == 1) {
        // not trying to untangle the very messy region resolution at present, just dealing with simplest case
        threadTargets.head match {
          case data: DataRegion =>
            val threadEntrance = program.procedures.find(_.name == data.regionIdentifier) match {
              case Some(proc) => proc
              case None => throw Exception("could not find procedure with name " + data.regionIdentifier)
            }
            val thread = ProgramThread(threadEntrance, mutable.LinkedHashSet(threadEntrance), Some(d))
            program.threads.addOne(thread)
          case _ =>
            throw Exception("unexpected non-data region " + threadTargets.head + " as PointsTo result for R2 at " + d)
        }
      }
    case _ =>
  }

  if (program.threads.nonEmpty) {
    val mainThread = ProgramThread(program.mainProcedure, mutable.LinkedHashSet(program.mainProcedure), None)
    program.threads.addOne(mainThread)

    val programProcs = program.procedures

    // do reachability for all threads
    for (thread <- program.threads) {
      val reachable = thread.entry.reachableFrom

      // add procedures to thread in way that maintains original ordering
      for (p <- programProcs) {
        if (reachable.contains(p)) {
          thread.procedures.add(p)
        }
      }

    }
  }
}
