package ir.transforms

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import analysis.solvers.*
import analysis.*
import bap.*
import ir.*
import translating.*
import util.Logger
import java.util.Base64
import spray.json.DefaultJsonProtocol.*
import util.intrusive_list.IntrusiveList
import scala.collection.mutable
import cilvisitor._

// identify calls to pthread_create
// use analysis result to determine the third parameter's value (the function pointer)
// split off that procedure into new thread
// do reachability analysis
// also need a bit in the IR where it creates separate files
def splitThreads(program: Program,
                 pointsTo: Map[RegisterVariableWrapper, Set[RegisterVariableWrapper | MemoryRegion]],
                 regionContents: Map[MemoryRegion, Set[BitVecLiteral | MemoryRegion]],
                 reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
                ): Unit = {
  
  // iterate over all commands - if call is to pthread_create, look up?
  program.foreach(c =>
    c match {
      case d: DirectCall if d.target.name == "pthread_create" =>

        // R2 should hold the function pointer of the function that begins the thread
        // look up R2 value using points to results
        val R2 = Register("R2", 64)
        val b = reachingDefs(d)
        val R2Wrapper = RegisterVariableWrapper(R2, getDefinition(R2, d, reachingDefs))
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
    })


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
