package analysis

import ir.Program

import scala.collection.mutable

/** Analysis manager for caching and invalidating analysis results.
  * 
  * @param program Each analysis manager is defined with respect to a particular program reference, which is always
  * passed to the static analyses invoked via this manager. This ensures that the cached analysis results all relate to
  * the same program reference. It is then the responsibility of Transforms to clear these results when they are
  * invalidated by a modification to this program.
  */
class AnalysisManager(val program: Program) {

  // memoizer to wrap static analyses and cache their results
  class Memoizer[ReturnType](analysis: StaticAnalysis[ReturnType]) {

    private var memo: Option[ReturnType] = None

    def invalidate() = { memo = None }

    // allows this memoizer to be called like a function
    def apply(): ReturnType = {
      // pass this analysis manager and its associated program to the static analysis
      if memo.isEmpty then memo = Some(analysis(AnalysisManager.this))
      memo.get
    }
  }

  enum Invalidation {
    case PreserveAll
    case ClobberAll
    case PreserveSome(toPreserve: Set[Memoizer[?]])
    case ClobberSome(toClobber: Set[Memoizer[?]])
  }

  // todo: not sure if this is the right approach - maybe we should implement convenience methods instead?
  export Invalidation.*

  // keep track of all memoizers to ensure we can invalidate all of them
  private val memoizers: mutable.Set[Memoizer[?]] = mutable.Set.empty

  // private helper function for creating and storing memoizers
  private def register[ReturnType](analysis: StaticAnalysis[ReturnType]): Memoizer[ReturnType] = {
    val mem = Memoizer[ReturnType](analysis)
    memoizers += mem
    return mem
  }

  def invalidate(invalidation: Invalidation): Unit = invalidation match {
    case PreserveAll => ()
    case ClobberAll => memoizers.foreach(_.invalidate())
    case PreserveSome(toPreserve) => (memoizers.toSet -- toPreserve).foreach(_.invalidate())
    case ClobberSome(toClobber) => toClobber.foreach(_.invalidate())
  }

  // todo: list of memoizers which can be directly called via this manager
  // val exampleAnalysis = register(ExampleAnalysis())
}
