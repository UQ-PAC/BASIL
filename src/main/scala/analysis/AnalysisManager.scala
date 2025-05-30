package analysis

import ir.Program

/** Analysis manager for caching and invalidating analysis results.
  * 
  * @param program Each analysis manager is defined with respect to a particular program reference, which is always
  * passed to the static analyses invoked via this manager. This ensures that the cached analysis results all relate to
  * the same program reference. It is then the responsibility of Transforms to clear these results when they are
  * invalidated by a modification to this program.
  */
class AnalysisManager(program: Program) {

  // memoizer to wrap static analyses and cache their results
  private class Memoizer[ReturnType](analysis: StaticAnalysis[ReturnType]) {

    private var memo: Option[ReturnType] = None

    def invalidate() = { memo = None }

    // allows this memoizer to be called like a function
    def apply(): ReturnType = {
      // pass this analysis manager and its associated program to the static analysis
      if memo.isEmpty then memo = Some(analysis(program, AnalysisManager.this))
      memo.get
    }
  }

  // keep track of all memoizers to ensure we can invalidate all of them
  private val memoizers: Set[Memoizer[?]] = Nil

  // private helper function for creating and storing memoizers
  private def register[ReturnType](analysis: StaticAnalysis[ReturnType]): Memoizer[ReturnType] = {
    val mem = Memoizer[ReturnType](analysis)
    memoizers ::= mem
    return mem
  }

  // list of memoizers - these can be directly called via this manager, e.g. val result = manager.exampleAnalysis()
  val intraProcConstProp = register(IntraProcConstantPropagationAnalysis())
  val interProcConstProp = register(InterProcConstantPropagationAnalysis())
  val memoryRegionResult = register(MemoryRegionAnalysisSolverAnalysis())
  val vsaResult = register(ValueSetAnalysisSolverAnalysis())
  val interLiveVarsResults = register(/* todo */)
  val paramResults = register(/* todo */)
  val steensgaardSolver = register(/* todo */) // fixme: merge these into one analysis result?
  val steensgaardPointsTo = register(/* todo */)
  val steensgaardCallSiteSummary = register(/* todo */)
  val mmmResults = register(/* todo */)
  val reachingDefs = register(/* todo */)
  val regionInjector = register(/* todo */)
  val symbolicAddresses = register(/* todo */)
  val localDSA = register(/* todo */)
  val bottomUpDSA = register(/* todo */)
  val topDownDSA = register(/* todo */)
  val writesToResult = register(/* todo */)
  val ssaResults = register(/* todo */)
  val graResult = register(/* todo */)
  val intraDomain = register(/* todo */)
  val interDomain = register(/* todo */)

  // clears the cached results of all analyses except for those in the given set
  def invalidateAllExcept(exceptions: Set[Memoizer]): Unit =
    memoizers.filterNot(exceptions.contains).foreach(_.invalidate())

  // useful to pass to 'invalidateAllExcept' when we want to preserve all or nearly all results after a transform
  def getAll(): Set[Memoizer[?]] = memoizers // safe to directly return non-mutable set
}
