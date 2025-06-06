package analysis

import ir.Program
import util.PerformanceTimer

/** Provides a consistent interface for static analyses.
  * Similar to Transform, but returns a result rather than modifying the IR in-place.
  * 
  * @tparam ReturnType The type of the result that this analysis generates.
  * @param name The name of this analysis.
  */
trait StaticAnalysis[ReturnType](val name: String) {

  val t = PerformanceTimer(name)

  protected def preRun(): Unit = {}

  protected def postRun(ret: ReturnType): Unit = {}

  protected def implementation: (Program, AnalysisManager) => ReturnType

  def apply(prog: Program, analyses: AnalysisManager): ReturnType = {
    if (analyses.program ne prog) {
      throw new RuntimeException(
        s"Analysis $name was passed an AnalysisManager of an IR Program with a different " +
          s"reference value than the program being transformed."
      )
    }
    preRun()
    t.checkPoint("start")
    val ret = implementation(prog, analyses)
    t.checkPoint("end")
    postRun(ret)
    ret
  }
}
