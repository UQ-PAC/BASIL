package analysis

import util.PerformanceTimer

/** Provides a consistent interface for static analyses.
  * Similar to Transform, but returns a result rather than modifying the IR in-place.
  * 
  * @tparam ReturnType The type of the result that this analysis generates.
  * @param name The name of this analysis.
  */
trait StaticAnalysis[ReturnType](val name: String) {

  val t = PerformanceTimer(name)

  protected def implementation(man: AnalysisManager): ReturnType

  def apply(man: AnalysisManager): ReturnType = {
    t.checkPoint("start")
    val ret = implementation(man)
    t.checkPoint("end")
    ret
  }
}
