package analysis

import ir.*
import analysis.solvers.*

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.immutable
import util.Logger
import util.PerformanceTimer

/** Trait for program analyses.
  *
  * @tparam R
  *   the type of the analysis result
  */
trait Analysis[+R]:

  /** Performs the analysis and returns the result.
    */
  def analyze(): R

  def timeAnalyze(timer: PerformanceTimer): R = {
    timer.reset()
    val r = analyze()
    timer.checkPoint(s"${this.getClass.getSimpleName}.analyze")
    r
  }
