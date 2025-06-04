package ir.transforms

import util.IRContext
import ir.Program
import analysis.AnalysisManager
import util.PerformanceTimer

/** Provides a consistent interface for IR transforms.
  * 
  * @param name The name of this transform.
  */
trait Transform(val name: String) {

  // when false, this transform does nothing
  val toggle: Boolean = true
  // the performance of each transform is implicitly tested
  val t = PerformanceTimer(name)

  // code to run before the transform implementation, such as logging information
  protected def preRun(ctx: IRContext): Unit = {}

  // code to run after the transform implementation, such as logging information or assertions
  protected def postRun(ctx: IRContext): Unit = {}

  /** Override this method to implement the logic for your transform.
    * 
    * @param ctx The IR to be modified in-place.
    * @param analyses Use this to access the results of static analyses. Any results not yet generated will be produced
    * automatically and then cached in the manager for later retrieval.
    * 
    * @return The set of analyses that are *preserved* after the transform. To clear all analyses after the transform is
    * invoked, return Set.empty. (Note that this will negatively impact performance.) To preserve all analyses, return
    * analyses.getAll().
    */
  protected def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer[?]]

  // instances of transforms can be directly called to invoke this method
  def apply(ctx: IRContext, analyses: AnalysisManager): Unit = {
    if (!toggle) return
    if (analyses.program ne ctx.program) {
      // the program we are transforming should be the same one for which the analysis results were produced
      throw new RuntimeException(s"Transform $name was passed an AnalysisManager of an IR Program with a different " +
        s"reference value than the program being transformed.")
    }
    preRun(ctx)
    t.checkPoint("start")
    val toPreserve = implementation(ctx, analyses)
    t.checkPoint("end")
    postRun(ctx)
    analyses.invalidateAllExcept(toPreserve)
  }
}

/** A transform can be a sequence of other transforms. We prefer using this over constructing transforms in the
  * implementations of other transforms.
  * 
  * @param name The name of this transform batch.
  * @param transforms The sequence of other transforms that comprise this transform.
  */
class TransformBatch(name: String, transforms: List[Transform]) extends Transform(name) {
  def implementation(ctx: IRContext, analyses: AnalysisManager): Set[analyses.Memoizer[?]] = {
    // simply apply each transform in-turn
    transforms.foreach(_(ctx, analyses))
    Set.empty
  }
}
