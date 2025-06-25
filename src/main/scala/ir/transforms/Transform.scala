package ir.transforms

import scala.collection.mutable
import util.{IRContext, Logger, DebugDumpIRLogger, PerformanceTimer}
import ir.{Program, dotBlockGraph}
import analysis.AnalysisManager
import translating.PrettyPrinter.pp_prog
import java.io.File

// TransformConfig accepts Log instances which specify what kind of logs to dump for particular transforms
trait Log {
  def dump(ctx: IRContext, transformName: String): Unit
}

// dumps a blockgraph log
case class BlockgraphLog(filenamePrefix: String) extends Log {
  def dump(ctx: IRContext, transformName: String): Unit =
    DebugDumpIRLogger.writeToFile(
      File(s"${filenamePrefix}_blockgraph-${transformName}.dot"),
      dotBlockGraph(ctx.program.mainProcedure)
    )
}

// dumps an IR log
case class IrLog(filenamePrefix: String) extends Log {
  def dump(ctx: IRContext, transformName: String): Unit =
    DebugDumpIRLogger.writeToFile(File(s"${filenamePrefix}_il-${transformName}.il"), pp_prog(ctx.program))
}

/** Allows the behaviour of transforms to be configured at runtime, upon invocation.
  *
  * @param disabled Optionally specify a set of transforms to disable.
  * @param dumpLogs Optionally specify which logs to dump for which transforms, if any.
  */
case class TransformConfig(
  disabled: Set[Transform] = Set.empty, 
  dumpLogs: Map[Transform, Set[Log]] = Map.empty,
  logPerformance: Set[Transform] = Set.empty
)

// default value for transforms
val emptyConfig = TransformConfig()

/** Currently, we have two kinds of transforms: SingleTransform, and TransformBatch. This trait provides a common
  * interface for them to share.
  * 
  * Transforms can be directly called to invoke the shared 'apply' method, which applies the transform in the context
  * of some runtime configuration. They are designed to be read-only to avoid inter-dependencies between their users; if
  * some configuration is required, it must be provided upon invocation.
  */
trait Transform {
  // human-readable name of the transform; it is used in the names of generated log files
  val name: String
  // optional message to log upon invocation of this transform
  val notice: String

  // modifies the given IR context in-place, using the analysis results provided by this analysis manager
  protected def transform(ctx: IRContext, man: AnalysisManager, config: TransformConfig): man.Invalidation

  // optional code to run *after* performance has been measured but *before* any logs are dumped, e.g. post-run checks
  protected def postRun(ctx: IRContext): Unit

  // executes the transform with any modifications or book-keeping specified by the given config
  def apply(ctx: IRContext, man: AnalysisManager, config: TransformConfig = emptyConfig): Unit = {
    if (config.disabled.contains(this)) return
    if (notice != "") then Logger.info(s"[!] ${notice}")
    if (man.program ne ctx.program) {
      // the program we are transforming should be the same one for which the analysis results were produced
      throw new RuntimeException(
        s"Transform '$name' was passed an AnalysisManager of an IR Program with a different " +
          s"reference value than the program being transformed."
      )
    }
    val maybeLogs = config.dumpLogs.get(this)
    maybeLogs.foreach(_.foreach(_.dump(ctx, s"before-$name")))
    val logPerformance: Boolean = config.logPerformance.contains(this)
    val timer: PerformanceTimer = if (logPerformance) then PerformanceTimer(name) else null
    val invalidation = transform(ctx, man, config) // run the actual transform and get the analysis results to clobber
    if (logPerformance) then timer.checkPoint("delta")
    postRun(ctx) // runs after performance checks, and before logging
    maybeLogs.foreach(_.foreach(_.dump(ctx, s"after-$name")))
    man.invalidate(invalidation) // clobber the specified analysis results
  }
}

/** A standard transform. Accepts an implementation function which modifies the given IR context in-place.
  */
case class SingleTransform(
  name: String,
  implementation: (ctx: IRContext, man: AnalysisManager) => man.Invalidation,
  notice: String = ""
) extends Transform {
  // simply calls the given implementation function
  def transform(ctx: IRContext, man: AnalysisManager, config: TransformConfig): man.Invalidation =
    implementation(ctx, man)

  // standard transforms don't need anything here; post-run checks should be handled by the implementation
  def postRun(ctx: IRContext): Unit = ()
}

/** A transform batch is a sequence of other transforms, followed by some optional post-run checks on the IR context.
  */
case class TransformBatch(
  name: String,
  transforms: List[Transform],
  notice: String = "",
  postRunChecks: IRContext => Unit = _ => ()
) extends Transform {
  // runs each sub-transform in-turn (invalidation is handled by the sub-transforms)
  def transform(ctx: IRContext, man: AnalysisManager, config: TransformConfig): man.Invalidation = {
    transforms.foreach(_(ctx, man, config))
    man.PreserveAll
  }

  def postRun(ctx: IRContext): Unit = postRunChecks(ctx)
}
