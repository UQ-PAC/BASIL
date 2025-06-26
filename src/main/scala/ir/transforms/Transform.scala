package ir.transforms

import scala.collection.mutable
import util.{IRContext, Logger, DebugDumpIRLogger, PerformanceTimer}
import ir.{Program, dotBlockGraph}
import analysis.AnalysisManager
import translating.PrettyPrinter.pp_prog
import java.io.File

// transforms accept Log instances which specify what kind of logs to dump
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

/** This case class defines the interface and configurable parameters for transform passes that modify the basil ir
  * context in-place. It is designed to be read-only to avoid inter-dependencies between uses of the same transform
  * instance. To configure the behaviour of a transform (e.g. based on arguments to the basil program), we supply
  * a set of methods that each return a copy of the transform with a particular behaviour tweaked, such as which logs
  * to dump before and after the transform runs, whether to log performance, or whether to disable the transform.
  */
case class Transform(
  // human-readable name of the transform; it is used in the names of generated log files
  name: String,
  // the function to invoke when this transform is called
  implementation: (ctx: IRContext, man: AnalysisManager) => man.Invalidation,
  // optional message to log upon invocation of this transform
  notice: String = "",
  // optional code to run after performance has been measured but before any logs are dumped; should not modify the ir
  postRunChecks: IRContext => Unit = _ => ()
) {
  // set to false to make the apply method do nothing
  val enabled: Boolean = true
  // set to true to have the performance of this transform be measured and dumped with a PerformanceTimer
  val logPerformance: Boolean = false
  // a set of log types to dump before and after running the transform
  val logsToDump: Set[Log] = Set.empty

  // the following methods return a copy of this transform with particular behaviours tweaked

  def when(cond: Boolean): Transform = copy(enabled = cond)

  def unless(cond: Boolean): Transform = copy(enabled = !cond)

  def timeIf(cond: Boolean): Transform = copy(logPerformance = cond)

  def withLogs(logs: Set[Log]): Transform = copy(logsToDump = logs)

  /** Applies this transform to the given ir context. This is effectively a wrapper for the `implementation` function
    * that handles all of our configurable behaviour and the invalidation of analysis results.
    *
    * @param ctx The ir context to transform.
    * @param man The analysis manager through which to access and invalidate analysis results.
    */
  def apply(ctx: IRContext, man: AnalysisManager): Unit = {
    if (!enabled) return
    if (notice != "") Logger.info(s"[!] ${notice}")
    if (man.program ne ctx.program) {
      // the program we are transforming should be the same one for which the analysis results were produced
      throw new RuntimeException(
        s"Transform '$name' was passed an AnalysisManager of an IR Program with a different " +
          s"reference value than the program being transformed."
      )
    }
    logsToDump.foreach(_.dump(ctx, s"before-$name"))
    val timer: PerformanceTimer = if logPerformance then PerformanceTimer(name) else null
    val invalidation = implementation(ctx, man)
    if logPerformance then timer.checkPoint("delta")
    postRunChecks(ctx)
    logsToDump.foreach(_.dump(ctx, s"after-$name"))
    man.invalidate(invalidation)
  }
}

// helper method for constructing transforms that are sequences of other transforms
// we prefer this over invoking transforms in the implementations of other transforms
def TransformBatch(
  name: String,
  transforms: List[Transform],
  notice: String = "",
  postRunChecks: IRContext => Unit = _ => ()
): Transform = Transform(
  name,
  (ctx, man) => {
    transforms.foreach(_(ctx, man))
    man.PreserveAll
  },
  notice,
  postRunChecks
)
