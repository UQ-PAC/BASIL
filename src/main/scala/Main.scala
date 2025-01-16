// package scala

import bap.*
import boogie.*
import translating.*
import util.RunUtils

import scala.collection.mutable.{ArrayBuffer, Set}
import scala.collection.{immutable, mutable}
import scala.language.postfixOps
import scala.sys.process.*
import util.*
import mainargs.{main, arg, ParserForClass, Flag}

object Main {

  @main(name = "BASIL")
  case class Config(
    @arg(name = "input", short = 'i', doc = "BAP .adt file or GTIRB/ASLi .gts file")
    inputFileName: String,
    @arg(name = "relf", short = 'r', doc = "Name of the file containing the output of 'readelf -s -r -W'.")
    relfFileName: String,
    @arg(name = "spec", short = 's', doc = "BASIL specification file.")
    specFileName: Option[String],
    @arg(name = "output", short = 'o', doc = "Boogie output destination file.")
    outFileName: String = "basil-out",
    @arg(name = "boogie-use-lambda-stores", doc = "Use lambda representation of store operations.")
    lambdaStores: Flag,
    @arg(name = "boogie-procedure-rg", doc = "Switch version of procedure rely/guarantee checks to emit. (function|ifblock)")
    procedureRG: Option[String],
    @arg(name = "verbose", short = 'v', doc = "Show extra debugging logs.")
    verbose: Flag,
    @arg(name = "analyse", doc = "Run static analysis pass.")
    analyse: Flag,
    @arg(name = "interpret", doc = "Run BASIL IL interpreter.")
    interpret: Flag,
    @arg(name = "dump-il", doc = "Dump the Intermediate Language to text.")
    dumpIL: Option[String],
    @arg(name = "main-procedure-name", short = 'm', doc = "Name of the main procedure to begin analysis at.")
    mainProcedureName: String = "main",
    @arg(name = "procedure-call-depth", doc = "Cull procedures beyond this call depth from the main function (defaults to Int.MaxValue)")
    procedureDepth: Int = Int.MaxValue,
    @arg(name = "help", short = 'h', doc = "Show this help message.")
    help: Flag,
    @arg(name = "analysis-results", doc = "Log analysis results in files at specified path.")
    analysisResults: Option[String],
    @arg(name = "analysis-results-dot", doc = "Log analysis results in .dot form at specified path.")
    analysisResultsDot: Option[String],
    @arg(name = "threads", short = 't', doc = "Separates threads into multiple .bpl files with given output filename as prefix (requires --analyse flag)")
    threadSplit: Flag,
    @arg(name = "summarise-procedures", doc = "Generates summaries of procedures which are used in pre/post-conditions (requires --analyse flag)")
    summariseProcedures: Flag,
    @arg(name = "memory-regions", doc = "Performs static analysis to separate memory into discrete regions in Boogie output (requires --analyse flag) (mra|dsa) (dsa is recommended over mra)")
    memoryRegions: Option[String],
    @arg(name = "no-irreducible-loops", doc = "Disable producing irreducible loops when --analyse is passed (does nothing without --analyse)")
    noIrreducibleLoops: Flag
  )

  def main(args: Array[String]): Unit = {
    val parser = ParserForClass[Config]
    val parsed = parser.constructEither(args.toSeq)

    val conf = parsed match {
      case Right(r) => r
      case Left(l) => 
        println(l)
        return
    }

    if (conf.help.value) {
      println(parser.helpText(sorted = false))
    }

    Logger.setLevel(LogLevel.INFO)
    if (conf.verbose.value) {
      Logger.setLevel(LogLevel.DEBUG)
    }

    val rely = conf.procedureRG match {
      case Some("function") => Some(ProcRelyVersion.Function)
      case Some("ifblock") => Some(ProcRelyVersion.IfCommandContradiction)
      case None => None
      case Some(_) => throw new IllegalArgumentException("Illegal option to boogie-procedure-rg, allowed are: ifblock, function")
    }
    val staticAnalysis = if (conf.analyse.value) {
      val memoryRegionsMode = conf.memoryRegions match {
        case Some("dsa") => MemoryRegionsMode.DSA
        case Some("mra") => MemoryRegionsMode.MRA
        case None => MemoryRegionsMode.Disabled
        case Some(_) => throw new IllegalArgumentException("Illegal option to memory-regions, allowed are: dsa, mra")
      }
      Some(StaticAnalysisConfig(
        conf.dumpIL,
        conf.analysisResults,
        conf.analysisResultsDot,
        conf.threadSplit.value,
        conf.summariseProcedures.value,
        memoryRegionsMode,
        !conf.noIrreducibleLoops.value
      ))
    } else {
      None
    }

    val boogieMemoryAccessMode = if (conf.lambdaStores.value) {
      BoogieMemoryAccessMode.LambdaStoreSelect
    } else {
      BoogieMemoryAccessMode.SuccessiveStoreSelect
    }
    val boogieGeneratorConfig = BoogieGeneratorConfig(boogieMemoryAccessMode, true, rely, conf.threadSplit.value)

    val q = BASILConfig(
      loading = ILLoadingConfig(conf.inputFileName, conf.relfFileName, conf.specFileName, conf.dumpIL, conf.mainProcedureName, conf.procedureDepth),
      runInterpret = conf.interpret.value,
      staticAnalysis = staticAnalysis,
      boogieTranslation = boogieGeneratorConfig,
      outputPrefix = conf.outFileName,
    )

    RunUtils.run(q)
  }

}
