// package scala

import bap.*
import boogie.*
import translating.*
import util.RunUtils
import scala.sys.process.*

import java.io.File
import scala.collection.mutable.{ArrayBuffer, Set}
import scala.collection.{immutable, mutable}
import scala.language.postfixOps
import scala.sys.process.*
import util.*
import mainargs.{main, arg, ParserForClass, Flag}

object Main {

  enum ChooseInput {
    case Gtirb
    case Bap
  }

  def loadDirectory(i: ChooseInput = ChooseInput.Gtirb, d: String): ILLoadingConfig = {
    val p = d.split("/")
    val tryName = Seq(p.last, p.dropRight(1).last)

    tryName
      .flatMap(name => {
        val trySpec = Seq((p ++ Seq(s"$name.spec")).mkString("/"), (p.dropRight(1) ++ Seq(s"$name.spec")).mkString("/"))
        val adt = (p ++ Seq(s"$name.adt")).mkString("/")
        val relf = (p ++ Seq(s"$name.relf")).mkString("/")
        val gtirb = (p ++ Seq(s"$name.gts")).mkString("/")

        val spec = trySpec
          .flatMap(s => {
            if (File(s).exists) {
              Seq(s)
            } else {
              Seq()
            }
          })
          .headOption

        val input = i match
          case ChooseInput.Gtirb => gtirb
          case ChooseInput.Bap => adt

        if (File(input).exists() && File(relf).exists()) {
          Logger.info(s"Found $input $relf ${spec.getOrElse("")}")
          Seq(ILLoadingConfig(input, relf, spec))
        } else {
          Seq()
        }
      })
      .head

  }

  @main(name = "BASIL")
  case class Config(
    @arg(name = "load-directory-bap", doc = "Load relf, adt, and bir from directory (and spec from parent directory)")
    bapInputDirName: Option[String],
    @arg(name = "load-directory-gtirb", doc = "Load relf, gts, and bir from directory (and spec from parent directory)")
    gtirbInputDirName: Option[String],
    @arg(name = "input", short = 'i', doc = "BAP .adt file or GTIRB/ASLi .gts file")
    inputFileName: Option[String],
    @arg(name = "relf", short = 'r', doc = "Name of the file containing the output of 'readelf -s -r -W'.")
    relfFileName: Option[String],
    @arg(name = "spec", short = 's', doc = "BASIL specification file.")
    specFileName: Option[String],
    @arg(name = "output", short = 'o', doc = "Boogie output destination file.")
    outFileName: String = "basil-out.bpl",
    @arg(name = "boogie-use-lambda-stores", doc = "Use lambda representation of store operations.")
    lambdaStores: Flag,
    @arg(
      name = "boogie-procedure-rg",
      doc = "Switch version of procedure rely/guarantee checks to emit. (function|ifblock)"
    )
    procedureRG: Option[String],
    @arg(name = "verbose", short = 'v', doc = "Show extra debugging logs (the same as -vl log)")
    verbose: Flag,
    @arg(
      name = "vl",
      doc = s"Show extra debugging logs for a specific logger (${Logger.allLoggers.map(_.name).mkString(", ")})."
    )
    verboseLog: Seq[String] = Seq(),
    @arg(name = "analyse", doc = "Run static analysis pass.")
    analyse: Flag,
    @arg(name = "interpret", doc = "Run BASIL IL interpreter.")
    interpret: Flag,
    @arg(name = "dump-il", doc = "Dump the Intermediate Language to text.")
    dumpIL: Option[String],
    @arg(name = "main-procedure-name", short = 'm', doc = "Name of the main procedure to begin analysis at.")
    mainProcedureName: String = "main",
    @arg(
      name = "procedure-call-depth",
      doc = "Cull procedures beyond this call depth from the main function (defaults to Int.MaxValue)"
    )
    procedureDepth: Int = Int.MaxValue,
    @arg(name = "trim-early", doc = "Cull procedures BEFORE running analysis")
    trimEarly: Flag,
    @arg(name = "help", short = 'h', doc = "Show this help message.")
    help: Flag,
    @arg(name = "analysis-results", doc = "Log analysis results in files at specified path.")
    analysisResults: Option[String],
    @arg(name = "analysis-results-dot", doc = "Log analysis results in .dot form at specified path.")
    analysisResultsDot: Option[String],
    @arg(
      name = "threads",
      short = 't',
      doc = "Separates threads into multiple .bpl files with given output filename as prefix (requires --analyse flag)"
    )
    threadSplit: Flag,
    @arg(name = "parameter-form", doc = "Lift registers to local variables passed by parameter")
    parameterForm: Flag,
    @arg(
      name = "summarise-procedures",
      doc = "Generates summaries of procedures which are used in pre/post-conditions (requires --analyse flag)"
    )
    summariseProcedures: Flag,
    @arg(name = "simplify", doc = "Partial evaluate / simplify BASIL IR before output (implies --parameter-form)")
    simplify: Flag,
    @arg(
      name = "validate-simplify",
      doc = "Emit SMT2 check for validation of simplification expression rewrites 'rewrites.smt2'"
    )
    validateSimplify: Flag,
    @arg(name = "verify", doc = "Run boogie on the resulting file")
    verify: Flag,
    @arg(
      name = "memory-regions",
      doc =
        "Performs static analysis to separate memory into discrete regions in Boogie output (requires --analyse flag) (mra|dsa) (dsa is recommended over mra)"
    )
    memoryRegions: Option[String],
    @arg(
      name = "no-irreducible-loops",
      doc = "Disable producing irreducible loops when --analyse is passed (does nothing without --analyse)"
    )
    noIrreducibleLoops: Flag,
    @arg(
      name = "dsa",
      doc =
        "Perform Data Structure Analysis if no version is specified perform constraint generation (requires --simplify flag) (none|norm|field|set|all)"
    )
    dsaType: Option[String]
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

    if (conf.verbose.value) {
      Logger.setLevel(LogLevel.DEBUG, true)
    }
    for (v <- conf.verboseLog) {
      Logger.findLoggerByName(v) match {
        case None =>
          throw Exception(s"Unknown logger: '${v}': allowed are ${Logger.allLoggers.map(_.name).mkString(", ")}")
        case Some(v) => v.setLevel(LogLevel.DEBUG, true)
      }
    }

    if (conf.analysisResults.isDefined) {
      DebugDumpIRLogger.setLevel(LogLevel.INFO)
    }
    if (conf.analysisResultsDot.isDefined) {
      AnalysisResultDotLogger.setLevel(LogLevel.INFO)
    }

    val rely = conf.procedureRG match {
      case Some("function") => Some(ProcRelyVersion.Function)
      case Some("ifblock") => Some(ProcRelyVersion.IfCommandContradiction)
      case None => None
      case Some(_) =>
        throw new IllegalArgumentException("Illegal option to boogie-procedure-rg, allowed are: ifblock, function")
    }
    val staticAnalysis = if (conf.analyse.value) {
      val memoryRegionsMode = conf.memoryRegions match {
        case Some("dsa") => MemoryRegionsMode.DSA
        case Some("mra") => MemoryRegionsMode.MRA
        case None => MemoryRegionsMode.Disabled
        case Some(_) => throw new IllegalArgumentException("Illegal option to memory-regions, allowed are: dsa, mra")
      }
      Some(
        StaticAnalysisConfig(
          conf.dumpIL,
          conf.analysisResults,
          conf.analysisResultsDot,
          conf.threadSplit.value,
          conf.summariseProcedures.value,
          memoryRegionsMode,
          !conf.noIrreducibleLoops.value
        )
      )
    } else {
      None
    }

    val dsa: Option[DSAConfig] = if (conf.simplify.value) {
      conf.dsaType match
        case Some("set") => Some(DSAConfig(immutable.Set(DSAAnalysis.Set)))
        case Some("field") => Some(DSAConfig(immutable.Set(DSAAnalysis.Field)))
        case Some("norm") => Some(DSAConfig(immutable.Set(DSAAnalysis.Norm)))
        case Some("all") => Some(DSAConfig(immutable.Set(DSAAnalysis.Set, DSAAnalysis.Field, DSAAnalysis.Norm)))
        case Some("none") => Some(DSAConfig(immutable.Set.empty))
        case None => None
        case Some(_) =>
          throw new IllegalArgumentException("Illegal option to dsa, allowed are: (none|set|field|norm|all)")
    } else {
      None
    }

    val boogieMemoryAccessMode = if (conf.lambdaStores.value) {
      BoogieMemoryAccessMode.LambdaStoreSelect
    } else {
      BoogieMemoryAccessMode.SuccessiveStoreSelect
    }
    val boogieGeneratorConfig = BoogieGeneratorConfig(boogieMemoryAccessMode, true, rely, conf.threadSplit.value)

    val loadingInputs = if (conf.bapInputDirName.isDefined) then {
      loadDirectory(ChooseInput.Bap, conf.bapInputDirName.get)

    } else if (conf.gtirbInputDirName.isDefined) then {
      loadDirectory(ChooseInput.Gtirb, conf.gtirbInputDirName.get)
    } else if (conf.inputFileName.isDefined && conf.relfFileName.isDefined) then {
      ILLoadingConfig(conf.inputFileName.get, conf.relfFileName.get, conf.specFileName)

    } else {
      throw IllegalArgumentException(
        "\nRequires --load-directory-bap OR --load-directory-gtirb OR --input and--relf\n\n" + parser
          .helpText(sorted = false)
      )
    }

    val q = BASILConfig(
      loading = loadingInputs.copy(
        dumpIL = conf.dumpIL,
        mainProcedureName = conf.mainProcedureName,
        procedureTrimDepth = conf.procedureDepth,
        parameterForm = conf.parameterForm.value,
        trimEarly = conf.trimEarly.value
      ),
      runInterpret = conf.interpret.value,
      simplify = conf.simplify.value,
      validateSimp = conf.validateSimplify.value,
      staticAnalysis = staticAnalysis,
      boogieTranslation = boogieGeneratorConfig,
      outputPrefix = conf.outFileName,
      dsaConfig = dsa
    )

    RunUtils.run(q)
    if (conf.verify.value) {
      Logger.info("Running boogie")
      val timer = PerformanceTimer("Verify", LogLevel.INFO)
      Seq("boogie", "/useArrayAxioms", q.outputPrefix).!
      timer.checkPoint("Finish")
    }
  }

}
