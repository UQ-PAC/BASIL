// package scala

import analysis.data_structure_analysis.DSAPhase
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
import mainargs.{Flag, ParserForClass, arg, main}
import util.boogie_interaction.BoogieResultKind

object Main {

  enum ChooseInput {
    case Gtirb
    case Bap
  }

  private def replacePathExtension(p: java.nio.file.Path, newExtension: String) = {
    val basename = p.getFileName().toString

    val withoutExtension = if (basename.contains(".")) {
      basename.split("\\.", -1).dropRight(1).mkString(".")
    } else {
      basename
    }

    p.resolveSibling(withoutExtension + newExtension)
  }

  def loadDirectory(i: ChooseInput = ChooseInput.Gtirb, d: String): ILLoadingConfig = {

    // at this point, `path` is the given directory with file extensions removed (if any).
    val path = replacePathExtension(java.nio.file.Paths.get(d), "")

    // name of the parent directory.
    // e.g., in "src/test/correct/TESTCASE/gcc_O2", this would be "TESTCASE".
    val parentName = path.getParent().getFileName()

    // the elements of this list are the filenames which will be tried. they will be
    // suffixed with different extensions to get the different files needed.
    val tryName = Seq(path.resolve(path.getFileName), path.resolve(parentName), path, path.getFileName, path.getParent)
    // NOTE: path.resolve(subpath) acts like `path + "/" + subpath`

    // helper function to locate a file adjacent to the given path, with the given extension,
    // and ensure that it exists. returns None if file does not exist, otherwise Some(Path).
    val findAdjacent = (x: java.nio.file.Path, ext: String) =>
      Some(replacePathExtension(x, ext)).filter(_.toFile.exists)

    val results = tryName
      .flatMap(name => {
        val trySpec = (tryName ++ Seq(path.getParent.resolve(parentName)))
          .flatMap(findAdjacent(_, ".spec"))
        val spec = trySpec.headOption

        val adt = findAdjacent(name, ".adt")
        val relf = findAdjacent(name, ".relf")
        val gtirb = findAdjacent(name, ".gts")

        val input = i match
          case ChooseInput.Gtirb => gtirb
          case ChooseInput.Bap => adt

        (input, relf) match {
          case (Some(input), Some(relf)) => {
            Logger.info(s"Found $input $relf ${spec.getOrElse("")}")
            Seq(ILLoadingConfig(input.toString, relf.toString, spec.map(_.toString)))
          }
          case _ => Seq()
        }
      })

    results match {
      case Nil => throw Exception(s"failed to load directory (tried: ${tryName.mkString(", ")})")
      case Seq(x) => x
      case more => throw Exception(s"found more than one potential input (${more.mkString(", ")})")
    }
  }

  @main(name = "BASIL")
  case class Config(
    @arg(name = "load-directory-bap", doc = "Load relf, adt, and bir from directory (and spec from parent directory)")
    bapInputDirName: Option[String],
    @arg(name = "load-directory-gtirb", doc = "Load relf and gts from directory (and spec from parent directory)")
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
    @arg(
      name = "generate-rely-guarantees",
      doc = "Generates rely-guarantee conditions for each procedure that contains a return node."
    )
    generateRelyGuarantees: Flag,
    @arg(name = "simplify", doc = "Partial evaluate / simplify BASIL IR before output (implies --parameter-form)")
    simplify: Flag,
    @arg(
      name = "pc",
      doc = "Program counter mode, supports GTIRB only. (options: none | keep | assert) (default: none)"
    )
    pcTracking: Option[String],
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
    @arg(name = "dsa", doc = "Perform Data Structure Analysis (requires --simplify flag) (pre|local|bu|td)")
    dsaType: Option[String],
    @arg(name = "dsa-checks", doc = "Perform additional dsa checks (requires --dsa (local|bu|td)")
    dsaChecks: Flag,
    @arg(name = "split-globals", doc = "split the globals for dsa (requires --dsa (pre|local|bu|td)")
    dsaSplitGlobals: Flag,
    @arg(
      name = "eq-cells",
      doc = "allow cells from same node to be merged without collapsing (requires --dsa (local|bu|td)"
    )
    dsaEqCells: Flag,
    @arg(name = "dsa-assert", doc = "insert assertions to check globals offset to top fall within global region bounds")
    dsaAssert: Flag,
    @arg(name = "memory-transform", doc = "Transform memory access to region accesses")
    memoryTransform: Flag,
    @arg(name = "noif", doc = "Disable information flow security transform in Boogie output")
    noif: Flag
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
      return
    }

    Logger.setLevel(LogLevel.INFO, false)
    if (conf.verbose.value) {
      Logger.setLevel(LogLevel.DEBUG, true)
    }
    DebugDumpIRLogger.setLevel(LogLevel.OFF)
    AnalysisResultDotLogger.setLevel(LogLevel.OFF)
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
          memoryRegionsMode,
          !conf.noIrreducibleLoops.value
        )
      )
    } else {
      None
    }

    val dsa: Option[DSConfig] = if (conf.simplify.value) {
      val phase = conf.dsaType match
        case Some("pre") => DSAPhase.Pre
        case Some("local") => DSAPhase.Local
        case Some("bu") => DSAPhase.BU
        case Some("td") => DSAPhase.TD
        case None => DSAPhase.TD
        case Some(_) =>
          throw new IllegalArgumentException("Illegal option to dsa, allowed are: (pre|local|bu|td)")

      Some(
        DSConfig(phase, conf.dsaSplitGlobals.value, conf.dsaAssert.value, conf.dsaEqCells.value, conf.dsaChecks.value)
      )
    } else {
      None
    }

    val boogieMemoryAccessMode = if (conf.lambdaStores.value) {
      BoogieMemoryAccessMode.LambdaStoreSelect
    } else {
      BoogieMemoryAccessMode.SuccessiveStoreSelect
    }
    val boogieGeneratorConfig =
      BoogieGeneratorConfig(boogieMemoryAccessMode, true, rely, conf.threadSplit.value, conf.noif.value)

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
        trimEarly = conf.trimEarly.value,
        pcTracking = PCTrackingOption.valueOf(conf.pcTracking.getOrElse("none").capitalize)
      ),
      runInterpret = conf.interpret.value,
      simplify = conf.simplify.value,
      validateSimp = conf.validateSimplify.value,
      summariseProcedures = conf.summariseProcedures.value,
      generateRelyGuarantees = conf.generateRelyGuarantees.value,
      staticAnalysis = staticAnalysis,
      boogieTranslation = boogieGeneratorConfig,
      outputPrefix = conf.outFileName,
      dsaConfig = dsa,
      memoryTransform = conf.memoryTransform.value
    )

    val result = RunUtils.run(q)
    if (conf.verify.value) {
      assert(result.boogie.nonEmpty)
      var failed = false
      for (b <- result.boogie) {
        val result = b.verifyBoogie(b.filename)
        result.kind match {
          case BoogieResultKind.Verified(c, _) if c > 0 => ()
          case _ => failed = true
        }
      }
      if (failed) {
        throw Exception("Verification failed")
      }
    }
  }

}
