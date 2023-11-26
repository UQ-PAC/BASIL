// package scala

import bap._
import boogie._
import translating._
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
      @arg(name = "adt", short = 'a', doc = "BAP ADT file name.")
      adtFileName: String,
      @arg(name = "relf", short = 'r', doc = "Name of the file containing the output of 'readelf -s -r -W'.")
      relfFileName: String,
      @arg(name = "spec", short = 's', doc = "BASIL specification file.")
      specFileName: Option[String],
      @arg(name = "output", short = 'o', doc = "Boogie output destination file.")
      outFileName: String = "basil-out",
      @arg(name = "boogie-use-lambda-stores", doc = "Use lambda representation of store operations.")
      lambdaStores: Flag,
      @arg(name = "verbose", short = 'v', doc = "Show extra debugging logs.")
      verbose: Flag,
      @arg(name = "analyse", doc = "Run static analysis pass.")
      analyse: Flag,
      @arg(name = "interpret", doc = "Run BASIL IL interpreter.")
      interpret: Flag,
      @arg(name = "dump-il", doc = "Dump the Intermediate Language to text.")
      dumpIL: Option[String],
      @arg(name = "help", short = 'h', doc = "Show this help message.")
      help: Flag,
      @arg(name = "analysis-results", doc = "Log analysis results in files at specified path.")
      analysisResults: Option[String],
      @arg(name = "analysis-results-dot", doc = "Log analysis results in .dot form at specified path.")
      analysisResultsDot: Option[String]
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

    val q = BASILConfig(
      loading = ILLoadingConfig(conf.adtFileName, conf.relfFileName, conf.specFileName, conf.dumpIL),
      runInterpret = conf.interpret.value,
      staticAnalysis = if conf.analyse.value then Some(StaticAnalysisConfig(conf.dumpIL, conf.analysisResults, conf.analysisResultsDot)) else None,
      boogieTranslation = BoogieGeneratorConfig(if conf.lambdaStores.value then BoogieMemoryAccessMode.LambdaStoreSelect else BoogieMemoryAccessMode.SuccessiveStoreSelect),
      outputPrefix = conf.outFileName,
    )

    RunUtils.run(q)
  }

}
