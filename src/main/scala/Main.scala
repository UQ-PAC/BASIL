// package scala

import bap._
import boogie._
import translating._
import util.RunUtils
import buildinfo.* 

import scala.collection.mutable.{ArrayBuffer, Set}
import scala.collection.{immutable, mutable}
import scala.language.postfixOps
import scala.sys.process._
import util.*
import mainargs.{main, arg, ParserForClass, Flag}

object Main {

  @main(name = f"${BuildInfo.name} ${BuildInfo.version}")
  case class Config(
      @arg(name = "adt", short = 'a', doc = "BAP ADT file name.")
      adtFileName: String = "",
      @arg(name = "relf", short = 'r', doc = "Name of the file containing the output of 'readelf -s -r -W'.")
      relfFileName: String = "",
      @arg(name = "spec", short = 's', doc = "BASIL specification file.")
      specFileName: Option[String],
      @arg(name = "output", short = 'o', doc = "Boogie output destination file.")
      outFileName: String = "boogie_out.bpl",
      @arg(name = "verbose", short = 'v', doc = "Show extra debugging logs.")
      verbose: Flag,
      @arg(name = "version", doc = "Show version.")
      version: Flag,
      @arg(name = "analyse", doc = "Run static analysis pass.")
      analyse: Flag,
      @arg(name = "interpret", doc = "Run BASIL IL interpreter.")
      interpret: Flag,
      @arg(name = "dump-il", doc = "Dump the Intermediate Language to text.")
      dumpIL: Flag,
      @arg(name = "help", short = 'h', doc = "Show this help message.")
      help: Flag
  )

  def main(args: Array[String]): Unit = {
    val parser = ParserForClass[Config]
    val parsed = parser.constructEither(args)

    val conf = parsed match {
      case Right(r) => r
      case Left(l) => {
        println(l)
        return
      }
    }


    if (conf.help.value) {
      println(parser.helpText(sorted = false));
      return;
    }

    if (conf.version.value) {
      println(f"${BuildInfo.name} ${BuildInfo.version}")
      return;
    }


    Logger.setLevel(LogLevel.INFO)
    if (conf.verbose.value) {
      Logger.setLevel(LogLevel.DEBUG)
    }

    if (conf.adtFileName == "" || conf.relfFileName == "") {
      println("Must specify adt and relf.")
      println(parser.helpText(sorted = false));
      return;
    }

    val program: BProgram = RunUtils.loadAndTranslate(
      conf.adtFileName,
      conf.relfFileName,
      conf.specFileName,
      conf.analyse.value,
      conf.interpret.value,
      conf.dumpIL.value
    )
    RunUtils.writeToFile(program, conf.outFileName)
  }

}
