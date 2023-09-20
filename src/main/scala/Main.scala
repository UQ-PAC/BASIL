// package scala

import bap._
import boogie._
import translating._
import util.RunUtils

import scala.collection.mutable.{ArrayBuffer, Set}
import scala.collection.{immutable, mutable}
import scala.language.postfixOps
import scala.sys.process._

@main def main(fileName: String, elfFileName: String, options: String*): Unit = {
  val specFileName: Option[String] = if (options.nonEmpty && options.head.endsWith(".spec")) {
    Some(options.head)
  } else {
    None
  }
  val outFileName = if (specFileName.isEmpty) {
    if (options.isEmpty) {
      "boogie_out"
    } else {
      options.head
    }
  } else {
    if (options.tail.isEmpty) {
      "boogie_out"
    } else {
      options.tail.head
    }
  }
  val performAnalysis = options.nonEmpty && options.contains("-analyse")
  val performInterpret = options.nonEmpty && options.contains("-interpret")
  val program: List[BProgram] = RunUtils.generateVCsAdt(fileName, elfFileName, specFileName, performAnalysis, performInterpret)
  program.foreach(prog => RunUtils.writeToFile(prog, outFileName + "_" + prog.entryProcedure + ".bpl"))
}