// package scala

import analysis._
import astnodes._
import boogie._
import translating._
import util.RunUtils
//import vcgen._

import scala.collection.mutable.{ArrayBuffer, Set}
import scala.collection.{immutable, mutable}
import scala.language.postfixOps
import scala.sys.process.*

@main def main(fileName: String, elfFileName: String, options: String*): Unit = {
  val outFileName = if (options.isEmpty) {
    "boogie_out.bpl"
  } else {
    options.head
  }
  val program: BProgram = RunUtils.generateVCsAdt(fileName, elfFileName)
  RunUtils.writeToFile(program, outFileName)

  // println("boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process(Seq("grep", "--color=always", ".*errors.*\\|$"), None, "GREP_COLORS" -> "'1;33"))
  // ("boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process(Seq("GREP_COLORS='1;32'", "grep", "--color=always", ".*errors.*\\|$"), None, "GREP_COLORS" -> "'1;32")) !
  // "boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process("grep --color=always '.*errors.*\\|$'", None, "GREP_COLORS" -> "'1;33")  !
  //"boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| "grep --color=always '.*parse errors.*\\|$'" !
}
