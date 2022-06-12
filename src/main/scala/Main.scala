// package scala

import BilParser.*
import analysis.*
import astnodes.exp.*
import astnodes.pred.Bool
import astnodes.stmt.Stmt
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import translating.{BoogieTranslator, FlowGraph, StatementLoader, SymbolTableListener}
import util.RunUtils
import vcgen.{State, VCGen}

import scala.collection.mutable.{ArrayBuffer, Set}
import scala.collection.{immutable, mutable}
import scala.language.postfixOps
import scala.sys.process.*

@main def main(fileName: String, elfFileName: String): Int = {
  val state: State = if (fileName.endsWith("adt")) {
    RunUtils.generateVCsAdt(fileName, elfFileName)
  } else {
    RunUtils.generateVCs(fileName, elfFileName)
  }
  RunUtils.writeToFile(state)

  // println("boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process(Seq("grep", "--color=always", ".*errors.*\\|$"), None, "GREP_COLORS" -> "'1;33"))
  // ("boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process(Seq("GREP_COLORS='1;32'", "grep", "--color=always", ".*errors.*\\|$"), None, "GREP_COLORS" -> "'1;32")) !
  // "boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process("grep --color=always '.*errors.*\\|$'", None, "GREP_COLORS" -> "'1;33")  !
  "boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| "grep --color=always '.*parse errors.*\\|$'" !
}
