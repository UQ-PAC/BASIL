import astnodes.stmt.Stmt
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTreeWalker

import scala.collection.mutable.Set
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException
import java.util.ArrayList
import java.util.Arrays
import java.util.List
import translating.{BoogieTranslator, FlowGraph, StatementLoader, SymbolTableListener}
import BilParser.*
import analysis.*
import astnodes.pred.Bool
import util.RunUtils
import vcgen.{State, VCGen}
import astnodes.exp.*

import collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import sys.process.*
import scala.language.postfixOps

@main def main(fileName: String, elfFileName: String, outputType: String = "") = {
  val state = RunUtils.generateVCs(fileName, elfFileName)
  RunUtils.writeToFile(state)

  // println("boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process(Seq("grep", "--color=always", ".*errors.*\\|$"), None, "GREP_COLORS" -> "'1;33"))
  // ("boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process(Seq("GREP_COLORS='1;32'", "grep", "--color=always", ".*errors.*\\|$"), None, "GREP_COLORS" -> "'1;32")) !
  // "boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process("grep --color=always '.*errors.*\\|$'", None, "GREP_COLORS" -> "'1;33")  !
  "boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| "grep --color=always '.*parse errors.*\\|$'" !
}
