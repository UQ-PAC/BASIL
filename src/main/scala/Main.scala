import astnodes.stmt.Stmt
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTreeWalker

import scala.collection.mutable.Set;

import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException
import java.util.ArrayList
import java.util.Arrays
import java.util.List
import translating.{BoogieTranslator, FlowGraph, StatementLoader, SymbolTableListener}
import BilParser.*
import analysis.*;
import astnodes.pred.Bool
import vcgen.{State, VCGen}

import collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import collection.JavaConverters.*
import sys.process.*
import scala.language.postfixOps

@main def main(fileName: String, elfFileName: String, outputType: String) = {
        // generate abstract syntax tree
        val bilLexer = new BilLexer(CharStreams.fromFileName(fileName));
        val tokens = new CommonTokenStream(bilLexer);
        val parser = new BilParser(tokens);
        parser.setBuildParseTree(true);
        val b = parser.bil(); // abstract syntax tree

        // extract all statement objects from the tree
        val stmts = new ArrayBuffer[Stmt]();
        val statementLoader = new StatementLoader(stmts);
        val walker = new ParseTreeWalker();
        walker.walk(statementLoader, b);

        val symsLexer = new SymsLexer(CharStreams.fromFileName(elfFileName))
        val symsTokens = new CommonTokenStream(symsLexer)
        val symsParser = new SymsParser(symsTokens)
        symsParser.setBuildParseTree(true)
        val symsListener = new SymbolTableListener()
        walker.walk(symsListener, symsParser.syms)

        if (outputType.equals("boogie")) {
          // TODO duplicated code for default value
          val flowGraph = FlowGraph.fromStmts(stmts.asJava, statementLoader.varSizes.toMap)

          var worklist: BlockWorklist = BlockWorklist(Set(TestingAnalysis()), flowGraph);
          worklist.workOnBlocks;

          val translator = new BoogieTranslator(flowGraph, "boogie_out.bpl");
          val updatedFlowGraph = translator.translate();

          val state = State(updatedFlowGraph, Bool.True, Bool.False, symsListener.symbolTable.toMap, statementLoader.lPreds.toMap, Map.empty)
          val vc = VCGen.genVCs(state)
          writeToFile(vc)

          // println("boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process(Seq("grep", "--color=always", ".*errors.*\\|$"), None, "GREP_COLORS" -> "'1;33"))
          // ("boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process(Seq("GREP_COLORS='1;32'", "grep", "--color=always", ".*errors.*\\|$"), None, "GREP_COLORS" -> "'1;32")) !
          // "boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" #| Process("grep --color=always '.*errors.*\\|$'", None, "GREP_COLORS" -> "'1;33")  !
          "boogie boogie_out.bpl" #| "grep --color=always '.*Error.*\\|$'" !
        } else {
            println("Output failed")
        }
    }

// TODO copy pasted
def writeToFile(state: State): Unit = {
  val outputFileName = "boogie_out.bpl"
  try {
    val writer = new BufferedWriter(new FileWriter(outputFileName, false))
    writer.write(state.toString)
    writer.flush()
  } catch {
    case _: IOException => System.err.println("Error writing to file.")
  }
}
