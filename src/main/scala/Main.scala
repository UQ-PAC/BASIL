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
import vcgen.{State, VCGen}
import astnodes.exp.*

import collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import collection.JavaConverters.*

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
            val flowGraph = FlowGraph.fromStmts(stmts.asJava)

            val toRemove : immutable.Set[String] = immutable.Set()
            val pending : immutable.Set[String] = immutable.Set()
            var worklist: BlockWorklist = BlockWorklist(Set(ConstantPropagationAnalysis(new 
                mutable.HashMap[Expr, String](), toRemove, pending)), flowGraph);
            worklist.printAllLinesWithLabels;
            worklist.analyseFromMain;
            System.out.println("left worklist")
//            println(worklist.getAllStates);
            worklist.printAllStates
            
            val translator = new BoogieTranslator(flowGraph, "boogie_out.bpl", symsListener
              .symbolTable, worklist.finalAnalysedStmtInfo);
            val updatedFlowGraph = translator.translate();
            System.out.println("left translator")

            val state = State(updatedFlowGraph, Bool.True, Bool.False, Map.empty, Map.empty)
            val vc = VCGen.genVCs(state)
            System.out.println("left vcgen")
            writeToFile(vc)
            System.out.println("wrote to file")
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
