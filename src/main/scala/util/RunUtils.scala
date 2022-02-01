package util

import BilParser.{BilLexer, BilParser, SymsLexer, SymsParser}
import astnodes.pred.Bool
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating.{BoogieTranslator, FlowGraph, StatementLoader, SymbolTableListener}
import vcgen.{State, VCGen}

import analysis.*;

import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters._

object RunUtils {

  def generateVCs(fileName: String, elfFileName: String): State = {
    // generate abstract syntax tree
    val bilLexer = new BilLexer(CharStreams.fromFileName(fileName));
    val tokens = new CommonTokenStream(bilLexer);
    val parser = new BilParser(tokens);
    parser.setBuildParseTree(true);
    val b = parser.bil(); // abstract syntax tree

    // extract all statement objects from the tree
    val statementLoader = new StatementLoader();
    val walker = new ParseTreeWalker();
    walker.walk(statementLoader, b);

    val symsLexer = new SymsLexer(CharStreams.fromFileName(elfFileName))
    val symsTokens = new CommonTokenStream(symsLexer)
    val symsParser = new SymsParser(symsTokens)
    symsParser.setBuildParseTree(true)
    val symsListener = new SymbolTableListener()
    walker.walk(symsListener, symsParser.syms)

    // TODO duplicated code for default value
    val flowGraph = FlowGraph.fromStmts(statementLoader.stmts.asJava, statementLoader.varSizes.toMap)

    val state = State(
      flowGraph,
      statementLoader.rely.getOrElse(Bool.True), // TODO check default
      Bool.False,
      symsListener.symbolTable.toMap,
      statementLoader.varSizes.toMap,
      statementLoader.lPreds.toMap,
      statementLoader.gammaMappings.toMap
    );

    val analysedState = Worklist(TestingAnalysis(), state).doAnalysis;

    val updatedState = BoogieTranslator.translate(analysedState)

    VCGen.genVCs(updatedState)
  }

  // TODO copy pasted
  def writeToFile(state: State, outputFileName: String = "boogie_out.bpl"): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(outputFileName, false))
      writer.write(state.toString)
      writer.flush()
    } catch {
      case _: IOException => System.err.println("Error writing to file.")
    }
  }

}
