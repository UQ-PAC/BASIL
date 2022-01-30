package util

import BilParser.{BilLexer, BilParser, SymsLexer, SymsParser}
import astnodes.pred.Bool
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating.{BoogieTranslator, FlowGraph, StatementLoader, SymbolTableListener}
import vcgen.{State, VCGen}
import analysis.*
import astnodes.exp.Expr

import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters._

object RunUtils {

  def generateVCs(fileName: String, elfFileName: String): State = {
    // println("in RunUtils")
    // generate abstract syntax tree
    // println(BilLexer)
    val bilLexer = new BilLexer(CharStreams.fromFileName(fileName));
    // println("h0.1")
    val tokens = new CommonTokenStream(bilLexer);
    // println("h0.2")
    val parser = new BilParser(tokens);
    // println("h0.3")
    parser.setBuildParseTree(true);
    // println("h0.4")
    val b = parser.bil(); // abstract syntax tree

    // println("h1")

    // extract all statement objects from the tree
    val statementLoader = new StatementLoader();
    val walker = new ParseTreeWalker();
    // println("h1.2")
    walker.walk(statementLoader, b);

    // println("h2")

    val symsLexer = new SymsLexer(CharStreams.fromFileName(elfFileName))
    val symsTokens = new CommonTokenStream(symsLexer)
    val symsParser = new SymsParser(symsTokens)
    symsParser.setBuildParseTree(true)
    val symsListener = new SymbolTableListener()
    walker.walk(symsListener, symsParser.syms)
    
    // println("h3")

    // TODO duplicated code for default value
    val flowGraph = FlowGraph.fromStmts(statementLoader.stmts.asJava, statementLoader.varSizes.toMap)

    val worklist = InlineWorklist(new ConstantPropagationAnalysis(new collection.mutable.HashMap[Expr, String](),
      Set(), null, flowGraph), flowGraph)
    println("Before CP:")
    worklist.printAllLinesWithLabels
    worklist.analyseFromMain
    println("After CP:")
    // worklist.printAllStates
    worklist.printAllLinesWithLabels
    // worklist.printAllStates

    // var worklist: BlockWorklist = BlockWorklist(Set(TestingAnal), flowGraph);
    // worklist.workOnBlocks;
    
    val state = State(
      flowGraph,
      statementLoader.rely.getOrElse(Bool.True), // TODO check default
      Bool.False,
      symsListener.symbolTable.toMap,
      statementLoader.varSizes.toMap,
      statementLoader.lPreds.toMap,
      statementLoader.gammaMappings.toMap
    )

    // println("h4")

    val updatedState = BoogieTranslator.translate(state)

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
