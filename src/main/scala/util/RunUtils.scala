package util

import BilParser.*
import analysis.*
import astnodes.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating.*
import vcgen.{State, VCGen}

import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters.*

object RunUtils {

  def generateVCsAdt(fileName: String, elfFileName: String): State = {
    val adtLexer = new BilAdtLexer(CharStreams.fromFileName(fileName))
    val tokens = new CommonTokenStream(adtLexer)
    // ADT
    val parser = new BilAdtParser(tokens)

    parser.setBuildParseTree(true)
    val b = parser.file(); // abstract syntax tree

    // extract all statement objects from the tree
    val statementLoader = new AdtStatementLoader()
    val walker = new ParseTreeWalker()
    walker.walk(statementLoader, b)

    val symsLexer = new SymsLexer(CharStreams.fromFileName(elfFileName))
    val symsTokens = new CommonTokenStream(symsLexer)
    val symsParser = new SymsParser(symsTokens)
    symsParser.setBuildParseTree(true)
    val symsListener = new SymbolTableListener()
    walker.walk(symsListener, symsParser.syms)

    // TODO duplicated code for default value
    val flowGraph = FlowGraph.fromStmts(statementLoader.stmts.toList, statementLoader.varSizes.toMap)

    val state = State(
      flowGraph,
      statementLoader.rely.getOrElse(Bool.True), // TODO check default
      Bool.False,
      symsListener.symbolTable.toMap,
      statementLoader.varSizes.toMap,
      statementLoader.lPreds.toMap,
      statementLoader.gammaMappings.toMap
    )

    val WL = Worklist(ConstantPropagationAnalysis(state, true), state)
    val analysedState = WL.doAnalysis()

    val updatedState = BoogieTranslator.translate(analysedState)

    VCGen.genVCs(updatedState)
  }

  def generateVCs(fileName: String, elfFileName: String): State = {
    val bilLexer = new BilLexer(CharStreams.fromFileName(fileName))
    val tokens = new CommonTokenStream(bilLexer)
    val parser = new BilParser(tokens)
    parser.setBuildParseTree(true)
    val b = parser.bil(); // abstract syntax tree

    // extract all statement objects from the tree
    val statementLoader = new StatementLoader()
    val walker = new ParseTreeWalker()
    walker.walk(statementLoader, b)

    val symsLexer = new SymsLexer(CharStreams.fromFileName(elfFileName))
    val symsTokens = new CommonTokenStream(symsLexer)
    val symsParser = new SymsParser(symsTokens)
    symsParser.setBuildParseTree(true)
    val symsListener = new SymbolTableListener()
    walker.walk(symsListener, symsParser.syms)

    // TODO duplicated code for default value
    val flowGraph = FlowGraph.fromStmts(statementLoader.stmts.toList, statementLoader.varSizes.toMap)

    val state = State(
      flowGraph,
      statementLoader.rely.getOrElse(Bool.True), // TODO check default
      Bool.False,
      symsListener.symbolTable.toMap,
      statementLoader.varSizes.toMap,
      statementLoader.lPreds.toMap,
      statementLoader.gammaMappings.toMap
    )

    val WL = Worklist(ConstantPropagationAnalysis(state, true), state)
    val analysedState = WL.doAnalysis()

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
