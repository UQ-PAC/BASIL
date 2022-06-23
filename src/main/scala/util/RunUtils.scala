package util

import BilParser._
//import analysis._
import astnodes._
import boogie._
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating._
//import vcgen.{State, VCGen}

import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters._

object RunUtils {

  def generateVCsAdt(fileName: String, elfFileName: String): BProgram = {
    val adtLexer = new BilAdtLexer(CharStreams.fromFileName(fileName))
    val tokens = new CommonTokenStream(adtLexer)
    // ADT
    val parser = new BilAdtParser(tokens)

    parser.setBuildParseTree(true)

    val program = AdtStatementLoader.visitProject(parser.project())

    /*
    TODO: do we still need symbol table?

    val walker = new ParseTreeWalker()
    walker.walk(statementLoader, b)

    val symsLexer = new SymsLexer(CharStreams.fromFileName(elfFileName))
    val symsTokens = new CommonTokenStream(symsLexer)
    val symsParser = new SymsParser(symsTokens)
    symsParser.setBuildParseTree(true)
    val symsListener = new SymbolTableListener()
    walker.walk(symsListener, symsParser.syms)
    */

    // TODO: fix constant propagation arithmetic before turning back on
    /*
     val WL = Worklist(ConstantPropagationAnalysis(state, true), state)
    val analysedState = WL.doAnalysis()
    val updatedState = BoogieTranslator.translate(analysedState)
    */

    /*
    TODO analyses/transformations
    -type checking
    --coerce bv literals to be the right size (bap sometimes messes this up for comparisons)
    -make sure there's no sneaky stack accesses
    -constant propagation to properly analyse control flow and replace all indirect calls
    -identify external calls
    -check for use of uninitialised registers in procedures to pass them in
    -add R29/R30/R31 to all procedure in/out
    -points to/alias analysis to split memory into separate maps as much as possible? do we want this?
    -make memory reads better?
    -instrument with gammas, vcs, rely, guarantee
     */

    BoogieTranslator(program).translate
  }

  def writeToFile(program: BProgram, outputFileName: String): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(outputFileName, false))
      writer.write(program.toString)
      writer.flush()
    } catch {
      case _: IOException => System.err.println("Error writing to file.")
    }
  }
}

class AnalysisTypeException(message: String) extends
  Exception("Tried to operate on two analyses of different types: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}

class AssumptionViolationException(message: String) extends
  Exception("Assumption Violation: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}

class LatticeViolationException(message: String) extends
  Exception("A lattice transfer function broke monotonicity: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}

class SegmentationViolationException(message: String) extends
  Exception("The code attempts to dereference a pointer we don't know about: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}