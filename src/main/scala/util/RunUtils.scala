package util

import BilParser._
//import analysis._
import astnodes._
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating._
//import vcgen.{State, VCGen}

import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters._

object RunUtils {

  def generateVCsAdt(fileName: String, elfFileName: String): Program = {
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
    --coerce bv1 to boolean properly instead of current mediocre approach
    -determine which is initial procedure - unclear?
    -define all global variables & functions - only ones that are used
    -constant propagation to properly analyse control flow and replace all indirect calls
    -initialise function calls properly with required variables including adding modifies clause, passing in any uninitialised registers
    -exit function calls properly
    -points to/alias analysis to split memory into separate maps as much as possible
    -replace all memory accesses with proper versions that correctly split things per byte
    -deal with overflow ugh
    -general simplification of multiple extracts etc.
    -instrument with gammas, vcs, rely, guarantee
     */

    program
  }

  // TODO copy pasted
  def writeToFile(program: Program, outputFileName: String = "boogie_out.bpl"): Unit = {
    try {
      val writer = new BufferedWriter(new FileWriter(outputFileName, false))
      writer.write(program.toBoogieString)
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