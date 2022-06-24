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
    val adtLexer = BilAdtLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(adtLexer)
    // ADT
    val parser = BilAdtParser(tokens)

    parser.setBuildParseTree(true)

    val program = AdtStatementLoader.visitProject(parser.project())

    val elfLexer = SymsLexer(CharStreams.fromFileName(elfFileName))
    val elfTokens = CommonTokenStream(elfLexer)
    val elfParser = SymsParser(elfTokens)
    elfParser.setBuildParseTree(true)

    val (externalFunctions, globals) = ElfLoader.visitSyms(elfParser.syms())

    //println(externalFunctions)
    //println(globals)


    /*
    TODO analyses/transformations
    -type checking
    --coerce bv literals to be the right size (bap sometimes messes this up for comparisons)
    -make sure there's no sneaky stack accesses
    -constant propagation to properly analyse control flow and replace all indirect calls
    -identify external calls
    -check for use of uninitialised registers in procedures to pass them in
    -points to/alias analysis to split memory into separate maps as much as possible? do we want this?
    -make memory reads better?
    -instrument with gammas, vcs, rely, guarantee
     */

    val programUnusedRemoved = BoogieTranslator(program).stripUnreachableFunctions
    BoogieTranslator(programUnusedRemoved).translate
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