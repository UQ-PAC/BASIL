package util
import astnodes._
import boogie._
import specification._
import BilParser._
import analysis.{ConstantPropagationAnalysis, ConstantPropagationLattice, IntraproceduralProgramCfg}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating._

import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters._

object RunUtils {

  def generateVCsAdt(fileName: String, elfFileName: String, specFileName: Option[String]): BProgram = {
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

    val specification = specFileName match {
      case Some(s) => val specLexer = SpecificationsLexer(CharStreams.fromFileName(s))
        val specTokens = CommonTokenStream(specLexer)
        val specParser = SpecificationsParser(specTokens)
        specParser.setBuildParseTree(true)
        val specLoader = SpecificationLoader(globals)
        specLoader.visitSpecification(specParser.specification())
      case None => Specification(globals, Map(), Map(), Map(), List(), List())
    }

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

    val externalNames = externalFunctions.map(e => e.name)

    val translator = BoogieTranslator(program, specification)
    translator.stripUnreachableFunctions(externalNames)

    // does not work properly
    //val cfg = IntraproceduralProgramCfg.generateFromProgram(translatorUnusedRemoved.program)
    //val result = new ConstantPropagationAnalysis.WorklistSolver(cfg).analyze()



    translator.translate
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

class AnalysisTypeException(message: String)
    extends Exception("Tried to operate on two analyses of different types: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}

class AssumptionViolationException(message: String) extends Exception("Assumption Violation: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}

class LatticeViolationException(message: String)
    extends Exception("A lattice transfer function broke monotonicity: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}

class SegmentationViolationException(message: String)
    extends Exception("The code attempts to dereference a pointer we don't know about: " + message) {

  def this(message: String, cause: Throwable) = {
    this(message)
    initCause(cause)
  }
}
