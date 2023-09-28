package util
import analysis._
import cfg_visualiser.{OtherOutput, Output, OutputKindE}
import bap._
import ir._
import boogie._
import specification._
import BilParser._
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating._

import java.io.{File, PrintWriter}
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters._
import analysis.solvers._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
object RunUtils {
  var memoryRegionAnalysisResults: Option[Map[CfgNode, _]] = None

  var iterations = 0;

  // ids reserved by boogie
  val reserved: Set[String] = Set("free")

  // constants
  private val exitRegister: Variable = Variable("R30", BitVecType(64))

  def generateVCsAdt(fileName: String, elfFileName: String, specFileName: Option[String], performAnalysis: Boolean, performInterpret: Boolean): BProgram = {

    val adtLexer = BilAdtLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(adtLexer)
    val parser = BilAdtParser(tokens)

    parser.setBuildParseTree(true)

    println("[!] Generating IR")
    val program = AdtStatementLoader.visitProject(parser.project())

    val elfLexer = SymsLexer(CharStreams.fromFileName(elfFileName))
    val elfTokens = CommonTokenStream(elfLexer)
    val elfParser = SymsParser(elfTokens)
    elfParser.setBuildParseTree(true)
    println("[!] Parsing .relf")
    val (externalFunctions, globals, globalOffsets) = ElfLoader.visitSyms(elfParser.syms())

    //println(globalOffsets)
    //val procmap = program.subroutines.map(s => (s.name, s.address)).toMap
    //println(procmap)
    //println(globals)
    /*
    TODO analyses/transformations
    -type checking
    -make sure there's no sneaky stack accesses
    -constant propagation to properly analyse control flow and replace all indirect calls
    -identify external calls
    -check for use of uninitialised registers in procedures to pass them in
    -points to/alias analysis to split memory into separate maps as much as possible? do we want this?
    -make memory reads better?
     */

    val externalNames = externalFunctions.map(e => e.name)

    println("[!] Translating from BAP to IR")
    val IRTranslator = BAPToIR(program)
    var IRProgram = IRTranslator.translate

    val specification = specFileName match {
      case Some(s) => val specLexer = SpecificationsLexer(CharStreams.fromFileName(s))
        val specTokens = CommonTokenStream(specLexer)
        val specParser = SpecificationsParser(specTokens)
        specParser.setBuildParseTree(true)
        val specLoader = SpecificationLoader(globals, IRProgram)
        specLoader.visitSpecification(specParser.specification())
      case None => Specification(globals, Map(), List(), List(), List())
    }

    if (performInterpret) {
      Interpret(IRProgram)
    }

    println("[!] Removing external function calls")
    // Remove external function references (e.g. @printf)
    val externalRemover = ExternalRemover(externalNames)
    // Removes BAP naming artefacts (e.g. # preceding variable names)
    val renamer = Renamer(reserved)
    IRProgram = externalRemover.visitProgram(IRProgram)
    IRProgram = renamer.visitProgram(IRProgram)

    if (performAnalysis) {
      iterations = 0;
      IRProgram = analyse(IRProgram, externalFunctions, globals, globalOffsets)
    }

    IRProgram.stripUnreachableFunctions()

    println("[!] Translating to Boogie")
    val boogieTranslator = IRToBoogie(IRProgram, specification)
    println("[!] Done! Exiting...")
    boogieTranslator.translate
  }

  def analyse(IRProgram: Program, externalFunctions: Set[ExternalFunction], globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt]): Program = {
    iterations += 1
    val subroutines = IRProgram.procedures.filter(p => p.address.isDefined).map{(p: Procedure) => BigInt(p.address.get) -> p.name}.toMap
    val globalAddresses = globals.map{(s: SpecGlobal) => s.address -> s.name}.toMap
    val externalAddresses = externalFunctions.map{(e: ExternalFunction) => e.offset -> e.name}.toMap
    println("Globals:" )
    println(globalAddresses)
    println("Global Offsets: ")
    println(globalOffsets)
    println("External: ")
    println(externalAddresses)
    println("Subroutine Addresses:")
    println(subroutines)

    val mergedSubroutines = subroutines ++ externalAddresses

    val cfg = ProgramCfgFactory().fromIR(IRProgram, false, 0)

    println("[!] Running Constant Propagation")
    val solver = ConstantPropagationAnalysis.WorklistSolver(cfg)
    val result = solver.analyze(true).asInstanceOf[Map[CfgNode, Map[Variable, Any]]]
    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(result, solver.stateAfterNode), Output.dotIder), "cpa")

    println("[!] Running MRA") // resolves memory loads from full mem to stack/mem
    val solver2 = MemoryRegionAnalysis.WorklistSolver(cfg, globalAddresses, globalOffsets, mergedSubroutines, result)
    val result2 = solver2.analyze(true).asInstanceOf[Map[CfgNode, MemoryRegion]]
    memoryRegionAnalysisResults = Some(result2)
    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(result2, solver2.stateAfterNode), Output.dotIder), "mra")

    println("[!] Running MMM")
    val mmm = MemoryModelMap()
    mmm.convertMemoryRegions(result2, mergedSubroutines)

    println("[!] Running VSA")
    val solver3 = ValueSetAnalysis.WorklistSolver(cfg, globalAddresses, externalAddresses, globalOffsets, subroutines, mmm, result)
    val result3 = solver3.analyze(false)
    Output.output(OtherOutput(OutputKindE.cfg), cfg.toDot(Output.labeler(result3, solver3.stateAfterNode), Output.dotIder), "vsa")

    println("[!] Resolving CFG")
    val (newIR, modified) = resolveCFG(cfg, result3.asInstanceOf[Map[CfgNode, Map[Expr, Set[Value]]]], IRProgram)
    if (modified) {
      println("[!] Analysing again")
      return analyse(newIR, externalFunctions, globals, globalOffsets)
    }
    val newCFG = ProgramCfgFactory().fromIR(newIR, inlineLimit = 0)
    Output.output(OtherOutput(OutputKindE.cfg), newCFG.toDot(x => x.toString, Output.dotIder), "resolvedCFG")

    println(s"[!] Finished indirect call resolution after $iterations iterations")

    newIR
  }

  def resolveCFG(cfg: ProgramCfg, valueSets: Map[CfgNode, Map[Expr, Set[Value]]], IRProgram: Program): (Program, Boolean) = {
    var modified: Boolean = false
    val worklist = ListBuffer[CfgNode]()
    // find the main function
    cfg.nodes.foreach {
      case main: CfgFunctionEntryNode if main.data.name == "main"=>
        main.succ(true).toSet.union(main.succ(false).toSet).foreach(node => worklist.addOne(node))
      case _ =>
    }

    val visited = scala.collection.mutable.Set[CfgNode]()
    while (worklist.nonEmpty) {
      val node = worklist.remove(0)
        if (!visited.contains(node)) {
          process (node)
          node.succ(true).toSet.union(node.succ(false).toSet).foreach(node => worklist.addOne(node))
          visited.add(node)
        }
    }

    def extractExprFromValue(v: Value): Expr = v match {
      case LiteralValue(expr) => expr
      case localAddress: LocalAddress => localAddress.expr
      case globalAddress: GlobalAddress => globalAddress.expr
      case _ => throw new Exception("Expected a Value with an Expr")
    }

    def process(n: CfgNode): Unit = n match {
      case commandNode: CfgCommandNode =>
        commandNode.data match
          case localAssign: LocalAssign =>
            localAssign.rhs match
              case _: MemoryLoad =>
                if (valueSets(n).contains(localAssign.lhs) && valueSets(n).get(localAssign.lhs).head.size == 1) {
                  val extractedValue = extractExprFromValue(valueSets(n).get(localAssign.lhs).head.head)
                  localAssign.rhs = extractedValue
                  println(s"RESOLVED: Memory load ${localAssign.lhs} resolved to ${extractedValue}")
                } else if (valueSets(n).contains(localAssign.lhs) && valueSets(n).get(localAssign.lhs).head.size > 1) {
                  println(s"RESOLVED: WARN Memory load ${localAssign.lhs} resolved to multiple values, cannot replace")

                   /*
                  // must merge into a single memory variable to represent the possible values
                  // Make a binary OR of all the possible values takes two at a time (incorrect to do BVOR)
                  val values = valueSets(n).get(localAssign.lhs).head
                  val exprValues = values.map(extractExprFromValue)
                  val result = exprValues.reduce((a, b) => BinaryExpr(BVOR, a, b)) // need to express nondeterministic 
                                                                                   // choice between these specific options
                  localAssign.rhs = result
                  */
                }
              case _ =>
          case indirectCall: IndirectCall =>
            if (!commandNode.block.jumps.contains(indirectCall)) {
              // We only replace the calls with DirectCalls in the IR, and don't replace the CommandNode.data
              // Hence if we have already processed this CFG node there will be no corresponding IndirectCall in the IR
              // to replace.
              // We want to replace all possible indirect calls based on this CFG, before regenerating it from the IR
              return
            }
            val valueSet: Map[Expr, Set[Value]] = valueSets(n)
            val functionNames = resolveAddresses(valueSet(indirectCall.target))
            if (functionNames.size == 1) {
              modified = true
              commandNode.block match
                case block: Block =>
                  block.jumps = block.jumps.filter(!_.equals(indirectCall))
                  block.jumps += DirectCall(IRProgram.procedures.filter(_.name.equals(functionNames.head.name)).head, indirectCall.condition, indirectCall.returnTarget)
                case null => throw new Exception("Node not found in nodeToBlock map")
            } else if (functionNames.size > 1) {
              modified = true
              functionNames.foreach(addressValue =>
                commandNode.block match
                  case block: Block =>
                    block.jumps = block.jumps.filter(!_.equals(indirectCall))
                    if (indirectCall.condition.isDefined) {
                      block.jumps += DirectCall(IRProgram.procedures.filter(_.name.equals(addressValue.name)).head, Option(BinaryExpr(BVAND, indirectCall.condition.get, BinaryExpr(BVEQ, indirectCall.target, addressValue.expr))), indirectCall.returnTarget)
                    } else {
                      block.jumps += DirectCall(IRProgram.procedures.filter(_.name.equals(addressValue.name)).head, Option(BinaryExpr(BVEQ, indirectCall.target, addressValue.expr)), indirectCall.returnTarget)
                    }
                  case null => throw new Exception("Node not found in nodeToBlock map")
              )
            } else {
              // must be a call to R30
              if (!indirectCall.target.equals(exitRegister)) {
                throw new Exception(s"Indirect call ${indirectCall} has no possible targets. Value set: ${valueSet(indirectCall.target)}")
              }
            }
          case _ =>
      case _ =>
    }

    def nameExists(name: String): Boolean = {
      IRProgram.procedures.exists(_.name.equals(name))
    }

    def addFakeProcedure(name: String): Unit = {
      IRProgram.procedures += Procedure(name, None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), true)
    }

    def resolveAddresses(valueSet: Set[Value]): Set[AddressValue] = {
      var functionNames: Set[AddressValue] = Set()
      valueSet.foreach {
        case globalAddress: GlobalAddress =>
          if (nameExists(globalAddress.name)) {
            functionNames += globalAddress
            println(s"RESOLVED: Call to Global address ${globalAddress.name} resolved.")
          } else {
            addFakeProcedure(globalAddress.name)
            functionNames += globalAddress
            println(s"Global address ${globalAddress.name} does not exist in the program.  Added a fake function.")
          }
        case localAddress: LocalAddress =>
          if (nameExists(localAddress.name)) {
            functionNames += localAddress
            println(s"RESOLVED: Call to Local address ${localAddress.name}")
          } else {
            addFakeProcedure(localAddress.name)
            functionNames += localAddress
            println(s"Local address ${localAddress.name} does not exist in the program. Added a fake function.")
          }
        case _ =>
      }
      functionNames
    }
    (IRProgram, modified)
  }

  def writeToFile(program: BProgram, outputFileName: String): Unit = {
    try {
      val writer = BufferedWriter(FileWriter(outputFileName, false))
      writer.write(program.toString)
      writer.flush()
      writer.close()
    } catch {
      case _: IOException => System.err.println("Error writing to file.")
    }
  }

  def dump_file(content: String, name: String): Unit = {
    val outFile = new File(s"${name}.txt")
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()
  }

  def dump_plot(content: String, name: String): Unit = {
    val outFile = new File(s"${name}.dot")
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()
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
