package util

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set as MutableSet
import java.io.{File, PrintWriter}
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters._
import analysis.solvers._

import analysis._
import cfg_visualiser.{OtherOutput, Output, OutputKindE}
import bap._
import ir._
import boogie._
import specification._
import Parsers._
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating._
import util.Logger

object RunUtils {
  var memoryRegionAnalysisResults: Map[CfgNode, Set[MemoryRegion]] = Map()

  var iterations = 0;

  // ids reserved by boogie
  val reserved: Set[String] = Set("free")

  // constants
  private val exitRegister: Variable = Register("R30", BitVecType(64))

  def loadBAP(fileName: String): BAPProgram = {
    val ADTLexer = BAP_ADTLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(ADTLexer)
    val parser = BAP_ADTParser(tokens)

    parser.setBuildParseTree(true)

    BAPLoader.visitProject(parser.project())
  }

  def loadReadELF(fileName: String): (Set[ExternalFunction], Set[SpecGlobal], Map[BigInt, BigInt], Int) = {
    val lexer = ReadELFLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(lexer)
    val parser = ReadELFParser(tokens)
    parser.setBuildParseTree(true)
    ReadELFLoader.visitSyms(parser.syms())
  }

  def loadSpecification(filename: Option[String], program: Program, globals: Set[SpecGlobal]): Specification = {
    filename match {
      case Some(s) =>
        val specLexer = SpecificationsLexer(CharStreams.fromFileName(s))
        val specTokens = CommonTokenStream(specLexer)
        val specParser = SpecificationsParser(specTokens)
        specParser.setBuildParseTree(true)
        val specLoader = SpecificationLoader(globals, program)
        specLoader.visitSpecification(specParser.specification())
      case None => Specification(globals, Map(), List(), List(), List(), Set())
    }
  }

  def run(q: BASILConfig): Unit = {
    Logger.info("[!] Writing file...")
    val boogieProgram = loadAndTranslate(q)
    RunUtils.writeToFile(boogieProgram, q.outputPrefix + ".bpl")
  }

  def loadAndTranslate(
                      q: BASILConfig
  ): BProgram = {
    /**
     *  Loading phase
     */
    val bapProgram = loadBAP(q.loading.adtFile)
    val (externalFunctions, globals, globalOffsets, mainAddress) = loadReadELF(q.loading.relfFile)

    val IRTranslator = BAPToIR(bapProgram, mainAddress)
    var IRProgram = IRTranslator.translate

    val specification = loadSpecification(q.loading.specFile, IRProgram, globals)

    /**
     * Analysis Phase
     */
    Logger.info("[!] Removing external function calls")
    // Remove external function references (e.g. @printf)
    val externalNames = externalFunctions.map(e => e.name)
    val externalRemover = ExternalRemover(externalNames)
    val renamer = Renamer(reserved)
    IRProgram = externalRemover.visitProgram(IRProgram)
    IRProgram = renamer.visitProgram(IRProgram)

    if (q.loading.dumpIL) {
      dump_file(serialiseIL(IRProgram), q.outputPrefix +  "-before-analysis.il")
    }

    q.staticAnalysis match {
      case Some(analysisConfig) => {
        IRProgram = analyse(IRProgram, externalFunctions, globals, globalOffsets)
        if (analysisConfig.dumpILEveryPhase) {
          dump_file(serialiseIL(IRProgram), q.outputPrefix +  "-after-analysis.il")
        }
      }
      case None => {}
    }

    IRProgram.determineRelevantMemory(globalOffsets)
    IRProgram.stripUnreachableFunctions()
    IRProgram.stackIdentification()

    val specModifies = specification.subroutines.map(s => s.name -> s.modifies).toMap
    IRProgram.setModifies(specModifies)

    if (q.runInterpret) {
      val interpreter = Interpreter()
      interpreter.interpret(IRProgram)
    }

    Logger.info("[!] Translating to Boogie")
    val boogieTranslator = IRToBoogie(IRProgram, specification)
    Logger.info("[!] Done! Exiting...")
    val boogieProgram = boogieTranslator.translate(q.boogieTranslation)
    boogieProgram

  }

  def analyse(
      IRProgram: Program,
      externalFunctions: Set[ExternalFunction],
      globals: Set[SpecGlobal],
      globalOffsets: Map[BigInt, BigInt]
  ): Program = {
    iterations += 1
    val subroutines = IRProgram.procedures
      .filter(p => p.address.isDefined)
      .map { (p: Procedure) => BigInt(p.address.get) -> p.name }
      .toMap
    val globalAddresses = globals.map { (s: SpecGlobal) => s.address -> s.name }.toMap
    val externalAddresses = externalFunctions.map { (e: ExternalFunction) => e.offset -> e.name }.toMap
    Logger.info("Globals:")
    Logger.info(globalAddresses)
    Logger.info("Global Offsets: ")
    Logger.info(globalOffsets)
    Logger.info("External: ")
    Logger.info(externalAddresses)
    Logger.info("Subroutine Addresses:")
    Logger.info(subroutines)

    val mergedSubroutines = subroutines ++ externalAddresses

    val cfg = ProgramCfgFactory().fromIR(IRProgram, false, 0)

    Logger.info("[!] Running Constant Propagation")
    val constPropSolver = ConstantPropagationAnalysis.WorklistSolver(cfg)
    val constPropResult: Map[CfgNode, Map[Variable, ConstantPropagationLattice.Element]] = constPropSolver.analyze(true)
    Output.output(
      OtherOutput(OutputKindE.cfg),
      cfg.toDot(Output.labeler(constPropResult, constPropSolver.stateAfterNode), Output.dotIder),
      "cpa"
    )

    Logger.info("[!] Running MRA")
    val mraSolver = MemoryRegionAnalysis.WorklistSolver(cfg, globalAddresses, globalOffsets, mergedSubroutines, constPropResult)
    val mraResult: Map[CfgNode, Set[MemoryRegion]] = mraSolver.analyze(true)
    memoryRegionAnalysisResults = mraResult
    Output.output(
      OtherOutput(OutputKindE.cfg),
      cfg.toDot(Output.labeler(mraResult, mraSolver.stateAfterNode), Output.dotIder),
      "mra"
    )

    Logger.info("[!] Running MMM")
    val mmm = MemoryModelMap()
    mmm.convertMemoryRegions(mraResult, mergedSubroutines)

    Logger.info("[!] Running VSA")
    val vsaSolver =
      ValueSetAnalysis.WorklistSolver(cfg, globalAddresses, externalAddresses, globalOffsets, subroutines, mmm, constPropResult)
    val vsaResult: Map[CfgNode, Map[Variable | MemoryRegion, Set[Value]]]  = vsaSolver.analyze(false)
    Output.output(
      OtherOutput(OutputKindE.cfg),
      cfg.toDot(Output.labeler(vsaResult, vsaSolver.stateAfterNode), Output.dotIder),
      "vsa"
    )

    Logger.info("[!] Resolving CFG")
    val (newIR, modified) = resolveCFG(cfg, vsaResult.asInstanceOf[Map[CfgNode, Map[Variable, Set[Value]]]], IRProgram)
    if (modified) {
      Logger.info(s"[!] Analysing again (iter $iterations)")
      return analyse(newIR, externalFunctions, globals, globalOffsets)
    }
    val newCFG = ProgramCfgFactory().fromIR(newIR)
    Output.output(OtherOutput(OutputKindE.cfg), newCFG.toDot(x => x.toString, Output.dotIder), "resolvedCFG")

    Logger.info(s"[!] Finished indirect call resolution after $iterations iterations")

    newIR
  }

  def resolveCFG(
      cfg: ProgramCfg,
      valueSets: Map[CfgNode, Map[Variable, Set[Value]]],
      IRProgram: Program
  ): (Program, Boolean) = {
    var modified: Boolean = false
    val worklist = ListBuffer[CfgNode]()
    cfg.startNode.succ(true).union(cfg.startNode.succ(false)).foreach(node => worklist.addOne(node))

    val visited = MutableSet[CfgNode]()
    while (worklist.nonEmpty) {
      val node = worklist.remove(0)
      if (!visited.contains(node)) {
        process(node)
        node.succ(true).union(node.succ(false)).foreach(node => worklist.addOne(node))
        visited.add(node)
      }
    }

    def extractExprFromValue(v: Value): Expr = v match {
      case LiteralValue(expr)           => expr
      case localAddress: LocalAddress   => localAddress.expr
      case globalAddress: GlobalAddress => globalAddress.expr
      case _                            => throw new Exception("Expected a Value with an Expr")
    }

    def process(n: CfgNode): Unit = n match {
      case commandNode: CfgCommandNode =>
        commandNode.data match
          /*
          We do not want to insert the VSA results into the IR like this
          case localAssign: LocalAssign =>
            localAssign.rhs match
              case _: MemoryLoad =>
                if (valueSets(n).contains(localAssign.lhs) && valueSets(n).get(localAssign.lhs).head.size == 1) {
                  val extractedValue = extractExprFromValue(valueSets(n).get(localAssign.lhs).head.head)
                  localAssign.rhs = extractedValue
                  Logger.info(s"RESOLVED: Memory load ${localAssign.lhs} resolved to ${extractedValue}")
                } else if (valueSets(n).contains(localAssign.lhs) && valueSets(n).get(localAssign.lhs).head.size > 1) {
                  Logger.info(s"RESOLVED: WARN Memory load ${localAssign.lhs} resolved to multiple values, cannot replace")

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
          */
          case indirectCall: IndirectCall =>
            if (!commandNode.block.jumps.contains(indirectCall)) {
              // We only replace the calls with DirectCalls in the IR, and don't replace the CommandNode.data
              // Hence if we have already processed this CFG node there will be no corresponding IndirectCall in the IR
              // to replace.
              // We want to replace all possible indirect calls based on this CFG, before regenerating it from the IR
              return
            }
            val valueSet: Map[Variable, Set[Value]] = valueSets(n)
            val functionNames = resolveAddresses(valueSet(indirectCall.target))
            if (functionNames.size == 1) {
              modified = true
              val block = commandNode.block
              block.jumps = block.jumps.filter(!_.equals(indirectCall))
              block.jumps += DirectCall(
                IRProgram.procedures.filter(_.name.equals(functionNames.head.name)).head,
                indirectCall.condition,
                indirectCall.returnTarget
              )
            } else if (functionNames.size > 1) {
              modified = true
              functionNames.foreach(addressValue =>
                val block = commandNode.block
                block.jumps = block.jumps.filter(!_.equals(indirectCall))
                if (indirectCall.condition.isDefined) {
                  block.jumps += DirectCall(
                    IRProgram.procedures.filter(_.name.equals(addressValue.name)).head,
                    Option(
                      BinaryExpr(
                        BVAND,
                        indirectCall.condition.get,
                        BinaryExpr(BVEQ, indirectCall.target, addressValue.expr)
                      )
                    ),
                    indirectCall.returnTarget
                  )
                } else {
                  block.jumps += DirectCall(
                    IRProgram.procedures.filter(_.name.equals(addressValue.name)).head,
                    Option(BinaryExpr(BVEQ, indirectCall.target, addressValue.expr)),
                    indirectCall.returnTarget
                  )
                }
              )
            } else {
              // must be a call to R30
              if (!indirectCall.target.equals(exitRegister)) {
                throw new Exception(
                  s"Indirect call ${indirectCall} has no possible targets. Value set: ${valueSet(indirectCall.target)}"
                )
              }
            }
          case _ =>
      case _ =>
    }

    def nameExists(name: String): Boolean = {
      IRProgram.procedures.exists(_.name.equals(name))
    }

    def addFakeProcedure(name: String): Unit = {
      IRProgram.procedures += Procedure(name, None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer())
    }

    def resolveAddresses(valueSet: Set[Value]): Set[AddressValue] = {
      var functionNames: Set[AddressValue] = Set()
      valueSet.foreach {
        case globalAddress: GlobalAddress =>
          if (nameExists(globalAddress.name)) {
            functionNames += globalAddress
            Logger.info(s"RESOLVED: Call to Global address ${globalAddress.name} resolved.")
          } else {
            addFakeProcedure(globalAddress.name)
            functionNames += globalAddress
            Logger.info(s"Global address ${globalAddress.name} does not exist in the program.  Added a fake function.")
          }
        case localAddress: LocalAddress =>
          if (nameExists(localAddress.name)) {
            functionNames += localAddress
            Logger.info(s"RESOLVED: Call to Local address ${localAddress.name}")
          } else {
            addFakeProcedure(localAddress.name)
            functionNames += localAddress
            Logger.info(s"Local address ${localAddress.name} does not exist in the program. Added a fake function.")
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
      case _: IOException => Logger.error("Error writing to file.")
    }
  }

  def dump_file(content: String, name: String): Unit = {
    val outFile = new File(name)
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
