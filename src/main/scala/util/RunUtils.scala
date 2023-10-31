package util

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set as MutableSet
import java.io.{File, PrintWriter}
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters.*
import analysis.solvers.*
import analysis.*
import cfg_visualiser.{OtherOutput, Output, OutputKindE}
import bap.*
import ir.*
import boogie.*
import specification.*
import Parsers.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating.*
import util.Logger

import scala.collection.mutable

object RunUtils {
  var memoryRegionAnalysisResults: Map[CfgNode, Set[MemoryRegion]] = Map()

  var iterations = 0

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

  def loadAndTranslate(
      BAPFileName: String,
      readELFFileName: String,
      specFileName: Option[String],
      performAnalysis: Boolean,
      performInterpret: Boolean,
      dumpIL: Boolean
  ): BProgram = {
    val bapProgram = loadBAP(BAPFileName)

    val (externalFunctions, globals, globalOffsets, mainAddress) = loadReadELF(readELFFileName)

    val IRTranslator = BAPToIR(bapProgram, mainAddress)
    var IRProgram = IRTranslator.translate

    val specification = loadSpecification(specFileName, IRProgram, globals)

    Logger.info("[!] Removing external function calls")
    // Remove external function references (e.g. @printf)
    val externalNames = externalFunctions.map(e => e.name)
    val externalRemover = ExternalRemover(externalNames)
    val renamer = Renamer(reserved)
    IRProgram = externalRemover.visitProgram(IRProgram)
    IRProgram = renamer.visitProgram(IRProgram)

    if (dumpIL) {
      dump_file(serialiseIL(IRProgram), "before-analysis.il")
    }

    if (performAnalysis) {
      IRProgram = analyse(IRProgram, externalFunctions, globals, globalOffsets)
      if (dumpIL) {
        dump_file(serialiseIL(IRProgram), "after-analysis.il")
      }
    }

    IRProgram.determineRelevantMemory(globalOffsets)
    IRProgram.stripUnreachableFunctions()
    IRProgram.stackIdentification()

    val specModifies = specification.subroutines.map(s => s.name -> s.modifies).toMap
    IRProgram.setModifies(specModifies)

    if (performInterpret) {
      val interpreter = Interpreter()
      interpreter.interpret(IRProgram)
    }

    Logger.info("[!] Translating to Boogie")
    val boogieTranslator = IRToBoogie(IRProgram, specification)
    Logger.info("[!] Done! Exiting...")
    boogieTranslator.translate
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

    val cfg = ProgramCfgFactory().fromIR(IRProgram)

    Logger.info("[!] Running Constant Propagation")
    val constPropSolver = ConstantPropagationAnalysis.WorklistSolver(cfg)
    val constPropResult: Map[CfgNode, Map[Variable, ConstantPropagationLattice.Element]] = constPropSolver.analyze(true)
    /*
    Output.output(
      OtherOutput(OutputKindE.cfg),
      cfg.toDot(Output.labeler(constPropResult, constPropSolver.stateAfterNode), Output.dotIder),
      "cpa"
    )
    */
    dump_file(printAnalysisResults(cfg, constPropResult), "cpa")

    Logger.info("[!] Running MRA")
    val mraSolver = MemoryRegionAnalysis.WorklistSolver(cfg, globalAddresses, globalOffsets, mergedSubroutines, constPropResult)
    val mraResult: Map[CfgNode, Set[MemoryRegion]] = mraSolver.analyze(true)
    memoryRegionAnalysisResults = mraResult
    /* Output.output(
      OtherOutput(OutputKindE.cfg),
      cfg.toDot(Output.labeler(mraResult, mraSolver.stateAfterNode), Output.dotIder),
      "mra"
    )*/
    dump_file(printAnalysisResults(cfg, mraResult), "mra")

    Logger.info("[!] Running MMM")
    val mmm = MemoryModelMap()
    mmm.convertMemoryRegions(mraResult, mergedSubroutines)

    Logger.info("[!] Running VSA")
    val vsaSolver =
      ValueSetAnalysis.WorklistSolver(cfg, globalAddresses, externalAddresses, globalOffsets, subroutines, mmm, constPropResult)
    val vsaResult: Map[CfgNode, Map[Variable | MemoryRegion, Set[Value]]]  = vsaSolver.analyze(false)
    /*
    Output.output(
      OtherOutput(OutputKindE.cfg),
      cfg.toDot(Output.labeler(vsaResult, vsaSolver.stateAfterNode), Output.dotIder),
      "vsa"
    )
    */
    dump_file(printAnalysisResults(cfg, vsaResult), "vsa")

    Logger.info("[!] Resolving CFG")
    val (newIR, modified): (Program, Boolean) = resolveCFG(cfg, vsaResult.asInstanceOf[Map[CfgNode, Map[Variable, Set[Value]]]], IRProgram)
    /*
    if (modified) {
      Logger.info(s"[!] Analysing again (iter $iterations)")
      return analyse(newIR, externalFunctions, globals, globalOffsets)
    }
    */
    val newCFG = ProgramCfgFactory().fromIR(newIR)
    Output.output(OtherOutput(OutputKindE.cfg), newCFG.toDot(x => x.toString, Output.dotIder), "resolvedCFG")

    Logger.info(s"[!] Finished indirect call resolution after $iterations iterations")

    newIR
  }

  def printAnalysisResults(cfg: ProgramCfg, result: Map[CfgNode, _]): String = {
    val functionEntries = cfg.nodes.collect { case n: CfgFunctionEntryNode => n }.toSeq.sortBy(_.data.name)
    val s = StringBuilder()

    for (f <- functionEntries) {
      val stack: mutable.Stack[CfgNode] = mutable.Stack()
      val visited: mutable.Set[CfgNode] = mutable.Set()
      stack.push(f)
      while (stack.nonEmpty) {
        val next = stack.pop()
        if (!visited.contains(next)) {
          visited.add(next)
          next.match {
            case c: CfgCommandNode => printNode(c)
            case c: CfgFunctionEntryNode => printNode(c)
            case c: CfgCallNoReturnNode => s.append(System.lineSeparator())
            case _ =>
          }
          val successors = next.succ(true)
          if (successors.size > 1) {
            val successorsCmd = successors.collect { case c: CfgCommandNode => c }.toSeq.sortBy(_.data.label)
            printGoTo(successorsCmd)
            for (s <- successorsCmd) {
              if (!visited.contains(s)) {
                stack.push(s)
              }
            }
          } else if (successors.size == 1) {
            val successor = successors.head
            if (!visited.contains(successor)) {
              stack.push(successor)
            } else {
              successor.match {
                case c: CfgCommandNode => printGoTo(Seq(c))
                case _ =>
              }
            }
          }
        }
      }
      s.append(System.lineSeparator())
    }

    def printNode(node: CfgNode): Unit = {
      s.append(node)
      s.append(" :: ")
      s.append(result(node))
      s.append(System.lineSeparator())
    }

    def printGoTo(nodes: Seq[CfgCommandNode]): Unit = {
      s.append("[GoTo] ")
      s.append(nodes.map(_.data.labelStr).mkString(", "))
      s.append(System.lineSeparator())
      s.append(System.lineSeparator())
    }

    s.toString
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

    def process(n: CfgNode): Unit = n match {
      /*
      case c: CfgStatementNode =>
        c.data match

        //We do not want to insert the VSA results into the IR like this
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
      case c: CfgJumpNode =>
        val block = c.block
        c.data match
          case indirectCall: IndirectCall =>
            if (!block.jumps.contains(indirectCall)) {
              // We only replace the calls with DirectCalls in the IR, and don't replace the CommandNode.data
              // Hence if we have already processed this CFG node there will be no corresponding IndirectCall in the IR
              // to replace.
              // We want to replace all possible indirect calls based on this CFG, before regenerating it from the IR
              return
            }
            val valueSet: Map[Variable, Set[Value]] = valueSets(n)
            val targetNames = resolveAddresses(valueSet(indirectCall.target)).map(_.name).toList.sorted
            val targets = targetNames.map(name => IRProgram.procedures.filter(_.name.equals(name)).head)
            if (targets.size == 1) {
              modified = true
              val newCall = DirectCall(targets.head, indirectCall.condition, indirectCall.returnTarget)
              block.jumps.remove(block.jumps.indexOf(indirectCall))
              block.jumps.append(newCall)
            } else if (targets.size > 1) {
              modified = true
              val procedure = c.parent.data
              indirectCall.condition match {
                // it doesn't seem like calls can actually have conditions in the ARM64 instruction set
                case Some(_) => throw Exception("indirect call has a condition")
                case None =>
                  val newBlocks = for (t <- targets) yield {
                    val assume = Assume(BinaryExpr(BVEQ, indirectCall.target, BitVecLiteral(t.address.get, 64)))
                    val newLabel: String = block.label + t.name
                    val directCall = DirectCall(t, None, indirectCall.returnTarget)
                    Block(newLabel, None, ArrayBuffer(assume), ArrayBuffer(directCall))
                  }
                  procedure.blocks.addAll(newBlocks)
                  block.jumps.remove(block.jumps.indexOf(indirectCall))
                  val newCall = NonDetGoTo(newBlocks)
                  block.jumps.append(newCall)
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
    val outFile = new File(s"$name.txt")
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()
  }

  def dump_plot(content: String, name: String): Unit = {
    val outFile = new File(s"$name.dot")
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()
  }

}
