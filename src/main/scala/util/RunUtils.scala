package util

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set as MutableSet
import java.io.{File, PrintWriter}
import java.io.{BufferedWriter, FileWriter, IOException}
import scala.jdk.CollectionConverters.*
import analysis.solvers.*
import analysis.*
import cfg_visualiser.Output
import bap.*
import ir.*
import boogie.*
import specification.*
import Parsers.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import translating.*
import util.Logger
import intrusiveList.IntrusiveList

import scala.collection.mutable


object RunUtils {
  var memoryRegionAnalysisResults: Map[CfgNode, Set[MemoryRegion]] = Map()

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
    writeToFile(boogieProgram.toString, q.outputPrefix)
  }

  def loadAndTranslate(q: BASILConfig): BProgram = {
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
    val returnUnifier = ConvertToSingleProcedureReturn()
    IRProgram = externalRemover.visitProgram(IRProgram)
    IRProgram = renamer.visitProgram(IRProgram)
    IRProgram = returnUnifier.visitProgram(IRProgram)


    q.loading.dumpIL.foreach(s => writeToFile(serialiseIL(IRProgram), s"$s-before-analysis.il"))

    q.staticAnalysis.foreach { analysisConfig =>
      IRProgram = analyse(IRProgram, externalFunctions, globals, globalOffsets, analysisConfig, 1)
      analysisConfig.dumpILToPath.foreach(s => writeToFile(serialiseIL(IRProgram), s"$s-after-analysis.il"))
    }

    IRProgram.determineRelevantMemory(globalOffsets)
    IRProgram.stripUnreachableFunctions()
    IRProgram.stackIdentification()

    val specModifies = specification.subroutines.map(s => s.name -> s.modifies).toMap
    IRProgram.setModifies(specModifies)

    /*
    if (q.runInterpret) {
      val interpreter = Interpreter()
      interpreter.interpret(IRProgram)
    }
    */

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
      globalOffsets: Map[BigInt, BigInt],
      config: StaticAnalysisConfig,
      iteration: Int
  ): Program = {
    val subroutines = IRProgram.procedures
      .filter(p => p.address.isDefined)
      .map(p => BigInt(p.address.get) -> p.name)
      .toMap
    val globalAddresses = globals.map(s => s.address -> s.name).toMap
    val externalAddresses = externalFunctions.map(e => e.offset -> e.name).toMap
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

    val domain = computeDomain(IRProgram)

    Logger.info("[!] Running Constant Propagation")
    val constPropSolver = ConstantPropagationSolver(cfg)
    val constPropResult = constPropSolver.analyze()

    def newSolverTest(): Unit = {
      val ilcpsolver = IRSimpleValueAnalysis.Solver(IRProgram)
      val newCPResult: ilcpsolver.lattice.Element = ilcpsolver.analyze()

      val newRes = newCPResult.flatMap((x, y) => y.flatMap {
        case (_, el) if el == FlatLattice[BitVecLiteral].top || el == FlatLattice[BitVecLiteral].bottom => None
        case z => Some(z)
      })
      val oldRes = constPropResult.flatMap((x, y) => y.flatMap {
        case (_, el) if el == FlatLattice[BitVecLiteral].top || el == FlatLattice[BitVecLiteral].bottom => None
        case z => Some(z)
      })
      val both = newRes.toSet.intersect(oldRes.toSet)
      val notnew = (newRes.toSet).filter(x => !both.contains(x))
      val notOld = (oldRes.toSet).filter(x => !both.contains(x))
      // newRes and oldRes should have value equality

      config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(IRProgram, newCPResult, iteration), s"${s}_newconstprop$iteration.txt"))
      config.analysisResultsPath.foreach(s => writeToFile(toDot(IRProgram), s"program.dot"))
      config.analysisResultsPath.foreach(s => writeToFile(toDot(IRProgram, newCPResult.map((k,v) => (k, v.toString))), s"program-constprop.dot"))
    }
    newSolverTest()

    config.analysisDotPath.foreach(s => writeToFile(cfg.toDot(Output.labeler(constPropResult, true), Output.dotIder), s"${s}_constprop$iteration.dot"))
    config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(cfg, constPropResult, iteration), s"${s}_constprop$iteration.txt"))

    config.analysisDotPath.foreach(s => writeToFile(cfg.toDot(Output.labeler(constPropResult, true), Output.dotIder), s"${s}_constprop$iteration.dot"))
    config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(cfg, constPropResult, iteration), s"${s}_constprop$iteration.txt"))

    Logger.info("[!] Running MRA")
    val mraSolver = MemoryRegionAnalysisSolver(cfg, globalAddresses, globalOffsets, mergedSubroutines, constPropResult)
    val mraResult = mraSolver.analyze()
    memoryRegionAnalysisResults = mraResult

    config.analysisDotPath.foreach(s => writeToFile(cfg.toDot(Output.labeler(mraResult, true), Output.dotIder), s"${s}_mra$iteration.dot"))
    config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(cfg, mraResult, iteration), s"${s}_mra$iteration.txt"))

    Logger.info("[!] Running MMM")
    val mmm = MemoryModelMap()
    mmm.convertMemoryRegions(mraResult, mergedSubroutines)

    Logger.info("[!] Running VSA")
    val vsaSolver = ValueSetAnalysisSolver(cfg, globalAddresses, externalAddresses, globalOffsets, subroutines, mmm, constPropResult)
    val vsaResult = vsaSolver.analyze()

    config.analysisDotPath.foreach(s => writeToFile(cfg.toDot(Output.labeler(vsaResult, true), Output.dotIder), s"${s}_vsa$iteration.dot"))
    config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(cfg, vsaResult, iteration), s"${s}_vsa$iteration.txt"))

    Logger.info("[!] Resolving CFG")
    val (newIR, modified): (Program, Boolean) = resolveCFG(cfg, vsaResult, IRProgram)
    if (modified) {
      Logger.info(s"[!] Analysing again (iter $iteration)")
      return analyse(newIR, externalFunctions, globals, globalOffsets, config, iteration + 1)
    }

    config.analysisDotPath.foreach { s =>
      val newCFG = ProgramCfgFactory().fromIR(newIR)
      writeToFile(newCFG.toDot(x => x.toString, Output.dotIder), s"${s}_resolvedCFG.dot")
    }

    Logger.info(s"[!] Finished indirect call resolution after $iteration iterations")

    newIR
  }

  def printAnalysisResults(cfg: Program, result: Map[IntraProcIRCursor.Node, _], iteration: Int): String = {
    val functionEntries = cfg.procedures
    val s = StringBuilder()
    s.append(System.lineSeparator())
    for (f <- functionEntries) {
      val stack: mutable.Stack[IntraProcIRCursor.Node] = mutable.Stack()
      val visited: mutable.Set[IntraProcIRCursor.Node] = mutable.Set()
      stack.push(f)
      var previousBlock: String = ""
      var isEntryNode = false
      while (stack.nonEmpty) {
        val next = stack.pop()
        if (!visited.contains(next)) {
          visited.add(next)
          next.match {
            case c: Block => printBlock(c)
            case c: Call => s.append(System.lineSeparator())
              isEntryNode = false
              printNode(c)
            case c: Command =>
              //if (c.parent.label != previousBlock) {
              //  printBlock(c.parent)
              //}
              printNode(c)
              previousBlock = c.parent.label
              isEntryNode = false
            case c: Procedure =>
              printNode(c)
              isEntryNode = true
            case _ => isEntryNode = false
          }
          val successors = IntraProcIRCursor.succ(next)
          if (successors.size > 1) {
            val successorsCmd = successors.collect { case c: Command => c }.toSeq.sortBy(_.label)
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
            }
            successor.match {
              case c: Command if (c.parent.label != previousBlock) && (!isEntryNode) => printGoTo(Seq(c))
              case _ =>
            }
          }
        }
      }
      s.append(System.lineSeparator())
    }

    def printNode(node: IntraProcIRCursor.Node): Unit = {
      node match {
        case _: Statement => s.append("[Stmt] ")
        case _: Procedure => s.append("[FunctionEntry] ")
        case _: Call => s.append("[Jmp] ")
        case _ => ()
      }

      s.append(node)
      s.append(" :: ")
      s.append(result(node))
      s.append(System.lineSeparator())
    }

    def printGoTo(nodes: Seq[Command]): Unit = {
      s.append("[GoTo] ")
      s.append(nodes.map(_.label).mkString(", "))
      s.append(System.lineSeparator())
      s.append(System.lineSeparator())
    }

    def printBlock(node: Block): Unit = {
      s.append("[Block] ")
      s.append(node.label)
      s.append(System.lineSeparator())
    }


    s.toString()
  }


    def printAnalysisResults(cfg: ProgramCfg, result: Map[CfgNode, _], iteration: Int): String = {
    val functionEntries = cfg.nodes.collect { case n: CfgFunctionEntryNode => n }.toSeq.sortBy(_.data.name)
    val s = StringBuilder()
    s.append(System.lineSeparator())
    for (f <- functionEntries) {
      val stack: mutable.Stack[CfgNode] = mutable.Stack()
      val visited: mutable.Set[CfgNode] = mutable.Set()
      stack.push(f)
      var previousBlock: String = ""
      var isEntryNode = false
      while (stack.nonEmpty) {
        val next = stack.pop()
        if (!visited.contains(next)) {
          visited.add(next)
          next.match {
            case c: CfgCommandNode =>
              if (c.block.label != previousBlock) {
                printBlock(c)
              }
              printNode(c)
              previousBlock = c.block.label
              isEntryNode = false
            case c: CfgFunctionEntryNode =>
              printNode(c)
              isEntryNode = true
            case c:
              CfgCallNoReturnNode => s.append(System.lineSeparator())
              isEntryNode = false
            case _ => isEntryNode = false
          }
          val successors = next.succIntra
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
            }
            successor.match {
              case c: CfgCommandNode if (c.block.label != previousBlock) && (!isEntryNode) => printGoTo(Seq(c))
              case _ =>
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
      s.append(nodes.map(_.block.label).mkString(", "))
      s.append(System.lineSeparator())
      s.append(System.lineSeparator())
    }

    def printBlock(node: CfgCommandNode): Unit = {
      s.append("[Block] ")
      s.append(node.block.label)
      s.append(System.lineSeparator())
    }

    s.toString
  }

  def resolveCFG(
      cfg: ProgramCfg,
      valueSets: Map[CfgNode, Map[Variable | MemoryRegion, Set[Value]]],
      IRProgram: Program
  ): (Program, Boolean) = {
    var modified: Boolean = false
    val worklist = ListBuffer[CfgNode]()
    cfg.startNode.succIntra.union(cfg.startNode.succInter).foreach(node => worklist.addOne(node))

    val visited = MutableSet[CfgNode]()
    while (worklist.nonEmpty) {
      val node = worklist.remove(0)
      if (!visited.contains(node)) {
        process(node)
        node.succIntra.union(node.succInter).foreach(node => worklist.addOne(node))
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
            if (block.jump != indirectCall) {
              // We only replace the calls with DirectCalls in the IR, and don't replace the CommandNode.data
              // Hence if we have already processed this CFG node there will be no corresponding IndirectCall in the IR
              // to replace.
              // We want to replace all possible indirect calls based on this CFG, before regenerating it from the IR
              return
            }
            val valueSet = valueSets(n)
            val targetNames = resolveAddresses(valueSet(indirectCall.target)).map(_.name).toList.sorted
            val targets = targetNames.map(name => IRProgram.procedures.filter(_.name.equals(name)).head)
            if (targets.size == 1) {
              modified = true
              block.replaceJump(DirectCall(targets.head, indirectCall.returnTarget, indirectCall.label))
            } else if (targets.size > 1) {
              modified = true
              val procedure = c.parent.data
              val newBlocks = ArrayBuffer[Block]()
              for (t <- targets) {
                val assume = Assume(BinaryExpr(BVEQ, indirectCall.target, BitVecLiteral(t.address.get, 64)), null)
                val newLabel: String = block.label + t.name
                val bl = Block(newLabel, None, ArrayBuffer(assume), DirectCall(t, indirectCall.returnTarget, None))
                //val directCall = DirectCall(t, indirectCall.returnTarget, null)
                newBlocks.append(bl)
              }
              procedure.addBlocks(newBlocks)
              block.replaceJump(GoTo(newBlocks, indirectCall.label))
            }
          case _ =>
      case _ =>
    }

    def nameExists(name: String): Boolean = {
      IRProgram.procedures.exists(_.name.equals(name))
    }

    def addFakeProcedure(name: String): Unit = {
      IRProgram.procedures += Procedure(name)
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

  def writeToFile(content: String, fileName: String): Unit = {
    val outFile = File(fileName)
    val pw = PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()
  }

}
