package util

import java.io.{File, PrintWriter, FileInputStream, BufferedWriter, FileWriter, IOException}
import com.grammatech.gtirb.proto.IR.IR
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Section.Section
import spray.json.*
import gtirb.*
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
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
import Parsers.SemanticsParser.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.BailErrorStrategy
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token}
import translating.*
import util.Logger
import java.util.Base64
import spray.json.DefaultJsonProtocol.*
import util.intrusive_list.IntrusiveList
import analysis.CfgCommandNode

import scala.annotation.tailrec
import scala.collection.mutable

/** This file contains the main program execution. See RunUtils.loadAndTranslate for the high-level process.
  */

/** Stores the IR Program loaded from the binary and ELF tables, which is modified during analysis and program
  * transformation.
  */
case class IRContext(
    externalFunctions: Set[ExternalFunction],
    globals: Set[SpecGlobal],
    globalOffsets: Map[BigInt, BigInt],
    specification: Specification,
    program: Program // internally mutable
)

/** Stores the results of the static analyses.
  */
case class StaticAnalysisContext(
    cfg: ProgramCfg,
    constPropResult: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]],
    IRconstPropResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
    memoryRegionResult: Map[CfgNode, LiftedElement[Set[MemoryRegion]]],
    vsaResult: Map[CfgNode, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]],
    interLiveVarsResults: Map[CFGPosition, Map[Variable, TwoElement]],
    paramResults: Map[Procedure, Set[Variable]],
    steensgaardResults: Map[RegisterVariableWrapper, Set[RegisterVariableWrapper | MemoryRegion]],
    mmmResults: MemoryModelMap,
    memoryRegionContents: Map[MemoryRegion, Set[BitVecLiteral | MemoryRegion]]
)

/** Results of the main program execution.
  */
case class BASILResult(ir: IRContext, analysis: Option[StaticAnalysisContext], boogie: BProgram)

/** Tools for loading the IR program into an IRContext.
  */
object IRLoading {

  /** Create a context from just an IR program.
    */
  def load(p: Program): IRContext = {
    IRContext(Set.empty, Set.empty, Map.empty, IRLoading.loadSpecification(None, p, Set.empty), p)
  }

  /** Load a program from files using the provided configuration.
    */
  def load(q: ILLoadingConfig): IRContext = {
    val (externalFunctions, globals, globalOffsets, mainAddress) = IRLoading.loadReadELF(q.relfFile, q)

    val program: Program = if (q.inputFile.endsWith(".adt")) {
      val bapProgram = loadBAP(q.inputFile)
      val IRTranslator = BAPToIR(bapProgram, mainAddress)
      IRTranslator.translate
    } else if (q.inputFile.endsWith(".gts")) {
      loadGTIRB(q.inputFile, mainAddress)
    } else {
      throw Exception(s"input file name ${q.inputFile} must be an .adt or .gst file")
    }

    val specification = IRLoading.loadSpecification(q.specFile, program, globals)

    IRContext(externalFunctions, globals, globalOffsets, specification, program)
  }

  def loadBAP(fileName: String): BAPProgram = {
    val ADTLexer = BAP_ADTLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(ADTLexer)
    val parser = BAP_ADTParser(tokens)

    parser.setBuildParseTree(true)

    BAPLoader.visitProject(parser.project())
  }

  def loadGTIRB(fileName: String, mainAddress: Int): Program = {
    val fIn = FileInputStream(fileName)
    val ir = IR.parseFrom(fIn)
    val mods = ir.modules
    val cfg = ir.cfg.get

    val semantics = mods.map(_.auxData("ast").data.toStringUtf8.parseJson.convertTo[Map[String, Array[Array[String]]]])

    def parse_insn(line: String): StmtContext = {
      val semanticsLexer = SemanticsLexer(CharStreams.fromString(line))
      val tokens = CommonTokenStream(semanticsLexer)
      val parser = SemanticsParser(tokens)
      parser.setErrorHandler(BailErrorStrategy())
      parser.setBuildParseTree(true)

      try {
        parser.stmt()
      } catch {
        case e: org.antlr.v4.runtime.misc.ParseCancellationException =>
          val extra = e.getCause match {
            case mismatch: org.antlr.v4.runtime.InputMismatchException =>
              val token = mismatch.getOffendingToken
              s"""
                exn: ${mismatch}
                offending token: ${token}

              ${line.replace('\n', ' ')}
              ${" " * token.getStartIndex}^ here!
              """.stripIndent
            case _ => ""
          }
          Logger.error(s"""Semantics parse error:\n  line: ${line}\n${extra}""")
          throw e
      }
    }

    val parserMap = semantics.map(_.map((k: String, v: Array[Array[String]]) => (k, v.map(_.map(parse_insn)))))

    val GTIRBConverter = GTIRBToIR(mods, parserMap.flatten.toMap, cfg, mainAddress)
    GTIRBConverter.createIR()
  }

  def loadReadELF(
      fileName: String,
      config: ILLoadingConfig
  ): (Set[ExternalFunction], Set[SpecGlobal], Map[BigInt, BigInt], Int) = {
    val lexer = ReadELFLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(lexer)
    val parser = ReadELFParser(tokens)
    parser.setBuildParseTree(true)
    ReadELFLoader.visitSyms(parser.syms(), config)
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
}

/** Methods related to transforming the IR `Program` in-place.
  *
  * These operate over the IRContext, and possibly use static analysis results.
  */
object IRTransform {
  val boogieReserved: Set[String] = Set("free")

  /** Initial cleanup before analysis.
    */
  def doCleanup(ctx: IRContext): IRContext = {
    Logger.info("[!] Removing external function calls")
    // Remove external function references (e.g. @printf)
    val externalNames = ctx.externalFunctions.map(e => e.name)
    val externalRemover = ExternalRemover(externalNames)
    val renamer = Renamer(boogieReserved)
    val returnUnifier = ConvertToSingleProcedureReturn()

    externalRemover.visitProgram(ctx.program)
    renamer.visitProgram(ctx.program)
    returnUnifier.visitProgram(ctx.program)
    ctx
  }

  /** Resolve indirect calls to an address-conditional choice between direct calls using the Value Set Analysis results.
    */
  def resolveIndirectCalls(
      cfg: ProgramCfg,
      valueSets: Map[CfgNode, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]],
      IRProgram: Program
  ): Boolean = {
    var modified: Boolean = false
    val worklist = ListBuffer[CfgNode]()
    cfg.startNode.succIntra.union(cfg.startNode.succInter).foreach(node => worklist.addOne(node))

    val visited = mutable.Set[CfgNode]()
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
            valueSets(n) match {
              case Lift(valueSet) =>
                val targetNames = resolveAddresses(valueSet(indirectCall.target)).map(_.name).toList.sorted
                val targets = targetNames.map(name => IRProgram.procedures.filter(_.name.equals(name)).head)

                if (targets.size == 1) {
                  modified = true

                  // indirectCall.parent.parent.removeBlocks(indirectCall.returnTarget)
                  val newCall = DirectCall(targets.head, indirectCall.returnTarget, indirectCall.label)
                  block.replaceJump(newCall)
                } else if (targets.size > 1) {
                  modified = true
                  val procedure = c.parent.data
                  val newBlocks = ArrayBuffer[Block]()
                  for (t <- targets) {
                    val assume = Assume(BinaryExpr(BVEQ, indirectCall.target, BitVecLiteral(t.address.get, 64)))
                    val newLabel: String = block.label + t.name
                    val directCall = DirectCall(t, indirectCall.returnTarget)
                    directCall.parent = indirectCall.parent

                    newBlocks.append(Block(newLabel, None, ArrayBuffer(assume), directCall))
                  }
                  procedure.addBlocks(newBlocks)
                  val newCall = GoTo(newBlocks, indirectCall.label)
                  block.replaceJump(newCall)
                }
              case LiftedBottom =>
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
            Logger.info(s"RESOLVED: Call to Global address ${globalAddress.name} rt statuesolved.")
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

    modified
  }

  def resolveIndirectCallsUsingPointsTo(
     cfg: ProgramCfg,
     pointsTos: Map[RegisterVariableWrapper, Set[RegisterVariableWrapper | MemoryRegion]],
     regionContents: Map[MemoryRegion, Set[BitVecLiteral | MemoryRegion]],
     IRProgram: Program
   ): Boolean = {
    var modified: Boolean = false
    val worklist = ListBuffer[CfgNode]()
    cfg.startNode.succIntra.union(cfg.startNode.succInter).foreach(node => worklist.addOne(node))

    val visited = mutable.Set[CfgNode]()
    while (worklist.nonEmpty) {
      val node = worklist.remove(0)
      if (!visited.contains(node)) {
        process(node)
        node.succIntra.union(node.succInter).foreach(node => worklist.addOne(node))
        visited.add(node)
      }
    }

    def searchRegion(region: MemoryRegion): mutable.Set[String] = {
      val result = mutable.Set[String]()
      region match {
        case stackRegion: StackRegion =>
          if (regionContents.contains(stackRegion)) {
            for (c <- regionContents(stackRegion)) {
              c match {
                case bitVecLiteral: BitVecLiteral => ???
                case memoryRegion: MemoryRegion =>
                  result.addAll(searchRegion(memoryRegion))
              }
            }
          }
          result
        case dataRegion: DataRegion =>
          if (!regionContents.contains(dataRegion) || regionContents(dataRegion).isEmpty) {
            result.add(dataRegion.regionIdentifier)
          } else {
            for (c <- regionContents(dataRegion)) {
              c match {
                case bitVecLiteral: BitVecLiteral => ???
                case memoryRegion: MemoryRegion =>
                  result.addAll(searchRegion(memoryRegion))
              }
            }
          }
          result
      }
    }

    def addFakeProcedure(name: String): Procedure = {
      val newProcedure = Procedure(name)
      IRProgram.procedures += newProcedure
      newProcedure
    }

    def resolveAddresses(variable: Variable): mutable.Set[String] = {
      val names = mutable.Set[String]()
      val variableWrapper = RegisterVariableWrapper(variable)
      pointsTos.get(variableWrapper) match {
        case Some(value) =>
          value.map {
            case v: RegisterVariableWrapper => names.addAll(resolveAddresses(v.variable))
            case m: MemoryRegion => names.addAll(searchRegion(m))
          }
          names
        case None => names
      }
    }

    def process(n: CfgNode): Unit = n match {
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
            val targetNames = resolveAddresses(indirectCall.target)
            Logger.debug(s"Points-To approximated call ${indirectCall.target} with $targetNames")
            Logger.debug(IRProgram.procedures)
            val targets: mutable.Set[Procedure] = targetNames.map(name => IRProgram.procedures.find(_.name == name).getOrElse(addFakeProcedure(name)))

            if (targets.size == 1) {
              modified = true

              // indirectCall.parent.parent.removeBlocks(indirectCall.returnTarget)
              val newCall = DirectCall(targets.head, indirectCall.returnTarget, indirectCall.label)
              block.replaceJump(newCall)
            } else if (targets.size > 1) {
              modified = true
              val procedure = c.parent.data
              val newBlocks = ArrayBuffer[Block]()
              // indirectCall.parent.parent.removeBlocks(indirectCall.returnTarget)
              for (t <- targets) {
                val assume = Assume(BinaryExpr(BVEQ, indirectCall.target, BitVecLiteral(t.address.get, 64)))
                val newLabel: String = block.label + t.name
                val directCall = DirectCall(t, indirectCall.returnTarget)
                directCall.parent = indirectCall.parent

                newBlocks.append(Block(newLabel, None, ArrayBuffer(assume), directCall))
              }
              procedure.addBlocks(newBlocks)
              val newCall = GoTo(newBlocks, indirectCall.label)
              block.replaceJump(newCall)
            }
          case _ =>
      case _ =>
    }

    modified
  }

  /** Cull unneccessary information that does not need to be included in the translation, and infer stack regions, and
    * add in modifies from the spec.
    */
  def prepareForTranslation(config: ILLoadingConfig, ctx: IRContext): Unit = {
    ctx.program.determineRelevantMemory(ctx.globalOffsets)

    Logger.info("[!] Stripping unreachable")
    val before = ctx.program.procedures.size
    ctx.program.stripUnreachableFunctions(config.procedureTrimDepth)
    Logger.info(
      s"[!] Removed ${before - ctx.program.procedures.size} functions (${ctx.program.procedures.size} remaining)"
    )

    val stackIdentification = StackSubstituter()
    stackIdentification.visitProgram(ctx.program)

    val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
    ctx.program.setModifies(specModifies)
  }

}

/** Methods relating to program static analysis.
  */
object StaticAnalysis {
  /** Run all static analysis passes on the provided IRProgram.
    */
  def analyse(
      ctx: IRContext,
      config: StaticAnalysisConfig,
      iteration: Int
  ): StaticAnalysisContext = {
    val IRProgram: Program = ctx.program
    val externalFunctions: Set[ExternalFunction] = ctx.externalFunctions
    val globals: Set[SpecGlobal] = ctx.globals
    val globalOffsets: Map[BigInt, BigInt] = ctx.globalOffsets

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


    // reducible loops
    val detector = LoopDetector(IRProgram)
    val foundLoops = detector.identify_loops()
    foundLoops.foreach(l => Logger.info(s"Loop found: ${l.name}"))

    val transformer = LoopTransform(foundLoops)
    val newLoops = transformer.llvm_transform()
    newLoops.foreach(l => Logger.info(s"Loop found: ${l.name}"))

    config.analysisDotPath.foreach { s =>
      val newCFG = ProgramCfgFactory().fromIR(IRProgram)
      writeToFile(newCFG.toDot(x => x.toString, Output.dotIder), s"${s}_resolvedCFG-reducible.dot")
      writeToFile(dotBlockGraph(IRProgram, IRProgram.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"${s}_blockgraph-after-reduce-$iteration.dot")
    }

    val mergedSubroutines = subroutines ++ externalAddresses
    SSAForm(IRProgram).applySSA()

    val cfg = ProgramCfgFactory().fromIR(IRProgram)

    val domain = computeDomain(IntraProcIRCursor, IRProgram.procedures)

    Logger.info("[!] Running ANR")
    val ANRSolver = ANRAnalysisSolver(cfg)
    val ANRResult = ANRSolver.analyze()

    config.analysisDotPath.foreach(s => writeToFile(cfg.toDot(Output.labeler(ANRResult, true), Output.dotIder), s"${s}_ANR$iteration.dot"))
    config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(cfg, ANRResult, iteration), s"${s}_ANR$iteration.txt"))

    Logger.info("[!] Running RNA")
    val RNASolver = RNAAnalysisSolver(cfg)
    val RNAResult = RNASolver.analyze()

    config.analysisDotPath.foreach(s => writeToFile(cfg.toDot(Output.labeler(RNAResult, true), Output.dotIder), s"${s}_RNA$iteration.dot"))
    config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(cfg, RNAResult, iteration), s"${s}_RNA$iteration.txt"))

    Logger.info("[!] Running Constant Propagation")
    val constPropSolver = ConstantPropagationSolver(cfg)
    val constPropResult: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]] = constPropSolver.analyze()

    val ilcpsolver = IRSimpleValueAnalysis.Solver(IRProgram)
    val newCPResult: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] = ilcpsolver.analyze()

    config.analysisResultsPath.foreach(s =>
      writeToFile(printAnalysisResults(IRProgram, newCPResult), s"${s}_new_ir_constprop$iteration.txt")
    )
    config.analysisDotPath.foreach(s =>
      writeToFile(cfg.toDot(Output.labeler(constPropResult, true), Output.dotIder), s"${s}_constprop$iteration.dot")
    )
    config.analysisResultsPath.foreach(s =>
      writeToFile(printAnalysisResults(IRProgram, cfg, constPropResult), s"${s}_constprop$iteration.txt")
    )
    config.analysisDotPath.foreach(s =>
      writeToFile(cfg.toDot(Output.labeler(constPropResult, true), Output.dotIder), s"${s}_constprop$iteration.dot")
    )

    config.analysisDotPath.foreach(f => {
      val dumpdomain = computeDomain[CFGPosition, CFGPosition](InterProcIRCursor, IRProgram.procedures)
      writeToFile(toDot(dumpdomain, InterProcIRCursor, Map.empty), s"${f}_new_ir_intercfg$iteration.dot")
    })

    config.analysisResultsPath.foreach(s =>
      writeToFile(printAnalysisResults(IRProgram, cfg, constPropResult), s"${s}_constprop$iteration.txt")
    )

    Logger.info("[!] Running RegToMemAnalysisSolver")
    val regionAccessesAnalysisSolver = RegionAccessesAnalysisSolver(cfg, constPropResult)
    val regionAccessesAnalysisResults = regionAccessesAnalysisSolver.analyze()

    config.analysisDotPath.foreach(s => writeToFile(cfg.toDot(Output.labeler(regionAccessesAnalysisResults, true), Output.dotIder), s"${s}_RegTo$iteration.dot"))
    config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(cfg, regionAccessesAnalysisResults, iteration), s"${s}_RegTo$iteration.txt"))

    Logger.info("[!] Running Constant Propagation with SSA")
    val constPropSolverWithSSA = ConstantPropagationSolverWithSSA(cfg)
    val constPropResultWithSSA = constPropSolverWithSSA.analyze()

    config.analysisDotPath.foreach(s => writeToFile(cfg.toDot(Output.labeler(constPropResultWithSSA, true), Output.dotIder), s"${s}_constpropWithSSA$iteration.dot"))
    config.analysisResultsPath.foreach(s => writeToFile(printAnalysisResults(cfg, constPropResultWithSSA, iteration), s"${s}_constpropWithSSA$iteration.txt"))

    Logger.info("[!] Running MRA")
    val mraSolver = MemoryRegionAnalysisSolver(cfg, globalAddresses, globalOffsets, mergedSubroutines, constPropResult, ANRResult, RNAResult, regionAccessesAnalysisResults)
    val mraResult = mraSolver.analyze()

    config.analysisDotPath.foreach(s => {

      writeToFile(cfg.toDot(Output.labeler(mraResult, true), Output.dotIder), s"${s}_mra$iteration.dot")
      writeToFile(dotCallGraph(IRProgram), s"${s}_callgraph$iteration.dot")
      writeToFile(
        dotBlockGraph(IRProgram, IRProgram.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap),
        s"${s}_blockgraph$iteration.dot"
      )

      writeToFile(
        toDot(IRProgram, IRProgram.filter(_.isInstanceOf[Command]).map(b => b -> newCPResult(b).toString).toMap),
        s"${s}_new_ir_constprop$iteration.dot"
      )

    })
    config.analysisResultsPath.foreach(s =>
      writeToFile(printAnalysisResults(IRProgram, cfg, mraResult), s"${s}_mra$iteration.txt")
    )

    Logger.info("[!] Running MMM")
    val mmm = MemoryModelMap()
    mmm.convertMemoryRegions(mraResult, mergedSubroutines, globalOffsets, mraSolver.procedureToSharedRegions)

    Logger.info("[!] Running Steensgaard")
    val steensgaardSolver = InterprocSteensgaardAnalysis(cfg, constPropResultWithSSA, regionAccessesAnalysisResults, mmm, globalOffsets)
    steensgaardSolver.analyze()
    val steensgaardResults = steensgaardSolver.pointsTo()
    val memoryRegionContents = steensgaardSolver.getMemoryRegionContents
    mmm.logRegions(steensgaardSolver.getMemoryRegionContents)

    Logger.info("[!] Running VSA")
    val vsaSolver =
      ValueSetAnalysisSolver(cfg, globalAddresses, externalAddresses, globalOffsets, subroutines, mmm, constPropResult)
    val vsaResult: Map[CfgNode, LiftedElement[Map[Variable | MemoryRegion, Set[Value]]]] = vsaSolver.analyze()

    config.analysisDotPath.foreach(s =>
      writeToFile(cfg.toDot(Output.labeler(vsaResult, true), Output.dotIder), s"${s}_vsa$iteration.dot")
    )
    config.analysisResultsPath.foreach(s =>
      writeToFile(printAnalysisResults(IRProgram, cfg, vsaResult), s"${s}_vsa$iteration.txt")
    )

    Logger.info("[!] Running Interprocedural Live Variables Analysis")
    val interLiveVarsResults = InterLiveVarsAnalysis(IRProgram).analyze()

    Logger.info("[!] Running Parameter Analysis")
    val paramResults = ParamAnalysis(IRProgram).analyze()

    StaticAnalysisContext(
      cfg = cfg,
      constPropResult = constPropResult,
      IRconstPropResult = newCPResult,
      memoryRegionResult = mraResult,
      vsaResult = vsaResult,
      interLiveVarsResults = interLiveVarsResults,
      paramResults = paramResults,
      steensgaardResults = steensgaardResults,
      mmmResults = mmm,
      memoryRegionContents = memoryRegionContents
    )
  }

  /** Converts MapLattice of CfgNodes to a MapLattice from IRPosition.
    * @param cfg
    *   The CFG
    * @param result
    *   The analysis result MapLattice
    * @tparam T
    *   The anlaysis result type.
    * @return
    *   The new map analysis result.
    */
  def convertAnalysisResults[T](cfg: ProgramCfg, result: Map[CfgNode, T]): Map[CFGPosition, T] = {
    val results = mutable.HashMap[CFGPosition, T]()
    result.foreach((node, res) =>
      node match {
        case s: CfgStatementNode     => results.addOne(s.data -> res)
        case s: CfgFunctionEntryNode => results.addOne(s.data -> res)
        case s: CfgJumpNode          => results.addOne(s.data -> res)
        case s: CfgCommandNode       => results.addOne(s.data -> res)
        case _                       => ()
      }
    )

    results.toMap
  }

  def printAnalysisResults[T](program: Program, cfg: ProgramCfg, result: Map[CfgNode, T]): String = {
    printAnalysisResults(program, convertAnalysisResults(cfg, result))
  }

  def printAnalysisResults(prog: Program, result: Map[CFGPosition, _]): String = {
    val results = mutable.ArrayBuffer[String]()
    val toVisit = mutable.Stack[CFGPosition]()
    val visited = mutable.HashSet[CFGPosition]()
    toVisit.pushAll(prog.procedures)

    while (toVisit.nonEmpty) {
      val next = toVisit.pop()
      visited.add(next)
      toVisit.pushAll(
        IntraProcBlockIRCursor
          .succ(next)
          .diff(visited.collect[Block] { case b: Block =>
            b
          })
      )

      def contentStr(b: CFGPosition) = {
        if result.contains(b) then "\n        :: " + result(b)
        else ""
      }

      val t = next match
        case p: Procedure => s"\nProcedure ${p.name}"
        case b: Block =>
          Seq(
            s"  Block ${b.label}${contentStr(b)}",
            b.statements
              .map(s => {
                "    " + s.toString + contentStr(s)
              })
              .mkString("\n"),
            "    " + b.jump.toString + contentStr(b.jump)
          ).mkString("\n")
        case s: Statement => s"    Statement $s${contentStr(s)}"
        case s: Jump      => s"  Jump $s${contentStr(s)}"
      results.addOne(t)
    }

    results.mkString(System.lineSeparator())
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
              c match {
                case _: CfgStatementNode => s.append("    ")
                case _                   => ()
              }
              printNode(c)
              previousBlock = c.block.label
              isEntryNode = false
            case c: CfgFunctionEntryNode =>
              printNode(c)
              isEntryNode = true
            case c: CfgCallNoReturnNode =>
              s.append(System.lineSeparator())
              isEntryNode = false
            case _ => isEntryNode = false
          }
          val successors = next.succIntra
          if (successors.size > 1) {
            val successorsCmd = successors.collect { case c: CfgCommandNode => c }.toSeq.sortBy(_.data.toString)
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
              case _                                                                       =>
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

}

object RunUtils {

  def run(q: BASILConfig): Unit = {
    val result = loadAndTranslate(q)

    Logger.info("[!] Writing file...")
    val wr = BufferedWriter(FileWriter(q.outputPrefix))
    result.boogie.writeToString(wr)
    wr.close()
  }

  def loadAndTranslate(q: BASILConfig): BASILResult = {
    Logger.info("[!] Loading Program")
    val ctx = IRLoading.load(q.loading)

    IRTransform.doCleanup(ctx)

    q.loading.dumpIL.foreach(s => writeToFile(serialiseIL(ctx.program), s"$s-before-analysis.il"))
    val analysis = q.staticAnalysis.map(conf => staticAnalysis(conf, ctx))
    q.loading.dumpIL.foreach(s => writeToFile(serialiseIL(ctx.program), s"$s-after-analysis.il"))

    if (q.runInterpret) {
      val interpreter = Interpreter()
      interpreter.interpret(ctx.program)
    }

    IRTransform.prepareForTranslation(q.loading, ctx)

    Logger.info("[!] Translating to Boogie")
    val boogieTranslator = IRToBoogie(ctx.program, ctx.specification)
    val boogieProgram = boogieTranslator.translate(q.boogieTranslation)

    BASILResult(ctx, analysis, boogieProgram)
  }

  /** Use static analysis to resolve indirect calls and replace them in the IR until fixed point.
   */
  def staticAnalysis(config: StaticAnalysisConfig, ctx: IRContext): StaticAnalysisContext = {
    var iteration = 1
    var modified: Boolean = true
    val analysisResult = mutable.ArrayBuffer[StaticAnalysisContext]()
    while (modified) {
      Logger.info("[!] Running Static Analysis")
      val result = StaticAnalysis.analyse(ctx, config, iteration)
      analysisResult.append(result)
      Logger.info("[!] Replacing Indirect Calls")
      modified = IRTransform.resolveIndirectCallsUsingPointsTo(result.cfg,
        result.steensgaardResults,
        result.memoryRegionContents,
        ctx.program
      )
      if (modified) {
        iteration += 1
        Logger.info(s"[!] Analysing again (iter $iteration)")
      }
    }

    config.analysisDotPath.foreach { s =>
      val newCFG = analysisResult.last.cfg
      writeToFile(newCFG.toDot(x => x.toString, Output.dotIder), s"${s}_resolvedCFG.dot")
    }

    Logger.info(s"[!] Finished indirect call resolution after $iteration iterations")
    analysisResult.last
  }
}

def writeToFile(content: String, fileName: String): Unit = {
  val outFile = File(fileName)
  val pw = PrintWriter(outFile, "UTF-8")
  pw.write(content)
  pw.close()
}
