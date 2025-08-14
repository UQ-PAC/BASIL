package ir

import Parsers.*
import analysis.Interval as _
import bap.*
import boogie.*
import com.grammatech.gtirb.proto.IR.IR
import gtirb.*
import ir.*
import org.antlr.v4.runtime.{BailErrorStrategy, CharStreams, CommonTokenStream}
import specification.*
import translating.*
import util.{ILLoadingConfig, Logger}

import java.io.FileInputStream
import scala.jdk.CollectionConverters.*

enum FrontendMode {
  case Bap
  case Gtirb
  case Basil
}

/** Stores the IR Program loaded from the binary and ELF tables, which is modified during analysis and program
  * transformation.
  */
case class IRContext(
  symbols: List[ELFSymbol],
  externalFunctions: Set[ExternalFunction],
  globals: Set[SpecGlobal],
  funcEntries: Set[FuncEntry],
  globalOffsets: Map[BigInt, BigInt],
  specification: Specification,
  program: Program // internally mutable
)

object IRLoading {

  /** Create a context from just an IR program.
    */
  def load(p: Program): IRContext = {
    IRContext(
      List.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      Map.empty,
      IRLoading.loadSpecification(None, p, Set.empty),
      p
    )
  }

  /** Load a program from files using the provided configuration.
    */
  def load(q: ILLoadingConfig): IRContext = {

    val mode = q.frontendMode
    if (q.inputFile.endsWith(".gtirb") && !q.gtirbLiftOffline) {
      throw IllegalArgumentException(".gtirb input requires --lifter")
    }

    val (mainAddress, makeContext) = q.relfFile match {
      case Some(relf) => {

        // allow loading elf from inputFile if using GTIRB mode.
        val relfData = if (relf == q.inputFile && mode == FrontendMode.Gtirb) {
          Logger.info("[!] Using ELF data from GTIRB: " + q.inputFile)
          IRLoading.loadGTIRBReadELF(q)
        } else {
          Logger.info("[!] Using ELF data from relf: " + relf)
          IRLoading.loadReadELF(relf, q)
        }

        val ReadELFData(symbols, externalFunctions, globals, funcEntries, globalOffsets, mainAddress) = relfData

        def continuation(ctx: IRContext) =
          val specification = IRLoading.loadSpecification(q.specFile, ctx.program, globals)
          IRContext(symbols, externalFunctions, globals, funcEntries, globalOffsets, specification, ctx.program)

        (Some(mainAddress), continuation)
      }
      case None if mode == FrontendMode.Gtirb => {
        Logger.warn(
          "RELF input not provided, this is not recommended! To provide a RELF input, specify --relf or --gts-relf."
        )
        (None, (x: IRContext) => x)
      }
      case None => {
        (None, (x: IRContext) => x)
      }
    }

    val program: IRContext = (mode, mainAddress) match {
      case (FrontendMode.Gtirb, _) =>
        IRLoading.load(loadGTIRB(q.inputFile, mainAddress, q.gtirbLiftOffline, Some(q.mainProcedureName)))
      case (FrontendMode.Basil, _) => ir.parsing.ParseBasilIL.loadILFile(q.inputFile)
      case (FrontendMode.Bap, None) => throw Exception("relf is required when using BAP input")
      case (FrontendMode.Bap, Some(mainAddress)) => {
        val bapProgram = loadBAP(q.inputFile)
        IRLoading.load(BAPToIR(bapProgram, mainAddress).translate)
      }
    }

    val ctx = makeContext(program)
    mode match {
      case FrontendMode.Basil => {
        ctx.program.procedures.foreach(_.updateBlockSuffix())
        Logger.info("[!] Disabling PC tracking transforms due to IL input")
      }
      case _ => {
        ir.transforms.PCTracking.applyPCTracking(q.pcTracking, ctx.program)
        ctx.program.procedures.foreach(_.normaliseBlockNames())
        ir.transforms.clearParams(ctx.program)
      }
    }
    ctx
  }

  def loadBAP(fileName: String): BAPProgram = {
    val ADTLexer = BAP_ADTLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(ADTLexer)
    val parser = BAP_ADTParser(tokens)

    parser.setBuildParseTree(true)

    BAPLoader().visitProject(parser.project())
  }

  def skipGTIRBMagic(fileName: String): FileInputStream = {
    val fIn = FileInputStream(fileName)
    (0 to 7).map(_ => fIn.read()).toList match {
      case List('G', 'T', 'I', 'R', 'B', _, _, _) => fIn
      case _ => {
        fIn.close()
        FileInputStream(fileName)
      }
    }
  }

  def loadGTIRB(
    fileName: String,
    mainAddress: Option[BigInt],
    gtirbLiftOffline: Boolean,
    mainName: Option[String] = None
  ): Program = {
    val fIn = skipGTIRBMagic(fileName)
    val ir = IR.parseFrom(fIn)
    val mods = ir.modules
    val cfg = ir.cfg.get

    val lifter =
      if (gtirbLiftOffline) then OfflineLifterInsnLoader(mods)
      else {
        ParserMapInsnLoader(mods)
      }

    val GTIRBConverter = GTIRBToIR(mods, lifter, cfg, mainAddress, mainName)
    GTIRBConverter.createIR()
  }

  /** Loads ELF data from the GTIRB input file. */
  def loadGTIRBReadELF(config: ILLoadingConfig): ReadELFData = {
    val ir = IR.parseFrom(FileInputStream(config.inputFile))
    if (ir.modules.length != 1) {
      Logger.warn(s"GTIRB file ${config.inputFile} unexpectedly has ${ir.modules.length} modules")
    }

    val gtirb = GTIRBResolver(ir.modules.head)
    val gtirbRelfLoader = GTIRBReadELF(gtirb)
    gtirbRelfLoader.getReadELFData(config.mainProcedureName)
  }

  /**
   * Loads ELF data from *both* .relf and .gts (if using GTIRB input). If both
   * sources load successfully, compares them and warns on any differences.
   */
  def loadReadELFWithGTIRB(fileName: String, config: ILLoadingConfig): (ReadELFData, Option[ReadELFData]) = {
    val lexer = ReadELFLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(lexer)
    val parser = ReadELFParser(tokens)
    parser.setErrorHandler(BailErrorStrategy())
    parser.setBuildParseTree(true)

    val relf = ReadELFLoader.visitSyms(parser.syms(), config)

    val gtirbRelf = if (config.inputFile.endsWith(".gts") || config.inputFile.endsWith(".gtirb")) {
      val gtirbRelf = loadGTIRBReadELF(config)
      GTIRBReadELF.checkReadELFCompatibility(gtirbRelf, relf)
      Some(gtirbRelf)
    } else {
      None
    }

    (relf, gtirbRelf)
  }

  /**
   * Loads ELF data from .relf.
   */
  def loadReadELF(fileName: String, config: ILLoadingConfig) =
    loadReadELFWithGTIRB(fileName, config)._1

  def emptySpecification(globals: Set[SpecGlobal]) =
    Specification(Set(), globals, Map(), List(), List(), List(), Set())

  def loadSpecification(filename: Option[String], program: Program, globals: Set[SpecGlobal]): Specification = {
    filename match {
      case Some(s) =>
        val specLexer = SpecificationsLexer(CharStreams.fromFileName(s))
        val specTokens = CommonTokenStream(specLexer)
        val specParser = SpecificationsParser(specTokens)
        specParser.setBuildParseTree(true)
        val specLoader = SpecificationLoader(globals, program)
        specLoader.visitSpecification(specParser.specification())
      case None => emptySpecification(globals)
    }
  }
}
