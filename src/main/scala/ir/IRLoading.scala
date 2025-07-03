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

    val mode = if q.inputFile.endsWith(".gts") then {
      FrontendMode.Gtirb
    } else if q.inputFile.endsWith(".adt") then {
      FrontendMode.Bap
    } else if (q.inputFile.endsWith(".il")) {
      FrontendMode.Basil
    } else {
      throw Exception(s"input file name ${q.inputFile} must be an .adt or .gts file")
    }

    val (mainAddress, makeContext) = q.relfFile match {
      case Some(relf) => {
        // TODO: this tuple is large, should be a case class
        val (symbols, externalFunctions, globals, funcEntries, globalOffsets, mainAddress) =
          IRLoading.loadReadELF(relf, q)

        def continuation(ctx: IRContext) =
          val specification = IRLoading.loadSpecification(q.specFile, ctx.program, globals)
          IRContext(symbols, externalFunctions, globals, funcEntries, globalOffsets, specification, ctx.program)

        (Some(mainAddress), continuation)
      }
      case None if mode == FrontendMode.Gtirb => {
        Logger.warn("RELF not provided, recommended for GTIRB input")
        (None, (x: IRContext) => x)
      }
      case None => {
        (None, (x: IRContext) => x)
      }
    }

    val program: IRContext = (mode, mainAddress) match {
      case (FrontendMode.Gtirb, _) => IRLoading.load(loadGTIRB(q.inputFile, mainAddress, Some(q.mainProcedureName)))
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

  def loadGTIRB(fileName: String, mainAddress: Option[BigInt], mainName: Option[String] = None): Program = {
    val fIn = FileInputStream(fileName)
    val ir = IR.parseFrom(fIn)
    val mods = ir.modules
    val cfg = ir.cfg.get

    val semanticsJson = mods.map(_.auxData("ast").data.toStringUtf8)

    val semantics = semanticsJson.map(upickle.default.read[Map[String, List[InsnSemantics]]](_))

    val parserMap: Map[String, List[InsnSemantics]] = semantics.flatten.toMap

    val GTIRBConverter = GTIRBToIR(mods, parserMap, cfg, mainAddress, mainName)
    GTIRBConverter.createIR()
  }

  def loadReadELF(
    fileName: String,
    config: ILLoadingConfig
  ): (List[ELFSymbol], Set[ExternalFunction], Set[SpecGlobal], Set[FuncEntry], Map[BigInt, BigInt], BigInt) = {
    val lexer = ReadELFLexer(CharStreams.fromFileName(fileName))
    val tokens = CommonTokenStream(lexer)
    val parser = ReadELFParser(tokens)
    parser.setErrorHandler(BailErrorStrategy())
    parser.setBuildParseTree(true)
    ReadELFLoader.visitSyms(parser.syms(), config)
  }

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
