package ir.parsing

import util.{LogLevel, PerformanceTimer}

import java.io.{FileReader, Reader, StringReader}

object ParseBasilIL {

  /**
   * Combines the parsed declarations and the parsed DSL program into a [[util.IRContext]],
   * including resolving the DSL program into a Basil IR program.
   */
  def makeBasilIRContext(decls: Declarations, prog: ir.dsl.EventuallyProgram) = {
    ir.IRContext(
      List(),
      decls.symtab.externalFunctions,
      decls.symtab.globals,
      decls.symtab.funcEntries,
      decls.symtab.globalOffsets,
      ir.IRLoading.emptySpecification(decls.symtab.globals),
      prog.resolve
    )
  }

  def loadILReader(reader: Reader) = {
    val timer = PerformanceTimer("ParseBasilIL", LogLevel.DEBUG)

    val lexer = new basil_ir.Yylex(reader);
    timer.checkPoint("lexed")
    val parser = new basil_ir.parser(lexer, lexer.getSymbolFactory());

    val ast = parser.pModule()
    timer.checkPoint("parsed")

    val vis0 = BasilEarlyBNFCVisitor[Unit]()
    val decls = ast.accept(vis0, ())
    timer.checkPoint("early visitor")
    // Logger.debug(decls)

    val vis = BasilMainBNFCVisitor[Unit](decls)
    val prog = ast.accept(vis, ())
    timer.checkPoint("main visitor")

    val result = makeBasilIRContext(decls, prog)
    timer.checkPoint("resolved and built ircontext")
    result
  }

  def loadILFile(filePath: String): ir.IRContext = {
    val reader = new FileReader(filePath)
    loadILReader(reader)
  }

  def loadILString(text: String): ir.IRContext = {
    val reader = new StringReader(text)
    loadILReader(reader)
  }

}
