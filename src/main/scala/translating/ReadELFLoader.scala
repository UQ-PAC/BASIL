package translating

import Parsers.ReadELFParser.*
import specification.*
import util.ILLoadingConfig

import scala.jdk.CollectionConverters.*

object ReadELFLoader {
  // TODO: load NOTYPE symbols, so we can get _bss_start ... _bss_end to zero-init in interpreter, as well as _end to find bottom of heap
  def visitSyms(ctx: SymsContext, config: ILLoadingConfig): (Set[ExternalFunction], Set[SpecGlobal], Map[BigInt, BigInt], Int) = {
    val externalFunctions = ctx.relocationTable.asScala.flatMap(r => visitRelocationTableExtFunc(r)).toSet
    val relocationOffsets = ctx.relocationTable.asScala.flatMap(r => visitRelocationTableOffsets(r)).toMap
    val globalVariables = ctx.symbolTable.asScala.flatMap(s => visitSymbolTable(s)).toSet
    val mainAddress = ctx.symbolTable.asScala.flatMap(s => getFunctionAddress(s, config.mainProcedureName))
    if (mainAddress.isEmpty) {
      throw Exception(s"no ${config.mainProcedureName} function in symbol table")
    }
    (externalFunctions, globalVariables, relocationOffsets, mainAddress.head)
  }

  def visitRelocationTableExtFunc(ctx: RelocationTableContext): Set[ExternalFunction] = {
    val sectionName = ctx.relocationTableHeader.tableName.STRING.getText
    if (sectionName == ".rela.plt" || sectionName == ".rela.dyn") {
      val rows = ctx.relocationTableRow.asScala
      rows.filter(r => r.name != null).map(r => visitRelocationTableRowExtFunc(r)).toSet
    } else {
      Set()
    }
  }

  def visitRelocationTableRowExtFunc(ctx: RelocationTableRowContext): ExternalFunction = {
    ExternalFunction(ctx.name.getText.stripSuffix("@GLIBC_2.17"), hexToBigInt(ctx.offset.getText))
  }

  def visitRelocationTableOffsets(ctx: RelocationTableContext): Map[BigInt, BigInt] = {
    if (ctx.relocationTableHeader.tableName.STRING.getText == ".rela.dyn") {
      val rows = ctx.relocationTableRow.asScala
      rows.flatMap(r => visitRelocationTableRowOffset(r)).toMap
    } else {
      Map()
    }
  }

  def visitRelocationTableRowOffset(ctx: RelocationTableRowContext): Option[(BigInt, BigInt)] = {
    if (ctx.relocType.getText == "R_AARCH64_RELATIVE") {
      Some((hexToBigInt(ctx.offset.getText), hexToBigInt(ctx.gotName.getText)))
    } else {
      None
    }
  }

  def visitSymbolTable(ctx: SymbolTableContext): Set[SpecGlobal] = {
    if (ctx.symbolTableHeader.tableName.STRING.getText == ".symtab") {
      val rows = ctx.symbolTableRow.asScala
      rows.flatMap(r => visitSymbolTableRow(r)).toSet
    } else {
      Set()
    }
  }

  def getFunctionAddress(ctx: SymsContext, functionName: String): Option[Int] = {
    ctx.symbolTable.asScala.flatMap(s => getFunctionAddress(s, functionName)).headOption
  }

  private def getFunctionAddress(ctx: SymbolTableContext, functionName: String): Option[Int] = {
    if (ctx.symbolTableHeader.tableName.STRING.getText == ".symtab") {
      val rows = ctx.symbolTableRow.asScala
      val mainAddress = rows.collectFirst {
        case r if r.entrytype.getText == "FUNC" && r.bind.getText == "GLOBAL" && r.name.getText == functionName =>
          Integer.parseInt(r.value.getText, 16)
      }
      mainAddress
    } else {
      None
    }
  }

  def visitSymbolTableRow(ctx: SymbolTableRowContext): Option[SpecGlobal] = {
    if (ctx.entrytype.getText == "OBJECT" && ctx.bind.getText == "GLOBAL" && ctx.vis.getText == "DEFAULT") {
      val name = ctx.name.getText
      if (name.forall(allowedChars.contains)) {
        Some(SpecGlobal(name, ctx.size.getText.toInt * 8, None, hexToBigInt(ctx.value.getText)))
      } else {
        None
      }
    } else {
      None
    }
  }

  def hexToBigInt(hex: String): BigInt = BigInt(hex, 16)

  val allowedChars: Set[Char] = ('A' to 'Z').toSet ++ ('a' to 'z') ++ ('0' to '9') + '_'

}
