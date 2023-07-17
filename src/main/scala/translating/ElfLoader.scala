package translating

import BilParser.SymsParser._
import specification._
import scala.jdk.CollectionConverters._

object ElfLoader {
  def visitSyms(ctx: SymsContext): (Set[ExternalFunction], Set[SpecGlobal], Map[BigInt, BigInt], Int) = {
    val externalFunctions = ctx.relocationTable.asScala.flatMap(r => visitRelocationTableExtFunc(r)).toSet
    val relocationOffsets = ctx.relocationTable.asScala.flatMap(r => visitRelocationTableOffsets(r)).toMap
    val globalVariables = ctx.symbolTable.asScala.flatMap(s => visitSymbolTable(s)).toSet
    val mainAddress = ctx.symbolTable.asScala.flatMap(s => getMainAddress(s))
    if (mainAddress.isEmpty) {
      throw Exception("no main function in symbol table")
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

  def getMainAddress(ctx: SymbolTableContext): Option[Int] = {
    if (ctx.symbolTableHeader.tableName.STRING.getText == ".symtab") {
      val rows = ctx.symbolTableRow.asScala
      val mainAddress = rows.collectFirst {
        case r if r.entrytype.getText == "FUNC" && r.bind.getText == "GLOBAL" && r.name.getText == "main" =>
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
