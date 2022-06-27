package translating

import astnodes._
import BilParser.SymsParser._
import scala.jdk.CollectionConverters._

object ElfLoader {
  def visitSyms(ctx: SymsContext): (Set[ExternalFunction], Set[GlobalVariable]) = {
    val externalFunctions = ctx.relocationTable.asScala.flatMap(r => visitRelocationTable(r)).toSet
    val globalVariables = ctx.symbolTable.asScala.flatMap(s => visitSymbolTable(s)).toSet
    (externalFunctions, globalVariables)
  }

  def visitRelocationTable(ctx: RelocationTableContext): Set[ExternalFunction] = {
    if (ctx.relocationTableHeader.tableName.STRING.getText == ".rela.plt") {
      val rows = ctx.relocationTableRow.asScala
      rows.map(r => visitRelocationTableRow(r)).toSet
    } else {
      Set()
    }
  }

  def visitRelocationTableRow(ctx: RelocationTableRowContext): ExternalFunction = {
    ExternalFunction(ctx.name.getText.stripSuffix("@GLIBC_2.17"), BigInt(ctx.offset.getText, 16))
  }

  def visitSymbolTable(ctx: SymbolTableContext): Set[GlobalVariable] = {
    if (ctx.symbolTableHeader.tableName.STRING.getText == ".symtab") {
      val rows = ctx.symbolTableRow.asScala
      rows.flatMap(r => visitSymbolTableRow(r)).toSet
    } else {
      Set()
    }
  }

  def visitSymbolTableRow(ctx: SymbolTableRowContext): Option[GlobalVariable] = {
    if (ctx.entrytype.getText == "OBJECT" && ctx.bind.getText == "GLOBAL" && ctx.vis.getText == "DEFAULT") {
      Some(GlobalVariable(ctx.name.getText, ctx.size.getText.toInt * 8, BigInt(ctx.value.getText, 16)))
    } else {
      None
    }
  }

}
