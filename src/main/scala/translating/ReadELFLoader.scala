package translating

import Parsers.ReadELFParser.*
import specification.*
import util.ILLoadingConfig

import scala.jdk.CollectionConverters.*

object ReadELFLoader {
  def visitSyms(ctx: SymsContext, config: ILLoadingConfig): (Set[ExternalFunction], Set[SpecGlobal], Set[FuncEntry], Map[BigInt, BigInt], Int) = {
    val externalFunctions = ctx.relocationTable.asScala.flatMap(r => visitRelocationTableExtFunc(r)).toSet
    val relocationOffsets = ctx.relocationTable.asScala.flatMap(r => visitRelocationTableOffsets(r)).toMap
    val symbolTableEntries = ctx.symbolTable.asScala.flatMap(s => visitSymbolTable(s)).toSet
    val globalVariables: Set[SpecGlobal] = symbolTableEntries.filter(_.isInstanceOf[SpecGlobal]).map(_.asInstanceOf[SpecGlobal])
    val functionEntries: Set[FuncEntry] = symbolTableEntries.filter(_.isInstanceOf[FuncEntry]).map(_.asInstanceOf[FuncEntry])
    val mainAddress = ctx.symbolTable.asScala.flatMap(s => getFunctionAddress(s, config.mainProcedureName))
    if (mainAddress.isEmpty) {
      throw Exception(s"no ${config.mainProcedureName} function in symbol table")
    }
    (externalFunctions, globalVariables, functionEntries, relocationOffsets, mainAddress.head)
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

  def visitSymbolTable(ctx: SymbolTableContext): Set[SymbolTableEntry] = {
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

  def visitSymbolTableRow(ctx: SymbolTableRowContext): Option[SymbolTableEntry] = {
    if ((ctx.entrytype.getText == "OBJECT" || ctx.entrytype.getText == "FUNC") && ctx.bind.getText == "GLOBAL" && ctx.vis.getText == "DEFAULT") {
      val name = ctx.name.getText
      if (name.forall(allowedChars.contains)) {
        ctx.entrytype.getText match
          case "OBJECT" => Some(SpecGlobal(name, ctx.size.getText.toInt * 8, None, hexToBigInt(ctx.value.getText)))
          case "FUNC" => Some(FuncEntry(name, ctx.size.getText.toInt * 8, hexToBigInt(ctx.value.getText)))
          case _ => None
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
