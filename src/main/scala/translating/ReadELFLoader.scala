package translating

import Parsers.ReadELFParser.*
import boogie.*
import ir.dsl.given
import specification.*
import util.{ILLoadingConfig, Logger}

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.jdk.CollectionConverters.*

/** https://refspecs.linuxfoundation.org/elf/elf.pdf
  */

enum ELFSymType derives ir.dsl.ToScala:
  case NOTYPE /* absolute symbol or similar */
  case SECTION /* memory section */
  case FILE
  case OBJECT
  case FUNC /* code function */
  case TLS /* ??? */

enum ELFBind derives ir.dsl.ToScala:
  case LOCAL /* local to the translation unit */
  case GLOBAL /* global to the program */
  case WEAK /* multiple versions of symbol may be exposed to the linker, and the last definition is used. */

enum ELFVis derives ir.dsl.ToScala:
  case HIDDEN
  case DEFAULT
  case PROTECTED

enum ELFNDX derives ir.dsl.ToScala:
  case Section(num: Int) /* Section containing the symbol */
  case UND /* Undefined */
  case ABS /* Absolute, unaffected by relocation */

case class ELFSymbol(
  num: Int, /* symbol number */
  value: BigInt, /* symbol address */
  size: Int, /* symbol size (bytes) */
  etype: ELFSymType,
  bind: ELFBind,
  vis: ELFVis,
  ndx: ELFNDX, /* The section containing the symbol */
  name: String
) derives ir.dsl.ToScala

case class ReadELFData(
  symbolTable: List[ELFSymbol],
  externalFunctions: Set[ExternalFunction],
  globalVariables: Set[SpecGlobal],
  functionEntries: Set[FuncEntry],
  relocationOffsets: Map[BigInt, BigInt],
  mainAddress: BigInt
) derives ir.dsl.ToScala {

  def sorted = ReadELFData(
    symbolTable,
    SortedSet.from(externalFunctions)(Ordering.by(Tuple.fromProductTyped(_))),
    SortedSet.from(globalVariables)(Ordering.by(Tuple.fromProductTyped(_))),
    SortedSet.from(functionEntries)(Ordering.by(Tuple.fromProductTyped(_))),
    SortedMap.from(relocationOffsets),
    mainAddress
  )

}

object ReadELFLoader {
  def visitSyms(ctx: SymsContext, config: ILLoadingConfig): ReadELFData = {
    val externalFunctions = ctx.relocationTable.asScala
      .filter(_.relocationTableHeader != null)
      .flatMap(r => visitRelocationTableExtFunc(r))
      .toSet
    val relocationOffsets = ctx.relocationTable.asScala
      .filter(_.relocationTableHeader != null)
      .flatMap(r => visitRelocationTableOffsets(r))
      .toMap
    val mainAddress = ctx.symbolTable.asScala.flatMap(s => getFunctionAddress(s, config.mainProcedureName))

    val symbolTable = ctx.symbolTable.asScala.flatMap(s => visitSymbolTable(s)).toList
    val globalVariables = symbolTable.collect {
      case ELFSymbol(_, value, size, ELFSymType.OBJECT, ELFBind.GLOBAL | ELFBind.LOCAL, ELFVis.DEFAULT, ndx, name)
          if ndx != ELFNDX.UND =>
        SpecGlobal(name, size * 8, None, value)
    }.toSet

    val functionEntries = symbolTable.collect {
      case ELFSymbol(_, value, size, ELFSymType.FUNC, ELFBind.GLOBAL, ELFVis.DEFAULT, ndx, name) if ndx != ELFNDX.UND =>
        FuncEntry(name, size * 8, value)
    }.toSet

    if (mainAddress.isEmpty) {
      throw Exception(s"no ${config.mainProcedureName} function in symbol table")
    }
    ReadELFData(symbolTable, externalFunctions, globalVariables, functionEntries, relocationOffsets, mainAddress.head)
  }

  def visitRelocationTableExtFunc(ctx: RelocationTableContext): Set[ExternalFunction] = {
    if (ctx.relocationTableHeader == null) {
      Set()
    } else {
      val sectionName = ctx.relocationTableHeader.tableName.STRING.getText
      if (sectionName == ".rela.plt" || sectionName == ".rela.dyn") {
        val rows = ctx.relocationTableRow.asScala
        rows.filter(r => r.name != null).map(r => visitRelocationTableRowExtFunc(r)).toSet
      } else {
        Set()
      }
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

  def visitSymbolTable(ctx: SymbolTableContext): List[ELFSymbol] = {
    if (ctx.symbolTableHeader.tableName.STRING.getText == ".symtab") {
      ctx.symbolTableRow.asScala.map(getSymbolTableRow).toList
    } else {
      List()
    }
  }

  def getFunctionAddress(ctx: SymsContext, functionName: String): Option[BigInt] = {
    ctx.symbolTable.asScala.flatMap(s => getFunctionAddress(s, functionName)).headOption
  }

  private def getFunctionAddress(ctx: SymbolTableContext, functionName: String): Option[BigInt] = {
    if (ctx.symbolTableHeader.tableName.STRING.getText == ".symtab") {
      val rows = ctx.symbolTableRow.asScala
      val mainAddress = rows.collect {
        case r if r.entrytype.getText == "FUNC" && r.name.getText == functionName =>
          hexToBigInt(r.value.getText)
      }
      if (mainAddress.size > 1) {
        Logger.warn(s"Multiple procedures with name $functionName")
      }
      mainAddress.headOption
    } else {
      None
    }
  }

  def getSymbolTableRow(ctx: SymbolTableRowContext): ELFSymbol = {
    val bind = ELFBind.valueOf(ctx.bind.getText)
    val etype = ELFSymType.valueOf(ctx.entrytype.getText)
    val size = ctx.size.getText.toInt
    val name = ctx.name match {
      case null => ""
      case x => x.getText
    }
    val value = BigInt(ctx.value.getText, 16)
    val num = ctx.num.getText.toInt
    val vis = ELFVis.valueOf(ctx.vis.getText)

    val ndx = (ctx.ndx.getText match {
      case "ABS" => ELFNDX.ABS
      case "UND" => ELFNDX.UND
      case o => ELFNDX.Section(o.toInt)
    })

    ELFSymbol(num, value, size, etype, bind, vis, ndx, name)
  }

  def hexToBigInt(hex: String): BigInt = BigInt(hex, 16)

  val allowedChars: Set[Char] = ('A' to 'Z').toSet ++ ('a' to 'z') ++ ('0' to '9') + '_'

}
