package translating

import Parsers.ReadELFParser.*
import specification.*
import util.ILLoadingConfig

import scala.jdk.CollectionConverters.*

/**
 * https://refspecs.linuxfoundation.org/elf/elf.pdf
 */

enum ELFSymType:
  case NOTYPE /* absolute symbol or similar */
  case SECTION /* memory section */
  case FILE
  case OBJECT
  case FUNC /* code function */


enum ELFBind:
  case LOCAL  /* local to the translation unit */
  case GLOBAL /* global to the program */
  case WEAK  /* multiple versions of symbol may be exposed to the linker, and the last definition is used. */

enum ELFVis:
  case HIDDEN
  case DEFAULT

enum ELFNDX:
  case Section(num: Int) /* Section containing the symbol */
  case UND /* Undefined */
  case ABS /* Absolute, unaffected by relocation */

case class ELFSymbol(num: Int,  /* symbol number */
  value: BigInt, /* symbol address */
  size: Int,  /* symbol size (bytes) */
  etype: ELFSymType,  
  bind: ELFBind,  
  vis: ELFVis,
  ndx: ELFNDX, /* The section containing the symbol */
  name: String)

object ReadELFLoader {
  // TODO: load NOTYPE symbols, so we can get _bss_start ... _bss_end to zero-init in interpreter, as well as _end to find bottom of heap
  def visitSyms(ctx: SymsContext, config: ILLoadingConfig): (List[ELFSymbol], Set[ExternalFunction], Set[SpecGlobal], Map[BigInt, BigInt], Int) = {
    val externalFunctions = ctx.relocationTable.asScala.flatMap(r => visitRelocationTableExtFunc(r)).toSet
    val relocationOffsets = ctx.relocationTable.asScala.flatMap(r => visitRelocationTableOffsets(r)).toMap
    val mainAddress = ctx.symbolTable.asScala.flatMap(s => getFunctionAddress(s, config.mainProcedureName))

    val symbolTable = ctx.symbolTable.asScala.flatMap(s => visitSymbolTable(s)).toList
    val globalVariables = (symbolTable.collect {
      case ELFSymbol(num, value, size, ELFSymType.OBJECT, ELFBind.GLOBAL, ELFVis.DEFAULT, _, name) =>  SpecGlobal(name, size * 8, None, value)
    }).toSet
    
    if (mainAddress.isEmpty) {
      throw Exception(s"no ${config.mainProcedureName} function in symbol table")
    }
    (symbolTable, externalFunctions, globalVariables, relocationOffsets, mainAddress.head)
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


  def visitSymbolTable(ctx: SymbolTableContext): List[ELFSymbol] = {
    if (ctx.symbolTableHeader.tableName.STRING.getText == ".symtab") {
      ctx.symbolTableRow.asScala.map(getSymbolTableRow).toList
    } else {
      List()
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
