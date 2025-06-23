package gtirb

import util.Logger
import gtirb.AuxDecoder
import gtirb.AuxDecoder.{AuxKind, decodeAux}

import translating.{ELFSymType, ELFBind, ELFVis, ELFNDX, ELFSymbol, ReadELFData}
import specification.{ExternalFunction, FuncEntry}
import boogie.{SpecGlobal}

import java.io.ByteArrayInputStream

import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.CFG.EdgeType.*
import com.grammatech.gtirb.proto.CFG.CFG
import com.grammatech.gtirb.proto.CFG.Edge
import com.grammatech.gtirb.proto.CFG.EdgeLabel
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Symbol.Symbol
import com.grammatech.gtirb.proto.ByteInterval.Block
import com.grammatech.gtirb.proto.ByteInterval.ByteInterval
import com.grammatech.gtirb.proto.Symbol.Symbol.OptionalPayload

import scala.collection.mutable
import scala.collection.immutable.{SortedMap, SortedSet}

class GTIRBReadELF(protected val gtirb: GTIRBResolver) {

  /**
   * An `Elf64_Rela` structure, as described by the [System V ABI](https://refspecs.linuxfoundation.org/elf/gabi4+/ch4.reloc.html).
   * The three fields `r_offset`, `r_info`, and `r_addend` are as described in the struct.
   * The last two fields, `r_sym` and `r_type`, are extracted from the `r_info` value.
   *
   * The [ABI supplement for AArch64](https://github.com/ARM-software/abi-aa/blob/main/aaelf64/aaelf64.rst#relocation-types)
   * provides information about the interpretation of the `r_type` values.
   */
  case class Elf64Rela(r_offset: BigInt, r_info: BigInt, r_addend: BigInt, r_sym: Long, r_type: Long)

  // https://refspecs.linuxbase.org/elf/gabi4+/ch4.reloc.html
  // https://github.com/ARM-software/abi-aa/blob/main/aaelf64/aaelf64.rst#relocation-types
  protected def readRela(bs: AuxDecoder.Input) =
    import AuxDecoder.*
    val (r_offset, r_info, r_addend) = readTuple(readUint(64), readUint(64), readUint(64))(bs)
    val r_sym = r_info >> 32
    val r_type = r_info & 0xffffffffL
    Elf64Rela(r_offset, r_info, r_addend, r_sym.toLong, r_type.toLong)

  protected def parseRelaTab(bstr: ByteString) =
    val bs = ByteArrayInputStream(bstr.toByteArray)
    List.unfold(bs) {
      case bs if bs.available() > 0 => Some(readRela(bs), bs)
      case _ => None
    }

  // see also:
  // https://www.javadoc.io/doc/net.fornwall/jelf/latest/net/fornwall/jelf/ElfSymbol.html
  //
  // https://gist.github.com/x0nu11byt3/bcb35c3de461e5fb66173071a2379779
  //
  // https://www.man7.org/linux/man-pages/man5/elf.5.html

  // Full ELF32 specification: https://refspecs.linuxfoundation.org/elf/elf.pdf

  // Full ELF64 specification: https://irix7.com/techpubs/007-4658-001.pdf

  /**
   * https://refspecs.linuxfoundation.org/elf/elf.pdf
   * Figure 1-7. Special Section Indexes
   */
  protected def parseElfNdx(n: BigInt) = n.toInt match {
    case 0 => ELFNDX.UND
    case 0xfff1 => ELFNDX.ABS
    case i =>
      if (i >= 0xff00)
        Logger.warn("unhandled special elf section index: " + i)
      ELFNDX.Section(i)
  }

  /**
   * https://github.com/ARM-software/abi-aa/blob/main/aaelf64/aaelf64.rst#dynamic-relocations
   */
  def parseRela(rela: Elf64Rela) =
    val sym = gtirb.symbolTables(".dynsym")(rela.r_sym.toInt).get

    rela.r_type match {
      case 1025 | 1026 => Right(ExternalFunction(sym.name, rela.r_offset))
      case 1027 => Left((rela.r_offset, rela.r_addend))
      case 1024 => Left((BigInt(0), BigInt(0)))
    }

  def getAllSymbols() = {
    gtirb.symbolEntriesByUuid
      .flatMap { case (k, pos) =>
        val sym = k.get

        val idx = k.symTabIdx.collectFirst { case (".symtab", i) =>
          i.toInt
        }

        val addr = k.getReferentAddress
        val value = k.getScalarValue
        val combinedValue = addr.orElse(value).getOrElse(0L)

        val (size, ty, bind, vis, shndx) = k.symEntry

        ty match {
          case "NONE" => None
          case ty =>
            Some(
              ELFSymbol(
                idx.getOrElse(-1),
                combinedValue,
                size.toInt,
                ELFSymType.valueOf(ty),
                ELFBind.valueOf(bind),
                ELFVis.valueOf(vis),
                parseElfNdx(shndx),
                sym.name
              )
            )
        }
      }
      .toList
      .sortBy(x => x.num)
  }

  def getRelocations() = {
    val relaDyns = parseRelaTab(gtirb.sectionsByName(".rela.dyn").byteIntervals.head.contents)
    val relaPlts = parseRelaTab(gtirb.sectionsByName(".rela.plt").byteIntervals.head.contents)

    val (offs, exts) = (relaDyns.view ++ relaPlts.view).partitionMap(parseRela)

    (offs.toMap, exts.toSet)
  }

  def getGlobals() = {
    gtirb.symbolEntriesByUuid.view.collect { case (symid, (size, "OBJECT", "GLOBAL", "DEFAULT", idx)) =>
      val blk = symid.getReferentUuid.get.getOption
      // val sec = blk.section
      // assert(gtirb.mod.sections(idx.toInt - 1) == sec)
      SpecGlobal(symid.get.name, (size * 8).toInt, None, blk.fold(BigInt(-1))(_.address))
    }.toSet
  }

  def getFunctionEntries() = {

    gtirb.symbolEntriesByUuid.view.collect {
      case (symid, (size, "FUNC", "GLOBAL", "DEFAULT", idx)) if idx != 0 =>

        val nameSymbol = symid.get
        val funcUuid = symid.getFunction.get
        val entries = funcUuid.getEntries

        assert(entries.size == 1, "function with non-singular entry")
        val entry = entries.head
        val addr = entry.get.address

        FuncEntry(nameSymbol.name, (size * 8).toInt, addr)
    }.toSet
  }

  def getMainAddress(mainProcedureName: String) = {
    gtirb.symbolsByName(mainProcedureName).getReferentUuid.get.get.address
  }

  def getReadELFData(mainProcedureName: String) = {

    val syms = getAllSymbols()
    val (offs, exts) = getRelocations()
    val globs = getGlobals()
    val funs = getFunctionEntries()
    val main = getMainAddress(mainProcedureName)

    val x = SortedSet.from(exts)(Ordering.by(_.toString))
    println(x)
    ReadELFData(syms, x, SortedSet.from(globs), funs, offs, main)
  }

  private val atSuffix = """@[A-Za-z_\d.]+$""".r

  /**
   * Strips away some information from `readelf`'s [[translating.ReadELFData]]
   * which is not so important and not produced by the GTIRB ELF loader.
   *
   * For example, this throws away symbols of type SECTION and symbols beginning with `$`.
   * It also strips the `@GLIBC_XX.X` suffix from symbol names.
   */
  def normaliseRelf(relf: ReadELFData) = {
    val exts = relf.externalFunctions.map(x => x.copy(name = atSuffix.replaceFirstIn(x.name, "")))
    val syms = relf.symbolTable.flatMap {
      case ELFSymbol(_, 0, 0, ELFSymType.FILE, ELFBind.LOCAL, ELFVis.DEFAULT, ELFNDX.ABS, "crtstuff.c") => None
      case sym if sym.etype != ELFSymType.SECTION && sym.num != -1 && !sym.name.startsWith("$") =>
        Some(sym.copy(name = atSuffix.replaceFirstIn(sym.name, "")))
      case _ => None
    }

    relf.copy(externalFunctions = exts, symbolTable = syms)
  }

  /**
   * Determines whether the current ReadELFData is compatible with
   * a given reference ReadELFData. That is, whether the The given reference object is
   * assumed to be the gold standard
   */
  def checkReadELFCompatibility(gtirbRelf: ReadELFData, referenceRelf: ReadELFData): Boolean = {
    var ok = true

    inline def check(b: Boolean, s: String) = {
      if (!b) {
        Logger.warn("PLEASE REPORT THIS ISSUE! include the gts and relf files. gtirb relf discrepancy, " + s)
        ok = false
      }
    }

    inline def checkSet[T](x: Set[T], y: Set[T], s: String) =
      check(x == y, s"$s:\ngtirb - relf = ${x -- y}\nrelf - gtirb = ${y -- x}")

    inline def checkEq(x: Any, y: Any, s: String) =
      check(x == y, s"$s: gtirb: $x, readelf: $y}")

    val g = normaliseRelf(gtirbRelf)
    val o = normaliseRelf(referenceRelf)
    checkEq(g.mainAddress, o.mainAddress, "main address differs")
    checkEq(g.functionEntries, o.functionEntries, "function entries differ")
    checkEq(g.relocationOffsets, o.relocationOffsets, "relocations differ")
    checkEq(g.globalVariables, o.globalVariables, "global variables differ")
    checkSet(g.externalFunctions, o.externalFunctions, "external functions differ")
    checkSet(g.symbolTable.toSet, o.symbolTable.toSet, "symbol tables differ")

    Logger.debug("gtirb relf and readelf relf compatible: " + ok)
    ok
  }

}
