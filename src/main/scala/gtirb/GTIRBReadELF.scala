package gtirb

import boogie.SpecGlobal
import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.CFG.EdgeType.*
import specification.{ExternalFunction, FuncEntry}
import translating.{ELFBind, ELFNDX, ELFSymType, ELFSymbol, ELFVis, ReadELFData}
import util.Logger

import java.io.ByteArrayInputStream
import scala.util.DynamicVariable
import scala.util.chaining.scalaUtilChainingOps

/**
 * Responsible for interpreting the GTIRB's symbol information
 * and producing ELF information in a format matching [[translating.ReadELFLoader]].
 *
 * **Useful links:**
 *
 * - Full ELF64 specification, useful for symbol kinds/visibility/binding: https://irix7.com/techpubs/007-4658-001.pdf
 * - Full ELF32 specification: https://refspecs.linuxfoundation.org/elf/elf.pdf
 * - ELF relocation specification, for relocation struct definition: https://refspecs.linuxbase.org/elf/gabi4+/ch4.reloc.html
 * - Aarch64 ELF supplement, for relocation types: https://github.com/ARM-software/abi-aa/blob/main/aaelf64/aaelf64.rst#relocation-types
 * - An ELF cheatsheet: https://gist.github.com/x0nu11byt3/bcb35c3de461e5fb66173071a2379779
 * - elf man page, extra details: https://www.man7.org/linux/man-pages/man5/elf.5.html
 *
 */
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

  /**
   * An Aarch64 relocation type, with constants from:
   * https://github.com/ARM-software/abi-aa/blob/main/aaelf64/aaelf64.rst#relocation-types
   */
  sealed trait Elf64RelaType(val value: Long)
  case object R_AARCH64_COPY extends Elf64RelaType(1024)
  case object R_AARCH64_GLOB_DAT extends Elf64RelaType(1025)
  case object R_AARCH64_JUMP_SLOT extends Elf64RelaType(1026)
  case object R_AARCH64_RELATIVE extends Elf64RelaType(1027)

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

  /**
   * Parsers an Aarch64 relocation type integer. See [[Elf64RelaType]] for constants.
   */
  protected def parseAarch64RelaType(x: Long) = x match {
    case R_AARCH64_COPY.value => R_AARCH64_COPY
    case R_AARCH64_GLOB_DAT.value => R_AARCH64_GLOB_DAT
    case R_AARCH64_JUMP_SLOT.value => R_AARCH64_JUMP_SLOT
    case R_AARCH64_RELATIVE.value => R_AARCH64_RELATIVE
  }

  /**
   * https://refspecs.linuxfoundation.org/elf/elf.pdf.
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

  def parseRelaExtFunc(rela: Elf64Rela): ExternalFunction =
    val sym = gtirb.getDynSym(rela.r_sym.toInt).get
    ExternalFunction(sym.name, rela.r_offset)

  def parseRela(kind: R_AARCH64_RELATIVE.type, rela: Elf64Rela): (BigInt, BigInt) =
    (rela.r_offset, rela.r_addend)

  def parseRela(kind: R_AARCH64_COPY.type, rela: Elf64Rela): gtirb.SymbolRef =
    gtirb.getDynSym(rela.r_sym.toInt)

  def getAllSymbols(): List[ELFSymbol] = {
    val normalsyms = gtirb.symbolEntriesByUuid.view
      .flatMap { case (k, pos) =>
        val sym = k.get

        val idx = k.symTabIdx.collectFirst { case (".symtab", i) =>
          i.toInt
        }

        val addr = k.getReferentAddress
        val value = k.getScalarValue
        val combinedValue = addr.orElse(value).getOrElse(0L)

        val (size, ty, bind, vis, shndx) = k.symEntry

        val name = sym.name

        (ty, idx) match {
          case ("NONE", _) => None
          case (_, None) => None
          case (ty, Some(idx)) =>
            Some(
              ELFSymbol(
                idx,
                combinedValue,
                size.toInt,
                ELFSymType.valueOf(ty),
                ELFBind.valueOf(bind),
                ELFVis.valueOf(vis),
                parseElfNdx(shndx),
                name
              )
            )
        }
      }

    val sectionsyms = gtirb.mod.sections.view.zipWithIndex.map { case (sec, i) =>
      val addr = sec.byteIntervals.head.address
      val num = i + 1

      ELFSymbol(num, addr, 0, ELFSymType.SECTION, ELFBind.LOCAL, ELFVis.DEFAULT, ELFNDX.Section(num), sec.name)
    }

    (normalsyms ++ sectionsyms).toList
      .sortBy(x => x.num)
  }

  /**
   * Returns relocations as a tuple of relocation offsets and external functions.
   */
  def getRelocations(): (Map[BigInt, BigInt], Set[ExternalFunction]) = {
    def getSectionBytes(sectionName: String) =
      gtirb.sectionsByName(sectionName).byteIntervals.head.contents

    val relaDyns = getSectionBytes(".rela.dyn").pipe(parseRelaTab)
    val relaPlts = getSectionBytes(".rela.plt").pipe(parseRelaTab)

    val relas = (relaDyns ++ relaPlts)
      .groupBy(x => parseAarch64RelaType(x.r_type))
      .withDefaultValue(Nil)

    val offs = relas(R_AARCH64_RELATIVE).map(parseRela(R_AARCH64_RELATIVE, _))
    val exts = (relas(R_AARCH64_GLOB_DAT) ++ relas(R_AARCH64_JUMP_SLOT)).map(parseRelaExtFunc(_))

    (offs.toMap, exts.toSet)
  }

  def getGlobals(): Set[SpecGlobal] =
    gtirb.symbolEntriesByUuid.view.flatMap {
      case (symid, (size, "OBJECT", "GLOBAL" | "LOCAL", "DEFAULT", idx)) =>

        // val addr = symid.getReferentUuid.map(_.get.address)
        val addr = symid.getReferentAddress
        addr match {
          case Some(addr) =>
            Some(SpecGlobal(symid.get.name, (size * 8).toInt, None, addr))

          // if the referent is not a real block, then this is a
          // forwarding target symbol. discard, because we generate
          // the SpecGlobal from the forwarding source symbol.
          case None =>
            assert(
              gtirb.symbolForwardingInverse.contains(symid),
              "a symbol with a referent that has no data block should be a forwarding target"
            )
            None
        }
      case _ => None
    }.toSet

  def getFunctionEntries(): Set[FuncEntry] =
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

  def getMainAddress(mainProcedureName: String): BigInt =
    gtirb.symbolsByName(mainProcedureName).getReferentAddress.get

  def getReadELFData(mainProcedureName: String): ReadELFData = {

    val (offs, exts) = getRelocations()
    val syms = getAllSymbols()
    val globs = getGlobals()
    val funs = getFunctionEntries()
    val main = getMainAddress(mainProcedureName)

    ReadELFData(syms, exts, globs, funs, offs, main)
  }

}

object GTIRBReadELF {

  enum RelfCompatibilityLevel { outer =>
    case Silent, Warning, Exception

    /** Returns true iff the current [[RelfCompatibilityLevel]] is equal to or
     *  stricter than the given [[RelfCompatibilityLevel]].
     */
    def isAtLeast(baseline: RelfCompatibilityLevel) =
      this.ordinal - baseline.ordinal >= 0

    def isAtLeastWarning() = this.isAtLeast(RelfCompatibilityLevel.Warning)
    def isAtLeastException() = this.isAtLeast(RelfCompatibilityLevel.Exception)
  }

  /**
   * This affects the behaviour of [[checkReadELFCompatibility]] when finding
   * mismatched [[translating.ReadELFData]]. The default is [[RelfCompatibilityLevel.Exception]],
   * but this is overridden by the main entry point of the Basil tool and certain test cases.
   */
  final val relfCompatibilityLevel = DynamicVariable(RelfCompatibilityLevel.Exception)
  // TODO: collect into centralised flag system

  /** Sets [[relfCompatibilityLevel]] to warning within the given block. */
  def withWarnings[T](f: => T) = relfCompatibilityLevel.withValue(RelfCompatibilityLevel.Warning)(f)

  /** Sets [[relfCompatibilityLevel]] to silent within the given block. */
  def withSilent[T](f: => T) = relfCompatibilityLevel.withValue(RelfCompatibilityLevel.Silent)(f)

  private val atSuffix = """@[A-Za-z_\d.]+$""".r

  /**
   * Strips away some information from [[translating.ReadELFData]]
   * which is not so important and causes spurious mismatches between the two
   * ELF loaders.
   *
   * For example, this throws away symbols of type SECTION and symbols beginning with `$`.
   * It also strips the `@GLIBC_XX.X` suffix from symbol names.
   */
  def normaliseRelf(relf: ReadELFData) = {
    val exts = relf.externalFunctions.map(x => x.copy(name = atSuffix.replaceFirstIn(x.name, "")))
    val syms = relf.symbolTable.flatMap {
      case ELFSymbol(_, 0, 0, ELFSymType.FILE, ELFBind.LOCAL, ELFVis.DEFAULT, ELFNDX.ABS, "crtstuff.c") => None
      case ELFSymbol(_, 0, 0, ELFSymType.SECTION, ELFBind.LOCAL, ELFVis.DEFAULT, ELFNDX.Section(_), ".comment") => None
      case sym if sym.name.startsWith("$") => None

      case sym => Some(sym.copy(name = atSuffix.replaceFirstIn(sym.name, "")))
    }
    val globs = relf.globalVariables.map { x =>
      x.copy(name = atSuffix.replaceFirstIn(x.name, ""))
    }

    // oldrelf will generate both SpecGlobal and ExternalFunction entries for external global variables
    // (e.g., errno, optind). subtract these from the ExternalFunctions, as the SpecGlobal entry
    // is more correct and contains strictly more information.
    val externalGlobalVariables = globs.view.map { glo => ExternalFunction(glo.name, glo.address) }

    relf.copy(externalFunctions = exts -- externalGlobalVariables, symbolTable = syms, globalVariables = globs)
  }

  /**
   * Determines whether the current ReadELFData is compatible with
   * a given reference ReadELFData. That is, whether the two ELF datas are
   * equivalent when normalised with [[normaliseRelf]]. If mismatching, prints
   * warning-level messages to the log and exhorts the user to report the issue.
   * This may throw, depending on the value of [[relfCompatibilityLevel]].
   */
  def checkReadELFCompatibility(gtirbRelf: ReadELFData, referenceRelf: ReadELFData): Boolean = {
    var ok = true

    val level = relfCompatibilityLevel.value

    inline def check(b: Boolean, s: String) = {
      if (!b) {
        val exhortation = if (level.isAtLeastWarning()) {
          "PLEASE REPORT THIS ISSUE (https://github.com/UQ-PAC/BASIL/issues/509)! include the gts and relf files.\n"
        } else {
          "(suppressed) "
        }
        Logger.warn(exhortation + "gtirb relf discrepancy, " + s)
        ok = false
      }
    }

    inline def checkSet[T](x: Set[T], y: Set[T], s: String) =
      check(x == y, s"$s:\ngtirb - relf = ${x -- y}\nrelf - gtirb = ${y -- x}")

    inline def checkEq(x: Any, y: Any, s: String) =
      check(x == y, s"$s: gtirb: $x, readelf: $y}")

    var g = normaliseRelf(gtirbRelf)
    var o = normaliseRelf(referenceRelf)

    // XXX: account for gtsrelf moving the bss_start symbols to .bss in certain binaries on certain compilers.
    {
      val gs = g.symbolTable.toSet
      val os = o.symbolTable.toSet
      val bss = g.symbolTable.collectFirst {
        case ELFSymbol(_, addr, 0, ELFSymType.SECTION, ELFBind.LOCAL, ELFVis.DEFAULT, ELFNDX.Section(_), ".bss") => addr
      }
      val bssNames = Seq("__bss_start", "__bss_start__")
      val bssGtirb = (gs -- os).filter(bssNames contains _.name)

      bss match {
        // if the gtsrelf incorrectly has bss_start pointing to .bss, rewrites the oldrelf to have the same,
        // effectively ignoring this mismatch
        case Some(bss) if bssGtirb.nonEmpty && bssGtirb.forall(_.value == bss) =>
          o = o.copy(symbolTable = o.symbolTable.map {
            case sym if bssNames contains sym.name => sym.copy(value = bss)
            case x => x
          })
          Logger.warn("ignoring bss_start quirk in gtsrelf data")
        case _ => ()
      }
    }

    checkEq(g.mainAddress, o.mainAddress, "main address differs")
    checkSet(g.functionEntries, o.functionEntries, "function entries differ")
    checkSet(g.relocationOffsets.toSet, o.relocationOffsets.toSet, "relocations differ")
    checkSet(g.globalVariables, o.globalVariables, "global variables differ")
    checkSet(g.externalFunctions, o.externalFunctions, "external functions differ")
    checkSet(g.symbolTable.toSet, o.symbolTable.toSet, "symbol tables differ")

    Logger.debug("gtirb relf and readelf relf compatibility result: " + ok)
    if (level.isAtLeastException()) {
      assert(ok, "gtirb/relf incompatibility")
    }
    ok
  }

}
