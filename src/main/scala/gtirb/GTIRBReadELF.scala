package gtirb

import gtirb.AuxDecoder
import gtirb.AuxDecoder.{AuxKind, decodeAux}

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
import scala.collection.immutable.SortedMap

object GTIRBReadELF {

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
  def readRela(bs: AuxDecoder.Input) =
    import AuxDecoder.*
    val (r_offset, r_info, r_addend) = readTuple(readUint(64), readUint(64), readUint(64))(bs)
    val r_sym = r_info >> 32
    val r_type = r_info & 0xffffffffL
    Elf64Rela(r_offset, r_info, r_addend, r_sym.toLong, r_type.toLong)

  def parseRelaTab(bstr: ByteString) =
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

  def getExternalFunctions(gtirb: GTIRBResolver) = {

    val mod = gtirb.mod

    val relaDyns = parseRelaTab(gtirb.sectionsByName(".rela.dyn").byteIntervals.head.contents)
    val relaPlts = parseRelaTab(gtirb.sectionsByName(".rela.plt").byteIntervals.head.contents)

    import scala.math.Ordering.Implicits.seqOrdering
    val allSymbols = gtirb.symbolKindsByUuid
      .map { case (k, pos) =>
        val sym = k.get
        println(k)
        val addr = k.getReferentBlock.map(_.address)
        val value = k.getScalarValue.fold("")("val=" + _.toString)
        (k.symTabIdx, addr, pos) -> s"${sym.name} $value"
      }
      .to(SortedMap)
    println(allSymbols.mkString("\n"))

    println()
    println(".rela.dyn")
    relaDyns.foreach { case x =>
      val symid = gtirb.symbolTables(".dynsym")(x.r_sym.toInt)
      println(s"$x " + symid.get.name)
    }
    println(".rela.plt")
    relaPlts.foreach { case x =>
      val symid = gtirb.symbolTables(".dynsym")(x.r_sym.toInt)
      println(s"$x " + symid.get.name)
    }

    val specGlobals = gtirb.symbolKindsByUuid.toList.collect {
      case (symid, (size, "OBJECT", "GLOBAL", "DEFAULT", idx)) =>
        val blk = symid.getReferentBlock.get
        val sec = blk.section
        assert(mod.sections(idx.toInt - 1) == sec)
        (symid.get.name, blk.size * 8, None, blk.address)
    }
    println(specGlobals)

    val funentry = gtirb.symbolKindsByUuid.toList.collect {
      case (symid, (size, "FUNC", "GLOBAL", "DEFAULT", idx)) if idx != 0 =>

        val nameSymbol = symid.get
        val funcUuid = symid.getFunction.get
        val entries = funcUuid.getEntries

        assert(entries.size == 1, "function with non-singular entry")
        val entry = entries.head
        val addr = entry.get.address

        (nameSymbol.name, size * 8, addr)
    }

    println(funentry)

  }
}
