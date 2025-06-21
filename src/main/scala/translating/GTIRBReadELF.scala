package translating

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

  def getExternalFunctions(mod: Module) = {

    val proxyBlockUuids = mod.proxies.map(_.uuid).toSet
    val externalFunctionSymbols = mod.symbols.filter(x => proxyBlockUuids.contains(x.getReferentUuid))
    val symbolsByUuid = mod.symbols.map(x => x.uuid -> x).toMap

    val dataBlocksByUuid = (for {
      sec <- mod.sections.toList
      interval <- sec.byteIntervals
      (b, innerb) <- interval.blocks.collect { case b @ Block(_, Block.Value.Data(dat), _) =>
        (b, dat)
      // case b @ Block(_, Block.Value.Code(cod), _) => (b, cod)
      }
    } yield innerb.uuid -> (innerb, b, interval, sec)).toMap

    val codeBlocksByUuid = (for {
      sec <- mod.sections.toList
      interval <- sec.byteIntervals
      (b, innerb) <- interval.blocks.collect { case b @ Block(_, Block.Value.Code(dat), _) =>
        (b, dat)
      }
    } yield innerb.uuid -> (innerb, b, interval, sec)).toMap

    val sectionsByName = mod.sections.map(x => x.name -> x).toMap
    val relaDyns = parseRelaTab(sectionsByName(".rela.dyn").byteIntervals.head.contents)
    val relaPlts = parseRelaTab(sectionsByName(".rela.plt").byteIntervals.head.contents)

    val symbolTabIdx = AuxDecoder.decodeAux(AuxDecoder.AuxKind.ElfSymbolTabIdxInfo)(mod)
    val tabidx = symbolTabIdx
      .flatMap { case (sym, idxs) =>
        idxs.map(_ -> sym)
      }
      .groupMapReduce(kv => kv.head.head)(kv => SortedMap(kv.head.last -> kv.last))(_ ++ _)
    // println(tabidx)

    val symbolKinds = decodeAux(AuxKind.ElfSymbolInfo)(mod)

    import scala.math.Ordering.Implicits.seqOrdering
    val allSymbols = symbolKinds
      .map { case (k, pos) =>
        val sym = symbolsByUuid(k)
        val addr = for {
          uuid <- sym.optionalPayload.referentUuid
          (_, block: Block, ival: ByteInterval, _) <- dataBlocksByUuid.get(uuid).orElse(codeBlocksByUuid.get(uuid))
        } yield (block.offset + ival.address)
        val value = sym.optionalPayload._value.fold("")("val=" + _.toString)
        (symbolTabIdx(k), addr, pos) -> s"${sym.name} $value"
      }
      .to(SortedMap)
    println(allSymbols.mkString("\n"))

    println()
    println(".rela.dyn")
    relaDyns.foreach { case x =>
      val symuuid = tabidx(".dynsym")(x.r_sym.toInt)
      println(s"$x " + symbolsByUuid.get(symuuid).map(_.name).filter(_.nonEmpty))
    }
    println(".rela.plt")
    relaPlts.foreach { case x =>
      val symuuid = tabidx(".dynsym")(x.r_sym.toInt)
      println(s"$x " + symbolsByUuid.get(symuuid).map(_.name).filter(_.nonEmpty))
    }

    val specGlobals = symbolKinds.toList.collect { case (uuid, (size, "OBJECT", "GLOBAL", "DEFAULT", idx)) =>
      val sym = symbolsByUuid(uuid)
      val (data, block, interval, sec) = dataBlocksByUuid(sym.optionalPayload.referentUuid.get)
      // assert(size == data.size)
      assert(mod.sections(idx.toInt - 1) == sec)
      (sym.name, size * 8, None, interval.address + block.offset)
    }
    println(specGlobals)

    val funcNames = decodeAux(AuxKind.FunctionNames)(mod)
    val funcNamesInverse = funcNames.map(_.swap)

    val funcEntries = decodeAux(AuxKind.FunctionEntries)(mod)
    val funentry = symbolKinds.toList.collect {
      case (symuuid, (size, "FUNC", "GLOBAL", "DEFAULT", idx)) if idx != 0 =>

        val nameSymbol = symbolsByUuid(symuuid)
        val funcUuid = funcNamesInverse(symuuid)
        val entries = funcEntries(funcUuid)

        assert(entries.size == 1, "function with non-singular entry")
        val entry = entries.head
        val (_, bl, ival, _) = codeBlocksByUuid(entry)
        val addr = bl.offset + ival.address

        (nameSymbol.name, size * 8, addr)
    }

    println(funentry)

  }
}
