package gtirb

import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.ByteInterval.{Block, ByteInterval}
import com.grammatech.gtirb.proto.CFG.EdgeType.*
import com.grammatech.gtirb.proto.CodeBlock.CodeBlock
import com.grammatech.gtirb.proto.DataBlock.DataBlock
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Section.Section
import com.grammatech.gtirb.proto.Symbol.Symbol
import com.grammatech.gtirb.proto.Symbol.Symbol.OptionalPayload
import gtirb.AuxDecoder.{AuxKind, decodeAux}

import scala.collection.immutable.SortedMap

/**
 * A class for querying the GTIRB IR, abstracting away common operations of
 * searching for symbols, functions, blocks, and their relations. The inner
 * type [[GTIRBRef]] wraps a Base64 UUID. A number of UUID subtypes are defined to
 * distinguish different kinds of GTIRB UUID, for example [[GTIRBRef.BlockRef]]
 * and [[GTIRBRef.FunctionRef]].
 *
 * Each Uuid specialisation defines a number of extension methods for common
 * query operations. For example, given a [[GTIRBRef.SymbolRef]], you can get the symbol
 * itself via the `.get` methods, and you can get its symbol table entry with the
 * [[symEntry]] method. Internally, the [[GTIRBResolver]] is indexing into the GTIRB
 * protobuf and parsing the AuxData, but this is all neatly hidden away.
 */
case class GTIRBResolver(val mod: Module) {

  sealed trait GTIRBRef(val kind: String, val uuid: String) {
    override def toString = s"$kind:$uuid"
    override def equals(o: Any) = o match {
      case x: GTIRBRef => x.kind == kind && x.uuid == uuid
      case _ => false
    }
    override def hashCode = (kind, uuid).hashCode
  }

  object GTIRBRef {

    private def b64(bs: String | ByteString) = bs match {
      case s: String => s
      case bs: ByteString => java.util.Base64.getEncoder().encodeToString(bs.toByteArray)
    }

    class BlockRef(xs: String | ByteString) extends GTIRBRef("blok", b64(xs))
    class FunctionRef(xs: String | ByteString) extends GTIRBRef("func", b64(xs))
    class SymbolRef(xs: String | ByteString) extends GTIRBRef("symb", b64(xs))
    class SectionRef(xs: String | ByteString) extends GTIRBRef("sect", b64(xs))
  }

  export GTIRBRef.*

  /**
   * Represents a GTIRB code/data block and its parents. In GTIRB, block
   * occurs within a byte interval which occur within a section. Desirable information,
   * such as offset and address, is spread across these levels, so it is useful to bundle
   * them all together.
   *
   * See the [GTIRB structure diagram](https://github.com/GrammaTech/gtirb#structure).
   */
  case class BlockData(inner: DataBlock | CodeBlock, block: Block, interval: ByteInterval, section: Section) {
    val uuid = inner match {
      case x: DataBlock => BlockRef(x.uuid)
      case x: CodeBlock => BlockRef(x.uuid)
    }
    val size = inner match {
      case x: DataBlock => x.size
      case x: CodeBlock => x.size
    }
    val address = BigInt(block.offset) + BigInt(interval.address)
  }

  extension (x: BlockRef)
    def get = blocksByUuid(x)
    def getOption = blocksByUuid.get(x)
    def isProxyBlock = proxyBlockUuids.contains(x)

  extension (x: SymbolRef)
    def get: Symbol = {
      val sym = symbolsByUuid(x)

      // XXX: forwarded object symbols correspond to R_AARCH64_COPY relocations.
      // for these, ddisasm produces a `*_copy` symbol. get the original symbol
      // name by following the forwarding.
      (getForwardingTarget, symEntry) match {
        case (Some(fwd), (_, "OBJECT", "GLOBAL", "DEFAULT", _)) =>
          sym.copy(name = fwd.get.name)
        case _ => sym
      }
    }

    /**
     * Returns the list of symbol table indices where this symbol can be found.
     * Each index is a tuple of table name and index within that table.
     */
    def symTabIdx = symbolTabIdxByUuid.getOrElse(x, Nil)

    /**
     * Returns the `.symtab` entry for the given symbol.
     * This is a 5-tuple made up of size, type, binding, visibility, and section index.
     * Every symbol table entry is in relation to some section. The section index is the
     * index of the relevant section's section header.
     */
    def symEntry = symbolEntriesByUuid(x)

    /**
     * Gets the [[GTIRBRef.BlockRef]] referred to by this symbol, for example
     * a data block or code block.
     * This is mutually-exclusive with [[getScalarValue]],
     * only one of these can be non-None.
     */
    def getReferentUuid = for {
      uuid <- x.get.optionalPayload.referentUuid
    } yield BlockRef(uuid)

    /**
     * Gets the address referred to by this symbol, if the referent is a
     * block. Correctly takes into account the `atEnd` field of [[Symbol]].
     */
    def getReferentAddress = for {
      uuid <- x.getReferentUuid
      block <- uuid.getOption
      atEndOffset = if x.get.atEnd then block.size else 0
    } yield block.address + atEndOffset

    /**
     * Gets the scalar value associated with this symbol.
     * This is mutually-exclusive with [[getReferentUuid]],
     * only one of these can be non-None.
     */
    def getScalarValue = x.get.optionalPayload._value

    /**
     * Gets the [[GTIRBRef.FunctionRef]] associated with this symbol,
     * or None if this is not a function name symbol.
     */
    def getFunction = funcNamesInverse.get(x)

    def getForwardingTarget = symbolForwarding.get(x)

  extension (x: FunctionRef)
    /**
     * Gets the entry block(s) for the given function.
     */
    def getEntries = funcEntries(x)

    /**
     * Gets the [[GTIRBRef.SymbolRef]] for the given function.
     */
    def getName = funcNames(x)

    /**
     * Gets the constituent block(s) for the given function.
     */
    def getBlocks = funcBlocks(x)

  private def mapFirst[T, T2, U](f: T => T2)(x: (T, U)) = (f(x._1), x._2)

  val proxyBlockUuids = mod.proxies.map(x => BlockRef(x.uuid)).toSet
  val symbolsByUuid = mod.symbols.map(x => SymbolRef(x.uuid) -> x).toMap
  val symbolsByName = mod.symbols.map(x => x.name -> SymbolRef(x.uuid)).toMap

  val blocksByUuid = (for {
    sec <- mod.sections.toList
    interval <- sec.byteIntervals
    (uuid, innerb, outerb) <- interval.blocks.collect {
      case b @ Block(_, Block.Value.Data(dat), _) => (dat.uuid, (dat: DataBlock | CodeBlock), b)
      case b @ Block(_, Block.Value.Code(cod), _) => (cod.uuid, (cod: DataBlock | CodeBlock), b)
    }
    id = BlockRef(uuid)
  } yield id -> BlockData(innerb, outerb, interval, sec)).toMap

  val sectionsByName = mod.sections.map(x => x.name -> x).toMap

  val symbolTabIdxByUuid: Map[SymbolRef, List[(String, BigInt)]] =
    decodeAux(AuxKind.ElfSymbolTabIdxInfo)(mod).map(mapFirst(SymbolRef(_)))

  /**
   * A nested map indexed by section name, then symbol index, and returning a symbol uuid.
   * For example, `symbolTables(".symtab")(63)`.
   */
  val symbolTables = symbolTabIdxByUuid
    .flatMap { case (sym, idxs) =>
      idxs.map(_ -> sym)
    }
    .groupMapReduce(kv => kv.head.head)(kv => SortedMap(kv.head.last -> kv.last))(_ ++ _)

  val symbolEntriesByUuid = decodeAux(AuxKind.ElfSymbolInfo)(mod)
    .map(mapFirst(SymbolRef(_)))

  val funcNames = decodeAux(AuxKind.FunctionNames)(mod).map { case (fun, sym) =>
    FunctionRef(fun) -> SymbolRef(sym)
  }
  val funcNamesInverse = funcNames.map(_.swap)
  val funcEntries = decodeAux(AuxKind.FunctionEntries)(mod).map { case (a, b) =>
    FunctionRef(a) -> b.map(BlockRef(_))
  }
  val funcBlocks = decodeAux(AuxKind.FunctionBlocks)(mod).map { case (a, b) =>
    FunctionRef(a) -> b.map(BlockRef(_))
  }

  val entryPoint = BlockRef(mod.entryPoint)

  def getDynSym(i: Int) =
    symbolTables(".dynsym")(i)

  /**
   * Symbol forwarding. Keys are "forwarding" symbols which are dynamically-bound to their associated value symbol.
   */
  val symbolForwarding =
    decodeAux(AuxKind.SymbolForwarding)(mod).map(SymbolRef(_) -> SymbolRef(_))

  /**
   * Inverse symbol forwarding.
   * Keys are "target" symbols.
   * At runtime, a target's associated symbols will point to the key symbol.
   */
  val symbolForwardingInverse =
    symbolForwarding.map(_.swap)

}
