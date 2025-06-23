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
import com.grammatech.gtirb.proto.Section.Section
import com.grammatech.gtirb.proto.ByteInterval.Block
import com.grammatech.gtirb.proto.CodeBlock.CodeBlock
import com.grammatech.gtirb.proto.DataBlock.DataBlock
import com.grammatech.gtirb.proto.ByteInterval.ByteInterval
import com.grammatech.gtirb.proto.Symbol.Symbol.OptionalPayload

import scala.collection.mutable
import scala.collection.immutable.SortedMap

case class GTIRBResolver(mod: Module) {

  private def b64(bs: ByteString) =
    java.util.Base64.getEncoder().encodeToString(bs.toByteArray)

  sealed trait Uuid(val kind: String, val uuid: String) {
    override def toString = s"$kind:$uuid"
    override def equals(o: Any) = o match {
      case x: Uuid => x.kind == kind && x.uuid == uuid
      case _ => false
    }
    override def hashCode = (kind, uuid).hashCode
  }
  object Uuid {
    class Block(xs: ByteString) extends Uuid("blok", b64(xs))
    class Function(xs: ByteString) extends Uuid("func", b64(xs))
    class Symbol(xs: ByteString) extends Uuid("symb", b64(xs))
  }

  case class BlockData(inner: DataBlock | CodeBlock, block: Block, interval: ByteInterval, section: Section) {
    def uuid = inner match {
      case x: DataBlock => x.uuid
      case x: CodeBlock => x.uuid
    }
    def size = inner match {
      case x: DataBlock => x.size
      case x: CodeBlock => x.size
    }
    def address = block.offset + interval.address
  }

  extension (x: Uuid.Block)
    def get = blocksByUuid(x)
    def isProxyBlock = proxyBlockUuids.contains(x)

  extension (x: Uuid.Symbol)
    def get = symbolsByUuid(x)
    def symTabIdx = symbolTabIdxByUuid(x)
    def symKind = symbolKindsByUuid(x)
    def getReferentBlock = for {
      uuid <- x.get.optionalPayload.referentUuid
      blok <- blocksByUuid.get(Uuid.Block(uuid))
    } yield blok
    def getScalarValue = x.get.optionalPayload._value
    def getFunction = funcNamesInverse.get(x)

  extension (x: Uuid.Function)
    def getEntries = funcEntries(x)
    def getName = funcNames(x)

  private def mapFirst[T, T2, U](f: T => T2)(x: (T, U)) = (f(x._1), x._2)

  val proxyBlockUuids = mod.proxies.map(x => Uuid.Block(x.uuid)).toSet
  val symbolsByUuid = mod.symbols.map(x => Uuid.Symbol(x.uuid) -> x).toMap

  val blocksByUuid = (for {
    sec <- mod.sections.toList
    interval <- sec.byteIntervals
    (uuid, innerb, outerb) <- interval.blocks.collect {
      case b @ Block(_, Block.Value.Data(dat), _) => (dat.uuid, (dat: DataBlock | CodeBlock), b)
      case b @ Block(_, Block.Value.Code(cod), _) => (cod.uuid, (cod: DataBlock | CodeBlock), b)
    }
    id: Uuid.Block = Uuid.Block(uuid)
  } yield id -> BlockData(innerb, outerb, interval, sec)).toMap

  val sectionsByName = mod.sections.map(x => x.name -> x).toMap

  val symbolTabIdxByUuid =
    AuxDecoder.decodeAux(AuxDecoder.AuxKind.ElfSymbolTabIdxInfo)(mod).map(mapFirst(Uuid.Symbol(_)))
  val symbolTables = symbolTabIdxByUuid
    .flatMap { case (sym, idxs) =>
      idxs.map(_ -> sym)
    }
    .groupMapReduce(kv => kv.head.head)(kv => SortedMap(kv.head.last -> kv.last))(_ ++ _)

  val symbolKindsByUuid = decodeAux(AuxKind.ElfSymbolInfo)(mod)
    .map(mapFirst(Uuid.Symbol(_)))

  val funcNames = decodeAux(AuxKind.FunctionNames)(mod).map { case (fun, sym) =>
    Uuid.Function(fun) -> Uuid.Symbol(sym)
  }
  val funcNamesInverse = funcNames.map(_.swap)
  val funcEntries = decodeAux(AuxKind.FunctionEntries)(mod).map { case (a, b) =>
    Uuid.Function(a) -> b.map(Uuid.Block(_))
  }

}
