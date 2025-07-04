package translating

import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.CFG.EdgeType.*
import com.grammatech.gtirb.proto.CFG.CFG
import com.grammatech.gtirb.proto.CFG.Edge
import com.grammatech.gtirb.proto.CFG.EdgeLabel
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.ByteInterval
import com.grammatech.gtirb.proto.Symbol.Symbol
import Parsers.ASLpParser.*
import gtirb.*
import ir.*

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Success, Failure}
import java.util.Base64
import java.nio.charset.*
import scala.util.boundary
import boundary.break
import java.nio.ByteBuffer
import util.intrusive_list.*
import util.functional.{Snoc}
import util.Logger

trait InsnLoader {
  def decodeBlock(blockUUID: ByteString, blockCountIn: Int, blockAddress: Option[BigInt]): Seq[Seq[Statement]]
}

class ParserMapInsnLoader(mods: Seq[Module]) extends InsnLoader {
  val parserMap: immutable.Map[String, immutable.List[InsnSemantics]] = {
    val semanticsJson = mods.map(_.auxData("ast").data.toStringUtf8)
    val semantics = semanticsJson.map(upickle.default.read[immutable.Map[String, List[InsnSemantics]]](_))
    semantics.flatten.toMap
  }

  val semanticsLoader = GTIRBLoader(parserMap)
  def decodeBlock(uuid: ByteString, blockCount: Int, addr: Option[BigInt]): Seq[Seq[Statement]] =
    semanticsLoader.visitBlock(uuid, blockCount, addr).toSeq
}

class OfflineLifterInsnLoader(mods: Seq[Module]) extends InsnLoader {

  val opcodeBytes = 4

  case class BlockPos(
    b: ByteInterval.Block,
    uuid: ByteString,
    address: Long,
    offset: Long,
    size: Long,
    byteIntervalContent: ByteString
  ) {

    def content: ByteString = {
      byteIntervalContent.substring(offset.toInt, (offset + size).toInt)
    }

    def opcodes: Seq[Int] = Range
      .Exclusive(0, size.toInt, opcodeBytes)
      .map(i => content.substring(i, i + opcodeBytes))
      .map(b => bytesToi32(b.toByteArray, true))

    def toStatements(): Seq[Seq[Statement]] = {
      offlineLifter.Lifter.liftBlockBytes(opcodes, address)
    }

    private def bytesToi32(bytes: Array[Byte], littleEndian: Boolean): Int = {
      val buffer = java.nio.ByteBuffer.wrap(bytes)
      if (littleEndian) {
        buffer.order(java.nio.ByteOrder.LITTLE_ENDIAN).getInt
      } else {
        buffer.getInt
      }
    }

  }

  def createOpcodeBlocks(): immutable.Map[ByteString, BlockPos] = {
    val blocks = mods
      .flatMap(_.sections)
      .flatMap(_.byteIntervals)
      .map(bi => {
        assert(
          bi.hasAddress
        ) // Unsure if this holds for PIC https://grammatech.github.io/gtirb/cpp/classgtirb_1_1_byte_interval.html#aaf13ececea7d2b943402feeb4f1aae35
        (bi.blocks.toList, bi.contents, bi.address)
      })

    val codeblocks = blocks.flatMap((bl, cont, bi_addr) =>
      bl.collect(b =>
        b.value.code match {
          case Some(c) =>
            c.uuid -> BlockPos(b, c.uuid, bi_addr + b.offset, b.offset, c.size, cont)
        }
      )
    )
    codeblocks.toMap
  }

  private lazy val uuidToBlockContent: immutable.Map[ByteString, BlockPos] = createOpcodeBlocks()

  def decodeBlock(blockUUID: ByteString, blockCountIn: Int, blockAddress: Option[BigInt]) =
    uuidToBlockContent(blockUUID).toStatements()

}
