package translating

import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.ByteInterval
import com.grammatech.gtirb.proto.Module.Module
import gtirb.*
import ir.*

import scala.collection.immutable

trait InsnLoader {
  def decodeBlock(blockUUID: String, blockAddress: Option[BigInt]): Seq[Seq[Statement]]
}

class ParserMapInsnLoader(mods: Seq[Module]) extends InsnLoader {
  val parserMap: immutable.Map[String, immutable.List[InsnSemantics]] = {
    val semanticsJson = mods.map(_.auxData("ast").data.toStringUtf8)
    val semantics = semanticsJson.map(upickle.default.read[immutable.Map[String, List[InsnSemantics]]](_))
    semantics.flatten.toMap
  }

  var blockCount = 0

  val semanticsLoader = GTIRBLoader(parserMap)
  def decodeBlock(uuid: String, addr: Option[BigInt]): Seq[Seq[Statement]] = {
    var s = semanticsLoader.visitBlock(uuid, blockCount, addr).toSeq
    blockCount += 1
    s
  }
}

class OfflineLifterInsnLoader(mods: Seq[Module]) extends InsnLoader {

  val opcodeBytes = 4

  case class BlockPos(
    b: ByteInterval.Block,
    uuid: String,
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

  def createOpcodeBlocks(): immutable.Map[String, BlockPos] = {
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
            b64encode(c.uuid) -> BlockPos(b, (b64encode(c.uuid)), bi_addr + b.offset, b.offset, c.size, cont)
        }
      )
    )
    codeblocks.toMap
  }

  private lazy val uuidToBlockContent: immutable.Map[String, BlockPos] = createOpcodeBlocks()

  def decodeBlock(blockUUID: String, blockAddress: Option[BigInt]) =
    uuidToBlockContent(blockUUID).toStatements()

}
