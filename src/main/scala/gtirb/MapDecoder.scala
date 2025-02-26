package gtirb
import java.io.FileInputStream
import com.google.protobuf.ByteString
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

/*
 * Provides some useful decoders for certian AuxData sections in gtirb.
 *
 * See https://grammatech.github.io/gtirb/python/_modules/gtirb/serialization.html#MappingCodec.decode, this was pulled
 * from their python API, and converted into scala
 */
object MapDecoder {
  def decode_set(totalBytes: Seq[ByteString]): Map[ByteString, Set[ByteString]] = {
    val totalMap: Map[ByteString, Set[ByteString]] = (for {
      bytes <- totalBytes
      byteStream = ByteArrayInputStream(bytes.toByteArray)
      len = bytesToLong(read_bytes(8, byteStream), true)
      s <- 0L until len
    } yield {
      val key = ByteString.copyFrom(read_bytes(16, byteStream))
      val len2 = bytesToLong(read_bytes(8, byteStream), true)
      val uuids = (for (k <- 0L until len2) yield { // should maybe check this
        ByteString.copyFrom(read_bytes(16, byteStream))
      }).toSet
      key -> uuids
    }).toMap
    totalMap
  }

  def decode_uuid(totalBytes: Seq[ByteString]): Map[ByteString, ByteString] = {
    val totalMap: Map[ByteString, ByteString] = (for {
      bytes <- totalBytes
      byteStream = ByteArrayInputStream(bytes.toByteArray)
      len = bytesToLong(read_bytes(8, byteStream), true)
      s <- 0L until len
    } yield {
      val key = ByteString.copyFrom(read_bytes(16, byteStream))
      val uuid = ByteString.copyFrom(read_bytes(16, byteStream))
      key -> uuid
    }).toMap
    totalMap
  }

  def read_bytes(size: Int, byteStream: ByteArrayInputStream): Array[Byte] = {
    byteStream.readNBytes(size)
  }

  def bytesToLong(bytes: Array[Byte], littleEndian: Boolean): Long = {
    val buffer = java.nio.ByteBuffer.wrap(bytes)
    if (littleEndian) {
      buffer.order(java.nio.ByteOrder.LITTLE_ENDIAN).getLong
    } else {
      buffer.getLong
    }
  }

}
