package gtirb
import java.io.FileInputStream
import com.google.protobuf.ByteString
import java.io.ByteArrayInputStream
import scala.collection.mutable
import java.nio.charset.StandardCharsets

class StringDecoder(bytes: ByteString) {
    // literally can't figure out what's wrong, might be something to do with java/scala treating all bits as signed, 
    // when api is unsigned
    val bytesArr: Array[Byte] = bytes.toByteArray()
    val byteStream = new ByteArrayInputStream(bytesArr)

    def decode(): mutable.Map[ByteString, String] = {

        val map : mutable.Map[ByteString, String] = mutable.Map.empty[ByteString, String]

        val len = bytesToInt(read_bytes(8), true)
        val num = len.toInt
        for (s <- 0 until num) {
            val key = ByteString.copyFrom(read_bytes(16))
            val len = bytesToInt(read_bytes(8), true);
            val str = new String(read_bytes(len.toInt), StandardCharsets.UTF_8)
            map += (key -> str);
        }
        return map
    }

    def read_bytes(size: Int): Array[Byte] = {
        return byteStream.readNBytes(size)
    }

    def bytesToInt(bytes: Array[Byte], littleEndian: Boolean): Long = {
        val buffer = java.nio.ByteBuffer.wrap(bytes)
        if (littleEndian) {
            return buffer.order(java.nio.ByteOrder.LITTLE_ENDIAN).getLong()
        } else {
            return buffer.getLong()
        }
    }
}





