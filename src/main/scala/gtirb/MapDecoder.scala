package gtirb
import java.io.FileInputStream
import com.google.protobuf.ByteString
import java.io.ByteArrayInputStream
import scala.collection.mutable
import java.nio.charset.StandardCharsets



/* Collection of previous decoders, see https://grammatech.github.io/gtirb/python/_modules/gtirb/serialization.html#MappingCodec.decode, this was pulled
* from their python API, and converted into scala 
*/
object MapDecoder {

    def decode_set(bytes: ByteString): collection.mutable.Map[ByteString, collection.mutable.Set[ByteString]]  = {

        val bytesArr: Array[Byte] = bytes.toByteArray()
        val byteStream = new ByteArrayInputStream(bytesArr)

        val map : collection.mutable.Map[ByteString, collection.mutable.Set[ByteString]] 
        = collection.mutable.Map.empty[ByteString, collection.mutable.Set[ByteString]]

        val len = bytesToInt(read_bytes(8, byteStream), true)
        val num = len.toInt
        for (s <- 0 until num) {
            val key = ByteString.copyFrom(read_bytes(16, byteStream))
            val uuids: collection.mutable.Set[ByteString] = collection.mutable.Set[ByteString]();
            val len = bytesToInt(read_bytes(8, byteStream), true);
            for (k <- 0 until len.toInt) {
                val byte = ByteString.copyFrom(read_bytes(16, byteStream))
                uuids += byte
            }

            map += (key -> uuids);
        }
        return map
    }

    def decode_uuid(bytes: ByteString): collection.mutable.Map[ByteString, ByteString]  = {

        val bytesArr: Array[Byte] = bytes.toByteArray()
        val byteStream = new ByteArrayInputStream(bytesArr)

        val map : collection.mutable.Map[ByteString, ByteString] 
        = collection.mutable.Map.empty[ByteString, ByteString]

        val len = bytesToInt(read_bytes(8, byteStream), true)
        val num = len.toInt
        for (s <- 0 until num) {
            val key = ByteString.copyFrom(read_bytes(16, byteStream))
            val uuid = ByteString.copyFrom(read_bytes(16, byteStream))
            map += (key -> uuid);
        }
        return map
    }

    def decode_string(bytes: ByteString): mutable.Map[ByteString, String] = {
        // THIS DOESNT WORK YET
        // literally can't figure out what's wrong, might be something to do with java/scala treating all bits as signed, 
        // when api is unsigned

        val map : mutable.Map[ByteString, String] = mutable.Map.empty[ByteString, String]
        val bytesArr: Array[Byte] = bytes.toByteArray()
        val byteStream = new ByteArrayInputStream(bytesArr)

        val len = bytesToInt(read_bytes(8, byteStream), true)
        val num = len.toInt
        for (s <- 0 until num) {
            val key = ByteString.copyFrom(read_bytes(16, byteStream))
            val len = bytesToInt(read_bytes(8, byteStream), true);
            val str = new String(read_bytes(len.toInt, byteStream), StandardCharsets.UTF_8)
            map += (key -> str);
        }
        return map
    }


    def read_bytes(size: Int, byteStream: ByteArrayInputStream): Array[Byte] = {
        byteStream.readNBytes(size)
    }

    def bytesToInt(bytes: Array[Byte], littleEndian: Boolean): Long = {
        val buffer = java.nio.ByteBuffer.wrap(bytes)
        if (littleEndian) {
            return buffer.order(java.nio.ByteOrder.LITTLE_ENDIAN).getLong
        } else {
            return buffer.getLong
        }
    }
  
}
