package gtirb
import java.io.FileInputStream
import com.google.protobuf.ByteString
import java.io.ByteArrayInputStream
import scala.collection.mutable
import java.nio.charset.StandardCharsets



/* 
* Provides some useful decoders for certian AuxData sections in gtirb.
*
* See https://grammatech.github.io/gtirb/python/_modules/gtirb/serialization.html#MappingCodec.decode, this was pulled
* from their python API, and converted into scala 
*/
object MapDecoder {

    def decode_set(totalBytes: Seq[ByteString]): collection.mutable.Map[ByteString, collection.mutable.Set[ByteString]]  = {
        
        val totalMap: collection.mutable.Map[ByteString, collection.mutable.Set[ByteString]] 
        = collection.mutable.Map.empty[ByteString, collection.mutable.Set[ByteString]]
        
        for (bytes <- totalBytes) {
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
            totalMap ++= map
        }
        return totalMap
        
    }

    def decode_uuid(totalBytes: Seq[ByteString]): collection.mutable.Map[ByteString, ByteString]  = {

        val totalMap : collection.mutable.Map[ByteString, ByteString] 
        = collection.mutable.Map.empty[ByteString, ByteString]
        
        for (bytes <- totalBytes) {
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
            totalMap ++= map
        }
        return totalMap
    }

    def decode_string(totalBytes: Seq[ByteString]): mutable.Map[ByteString, String] = {
        // THIS DOESNT WORK YET
        // literally can't figure out what's wrong, might be something to do with java/scala treating all bits as signed, 
        // when api is unsigned
        val totalMap : mutable.Map[ByteString, String] = mutable.Map.empty[ByteString, String]

        for (bytes <- totalBytes) {

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
            totalMap ++= map 
        } 
        return totalMap
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
