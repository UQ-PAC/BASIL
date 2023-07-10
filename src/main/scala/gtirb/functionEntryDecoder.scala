package gtirb
import java.io.FileInputStream
import com.google.protobuf.ByteString
import java.io.ByteArrayInputStream

/* Decodes functionEntries section in AuxData of gtirb */
class decoder(bytes: ByteString) {

    val bytesArr: Array[Byte] = bytes.toByteArray()
    val byteStream = new ByteArrayInputStream(bytesArr)

    /* see https://grammatech.github.io/gtirb/python/_modules/gtirb/serialization.html#MappingCodec.decode, this was pulled
    * from their python API, and converted into scala 
    */
    def decode(): collection.mutable.Map[ByteString, collection.mutable.Set[ByteString]]  = {

        val map : collection.mutable.Map[ByteString, collection.mutable.Set[ByteString]] 
        = collection.mutable.Map.empty[ByteString, collection.mutable.Set[ByteString]]

        val len = bytesToInt(read_bytes(false), true)
        val num = len.toInt
        for (s <- 0 until num) {
            val key = ByteString.copyFrom(read_bytes(true))
            val uuids: collection.mutable.Set[ByteString] = collection.mutable.Set[ByteString]();
            val len = bytesToInt(read_bytes(false), true);
            for (k <- 0 until len.toInt) {
                val byte = ByteString.copyFrom(read_bytes(true))
                uuids += byte
            }

            map += (key -> uuids);
        }
        return map
    }

    def read_bytes(uuid: Boolean): Array[Byte] = {
        if (uuid) {
            byteStream.readNBytes(16)
        } else {
            byteStream.readNBytes(8)
        }
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





