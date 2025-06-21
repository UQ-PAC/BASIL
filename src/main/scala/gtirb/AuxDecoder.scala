package gtirb

import java.util.Base64
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.AuxData.AuxData
import com.grammatech.gtirb.proto.Module.Module

object AuxDecoder {

  enum AuxKind[T](val name: String, val decoder: Reader[T]) {
    case ElfSymbolTabIdxInfo
        extends AuxKind("elfSymbolTabIdxInfo", readMap(readUuid, readList(readTuple(readString, readUint(64)))))
    case ElfSymbolInfo
        extends AuxKind(
          "elfSymbolInfo",
          readMap(readUuid, readTuple(readUint(64), readString, readString, readString, readUint(64)))
        )
    case FunctionEntries extends AuxKind("functionEntries", readMap(readUuid, readSet(readUuid)))
    case FunctionBlocks extends AuxKind("functionBlocks", readMap(readUuid, readSet(readUuid)))
    case FunctionNames extends AuxKind("functionNames", readMap(readUuid, readUuid))
  }

  type Input = ByteArrayInputStream
  type Reader[T] = Input => T

  def decodeAux[T](known: AuxKind[T])(mod: Module) =
    decode(known.decoder)(mod.auxData(known.name))

  def decode[T](reader: Reader[T])(bytes: ByteString): T =
    reader(ByteArrayInputStream(bytes.toByteArray))

  def decode[T](reader: Reader[T])(aux: AuxData): T =
    reader(ByteArrayInputStream(aux.data.toByteArray))

  def readBytes(numBytes: Int)(bs: Input): Array[Byte] =
    val bytes = bs.readNBytes(numBytes)
    assert(bytes.length == numBytes, s"insufficient bytes to read. got ${bytes.length} but wanted $numBytes")
    bytes

  def readBool(bs: Input) =
    readUint(8)(bs) != 0

  def readUint(numBits: Int) = readInt(numBits, false)

  def readString(bs: Input) =
    val len = readUint(64)(bs)
    assert(len <= Int.MaxValue, "string length out of int32 range")
    new String(readBytes(len.toInt)(bs), StandardCharsets.UTF_8)

  def readInt(numBits: Int, signed: Boolean = false)(bs: Input) =
    val numBytes = numBits / 8
    require(numBytes * 8 == numBits, "requires multiple of 8")

    readBytes(numBytes)(bs).foldRight(BigInt(0)) {
      case (x, acc) =>
        val n = x.toInt
        acc * 256 + (if (!signed && n < 0) then n + 256 else n)
    }

  def readMap[K, V](keyReader: Reader[K], valReader: Reader[V])(bs: Input) =
    val len = readUint(64)(bs)
    (BigInt(0) until len).map {
      case _ =>
        val k = keyReader(bs)
        val v = valReader(bs)
        k -> v
    }.toMap

  def readSet[K, V](valReader: Reader[V])(bs: Input) =
    val len = readUint(64)(bs)
    (BigInt(0) until len).map(_ => valReader(bs)).toSet

  def readList[K, V](valReader: Reader[V])(bs: Input) =
    val len = readUint(64)(bs)
    (BigInt(0) until len).map(_ => valReader(bs)).toList

  type Return[x] = x match { case Reader[t] => t }

  inline def readTuple[T <: Tuple](xs: T)(bs: Input): Tuple.Map[T, Return] =
    readTupleInner(xs)(bs).asInstanceOf[Tuple.Map[T, Return]]

  private inline def readTupleInner[T <: Tuple](xs: T)(bs: Input): Tuple =
    inline xs match
      case xs: (Reader[_] *: _) => xs.head(bs) *: readTupleInner(xs.tail)(bs)
      case EmptyTuple => EmptyTuple

  def readUuid(bs: Input) =
    ByteString.copyFrom(readBytes(16)(bs))
    // Base64.getEncoder().encodeToString(readBytes(16)(bs))

  def readOffset(bs: Input) =
    val uuid = readUuid(bs)
    val len = readUint(64)(bs)
    (uuid, len)

}
