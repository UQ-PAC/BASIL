package gtirb

import java.util.Base64
import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.AuxData.AuxData

object AuxDecoder {

  type Input = ByteArrayInputStream
  type Reader[T] = Input => T

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

    readBytes(numBytes)(bs).foldRight(BigInt(0)) { case (x, acc) =>
      val n = x.toInt
      acc * 256 + (if (!signed && n < 0) then n + 256 else n)
    }

  def readMap[K, V](keyReader: Reader[K], valReader: Reader[V])(bs: Input) =
    val len = readUint(64)(bs)
    (BigInt(0) until len).map { case _ =>
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

  def readTuple[T1, T2](r1: Reader[T1], r2: Reader[T2])(bs: Input) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    (x1, x2)

  def readTuple[T1, T2, T3](r1: Reader[T1], r2: Reader[T2], r3: Reader[T3])(bs: Input) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    val x3 = r3(bs)
    (x1, x2, x3)


  def readTuple[T1, T2, T3, T4](r1: Reader[T1], r2: Reader[T2], r3: Reader[T3], r4: Reader[T4])(bs: Input) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    val x3 = r3(bs)
    val x4 = r4(bs)
    (x1, x2, x3, x4)

  def readTuple[T1, T2, T3, T4, T5](r1: Reader[T1], r2: Reader[T2], r3: Reader[T3], r4: Reader[T4], r5: Reader[T5])(
    bs: Input
  ) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    val x3 = r3(bs)
    val x4 = r4(bs)
    val x5 = r5(bs)
    (x1, x2, x3, x4, x5)

  def readTuple[T1, T2, T3, T4, T5, T6](r1: Reader[T1], r2: Reader[T2], r3: Reader[T3], r4: Reader[T4], r5: Reader[T5], r6: Reader[T6])(
    bs: Input
  ) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    val x3 = r3(bs)
    val x4 = r4(bs)
    val x5 = r5(bs)
    val x6 = r6(bs)
    (x1, x2, x3, x4, x5, x6)


  def readUuid(bs: Input) =
    ByteString.copyFrom(readBytes(16)(bs))
    // Base64.getEncoder().encodeToString(readBytes(16)(bs))

  def readOffset(bs: Input) =
    val uuid = readUuid(bs)
    val len = readUint(64)(bs)
    (uuid, len)

}
