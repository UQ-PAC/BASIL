package gtirb

import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.AuxData.AuxData
import com.grammatech.gtirb.proto.Module.Module

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.util.Base64

/**
 * Provides methods for decoding binary data, particularly the binary encoding of
 * GTIRB's [AuxData](https://grammatech.github.io/gtirb/md__aux_data.html).
 * The encoding format is inferred by reverse-engineering the
 * [Python implementation](https://grammatech.github.io/gtirb/python/_modules/gtirb/serialization.html#SetCodec).
 *
 * The various `read*` methods have some pre-defined decoders for common types.
 * For parametrised decoders, their `read*` methods require a decoder for the inner type as an argument.
 * The read methods return [[Decoder]] values which can be passed to the [[decode]] methods.
 *
 * [[AuxKind]] provides pre-defined decoders for some official AuxData fields. An [[AuxKind]] can be
 * passed to [[decodeAux]] to automatically extract and decode the given AuxData from a GTIRB
 * [[com.grammatech.gtirb.proto.Module.Module]].
 *
 * Within a [[Decoder]], the internal state of the [[java.io.ByteArrayInputStream]] is used to keep
 * track of the current byte position.
 */
object AuxDecoder {

  /**
   * [[AuxKind]] provides pre-defined decoders for some official AuxData fields. An [[AuxKind]] can be
   * passed to [[decodeAux]] to automatically extract and decode the given AuxData from a GTIRB [[com.grammatech.gtirb.proto.Module.Module]].
   * See the [Standard AuxData Schemata](https://grammatech.github.io/gtirb/md__aux_data.html) for a list of official AuxData fields
   * and their types.
   */
  enum AuxKind[T](val name: String, val decoder: Decoder[T]) {
    case ElfSymbolTabIdxInfo
        extends AuxKind("elfSymbolTabIdxInfo", readMap(readUuid, readList(readTuple(readString, readUint(64)))))
    case ElfSymbolInfo
        extends AuxKind(
          "elfSymbolInfo",
          readMap(readUuid, readTuple(readUint(64), readString, readString, readString, readUint(64)))
        )
    case SectionProperties
        extends AuxKind("sectionProperties", readMap(readUuid, readTuple(readUint(64), readUint(64))))
    case FunctionEntries extends AuxKind("functionEntries", readMap(readUuid, readSet(readUuid)))
    case FunctionBlocks extends AuxKind("functionBlocks", readMap(readUuid, readSet(readUuid)))
    case FunctionNames extends AuxKind("functionNames", readMap(readUuid, readUuid))
    case SymbolForwarding extends AuxKind("symbolForwarding", readMap(readUuid, readUuid))
  }

  type Input = ByteArrayInputStream
  type Decoder[T] = ByteArrayInputStream => T

  def decodeAux[T](known: AuxKind[T])(mod: Module) =
    decode(known.decoder)(mod.auxData(known.name))

  def decode[T](decoder: Decoder[T])(bytes: ByteString): T =
    decoder(ByteArrayInputStream(bytes.toByteArray))

  def decode[T](decoder: Decoder[T])(aux: AuxData): T =
    decoder(ByteArrayInputStream(aux.data.toByteArray))

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

  def readMap[K, V](keyDecoder: Decoder[K], valDecoder: Decoder[V])(bs: Input) =
    val len = readUint(64)(bs)
    (BigInt(0) until len).map { case _ =>
      val k = keyDecoder(bs)
      val v = valDecoder(bs)
      k -> v
    }.toMap

  def readSet[K, V](valDecoder: Decoder[V])(bs: Input) =
    val len = readUint(64)(bs)
    (BigInt(0) until len).map(_ => valDecoder(bs)).toSet

  def readList[K, V](valDecoder: Decoder[V])(bs: Input) =
    val len = readUint(64)(bs)
    (BigInt(0) until len).map(_ => valDecoder(bs)).toList

  def readTuple[T1, T2](r1: Decoder[T1], r2: Decoder[T2])(bs: Input) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    (x1, x2)

  def readTuple[T1, T2, T3](r1: Decoder[T1], r2: Decoder[T2], r3: Decoder[T3])(bs: Input) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    val x3 = r3(bs)
    (x1, x2, x3)

  def readTuple[T1, T2, T3, T4](r1: Decoder[T1], r2: Decoder[T2], r3: Decoder[T3], r4: Decoder[T4])(bs: Input) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    val x3 = r3(bs)
    val x4 = r4(bs)
    (x1, x2, x3, x4)

  def readTuple[T1, T2, T3, T4, T5](
    r1: Decoder[T1],
    r2: Decoder[T2],
    r3: Decoder[T3],
    r4: Decoder[T4],
    r5: Decoder[T5]
  )(bs: Input) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    val x3 = r3(bs)
    val x4 = r4(bs)
    val x5 = r5(bs)
    (x1, x2, x3, x4, x5)

  def readTuple[T1, T2, T3, T4, T5, T6](
    r1: Decoder[T1],
    r2: Decoder[T2],
    r3: Decoder[T3],
    r4: Decoder[T4],
    r5: Decoder[T5],
    r6: Decoder[T6]
  )(bs: Input) =
    val x1 = r1(bs)
    val x2 = r2(bs)
    val x3 = r3(bs)
    val x4 = r4(bs)
    val x5 = r5(bs)
    val x6 = r6(bs)
    (x1, x2, x3, x4, x5, x6)

  // type ReadTuple[T <: Tuple] <: Tuple = T match
  //   case Reader[out] *: rest => out *: ReadTuple[rest]
  //   case EmptyTuple => EmptyTuple
  //
  // inline def readTuple[T <: Tuple](xs: T)(bs: Input): ReadTuple[T] =
  //   inline xs match
  //     case xs: (Reader[o] *: rest) =>
  //       xs match
  //         case h *: t => h(bs) *: readTuple[rest](t)(bs)
  //     case _: EmptyTuple => EmptyTuple

  def readUuid(bs: Input) =
    // ByteString.copyFrom(readBytes(16)(bs))
    Base64.getEncoder().encodeToString(readBytes(16)(bs))

  def readOffset(bs: Input) =
    val uuid = readUuid(bs)
    val len = readUint(64)(bs)
    (uuid, len)

}
