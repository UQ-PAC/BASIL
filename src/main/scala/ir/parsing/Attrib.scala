package ir.parsing

import boogie.SpecGlobal
import ir.{BitVecLiteral, FalseLiteral, IntLiteral, Literal, MemorySection, Sigil, TrueLiteral}
import specification.{ExternalFunction, FuncEntry}
import util.Logger
import util.assertion.*

import java.io.*
import java.util.Base64
import java.util.zip.*
import scala.collection.immutable.ListMap

enum Attrib {
  /*
   * Representation for json-style data in Basil IL language, used for storing static data and annotationg procedures.
   */
  case List(l: Vector[Attrib])
  case Map(l: ListMap[String, Attrib])
  case ValLiteral(l: ir.Literal)
  case ValString(l: String)

  def Str: Option[String] = this match {
    case Attrib.ValString(s) => Some(s)
    case _ => None
  }

  def Int: Option[BigInt] = this match {
    case Attrib.ValLiteral(IntLiteral(l)) => Some(l)
    case Attrib.ValLiteral(BitVecLiteral(l, _)) => Some(l)
    case _ => None
  }
  def Bool: Option[Boolean] = this match {
    case Attrib.ValLiteral(TrueLiteral) => Some(true)
    case Attrib.ValLiteral(FalseLiteral) => Some(false)
    case _ => None
  }

  def Map: Option[ListMap[String, Attrib]] = this match {
    case Attrib.Map(l) => Some(l)
    case _ => None
  }

  def List: Option[Vector[Attrib]] = this match {
    case Attrib.List(l) => Some(l)
    case _ => None
  }

  // TODO: distinguish allowable failures (e.g., missing optional key)
  // from unacceptable ones (e.g., wrong type for a certain known key like address)

  def get(k: String) = this.Map.flatMap(_.get(k))
  def getInt(k: String) = this.Map
    .flatMap(_.get(k))
    .map(_.Int.getOrElse {
      throw Exception(s"integer-like attribute value required for '$k'")
    })
  def getString(k: String) = this.Map
    .flatMap(_.get(k))
    .map(_.Str.getOrElse {
      throw Exception(s"string attribute value required for '$k'")
    })

  import translating.indent

  def field(p: String) = {
    Sigil.BASIR.attrib + p
  }

  import util.StringEscape.quote

  private val lineLim = 80

  def pprint: String = {
    this match {
      case Attrib.List(xs) => {
        val res = xs.map(_.pprint)
        val tlength = res.map(_.size).sum
        if (tlength > lineLim) {
          "[\n  " + indent(res.mkString(";\n"), "  ") + "\n]"
        } else {
          "[ " + res.mkString("; ") + " ]"
        }
      }
      case Attrib.Map(xs) => {
        val keyvals = xs.map { case (f, v) =>
          field(f) + " = " + v.pprint
        }
        if (keyvals.map(_.size).sum > lineLim) {
          "{\n  " + indent(keyvals.mkString(";\n"), "  ") + "\n}"
        } else {
          "{ " + keyvals.mkString("; ") + " }"
        }
      }
      case Attrib.ValLiteral(l) => translating.PrettyPrinter.pp_expr(l)
      case Attrib.ValString(l) => quote(l)
    }
  }
}

/* constructors for attrib values */
object Attrib {
  object Int {
    def apply(i: BigInt) = Attrib.ValLiteral(IntLiteral(i))
    def unapply(x: Attrib) = x.Int
  }
  object Str {
    def apply(i: String) = Attrib.ValString(i)
    def unapply(x: Attrib) = x.Str
  }
  object Bool {
    def apply(i: Boolean) = Attrib.ValLiteral(if i then TrueLiteral else FalseLiteral)
    def unapply(x: Attrib) = x.Bool
  }
  case class MapOf(keys: String*) {
    def unapply(x: Attrib): Option[scala.List[Attrib]] = x.Map match {
      case Some(map) if map.keySet.forall(keys.contains) =>
        val values = keys.map(map.get(_)).toList
        util.functional.sequence(values)
      case _ => None
    }
  }
}

/*
 * Translation of IRContext / Spec structures to and from Attrib
 */

extension (p: FuncEntry)
  def toAttrib =
    Attrib.Map(ListMap("name" -> Attrib.Str(p.name), "address" -> Attrib.Int(p.address), "size" -> Attrib.Int(p.size)))

extension (p: SpecGlobal) {
  def toAttrib: Attrib = {
    Attrib.Map(
      ListMap(
        "name" -> Attrib.Str(p.name),
        "address" -> Attrib.Int(p.address),
        "size" -> Attrib.Int(p.size)
      ) ++ p.arraySize.map(s => "arraySize" -> Attrib.Int(s))
    )
  }
}

def funcEntryFromAttrib(a: Attrib) =
  val FuncEntryMap = Attrib.MapOf("name", "address", "size")
  a match {
    case FuncEntryMap(List(Attrib.Str(name), Attrib.Int(address), Attrib.Int(size))) =>
      Some(FuncEntry(name, size.toInt, address))
    case _ => None
  }

def specGlobalFromAttrib(a: Attrib) = {
  for {
    l <- a.Map
    name <- l.get("name").flatMap(_.Str)
    address <- l.get("address").flatMap(_.Int)
    size <- l.get("size").flatMap(_.Int)
    arraySize = l.get("arraySize").flatMap(_.Int).map(_.toInt)
  } yield (SpecGlobal(name, size.toInt, arraySize, address))
}

extension (p: ExternalFunction) {
  def toAttrib: Attrib =
    Attrib.Map(ListMap("name" -> Attrib.Str(p.name), "offset" -> Attrib.Int(p.offset)))
}

def externalFunctionFromAttrib(a: Attrib) = {
  for {
    l <- a.Map
    name <- l.get("name").flatMap(_.Str)
    off <- l.get("offset").flatMap(_.Int)
  } yield (ExternalFunction(name, off))
}

/*
 * The subset of IRContext that we preserve through serialisation.
 */
case class SymbolTableInfo(
  externalFunctions: Set[ExternalFunction] = Set(),
  globals: Set[SpecGlobal] = Set(),
  funcEntries: Set[FuncEntry] = Set(),
  globalOffsets: Map[BigInt, BigInt] = Map()
) {

  def merge(o: SymbolTableInfo) = {
    SymbolTableInfo(
      externalFunctions ++ o.externalFunctions,
      globals ++ o.globals,
      funcEntries ++ o.funcEntries,
      globalOffsets ++ o.globalOffsets
    )
  }

  def toAttrib = {

    val goffs = Attrib.List(globalOffsets.toVector.sorted.map { case (l, r) =>
      Attrib.List(Vector(Attrib.Int(l), Attrib.Int(r)))
    })

    Attrib.Map(
      ListMap(
        "externalFunctions" -> Attrib.List(externalFunctions.toVector.sorted.map(_.toAttrib)),
        "globals" -> Attrib.List(globals.toVector.sorted.map(_.toAttrib)),
        "funcEntries" -> Attrib.List(funcEntries.toVector.sorted.map(_.toAttrib)),
        "globalOffsets" -> goffs
      )
    )
  }

  def mergeFromAttrib(a: Attrib) =
    SymbolTableInfo.fromAttrib(a).map(this.merge(_))

}

object SymbolTableInfo {
  def from(e: util.IRContext) = {
    SymbolTableInfo(e.externalFunctions, e.globals, e.funcEntries, e.globalOffsets)
  }

  def empty = SymbolTableInfo(Set(), Set(), Set(), Map())

  def fromAttrib(a: Attrib) = {
    import scala.util.chaining.scalaUtilChainingOps

    def logIfNone[T](str: => String)(x: Option[T]) = x match {
      case Some(x) => x
      case None =>
        Logger.error(str)
        None
    }

    for {
      l <- a.Map
      externalFunctionsList <- l.get("externalFunctions").flatMap(_.List)
      externalFunctions = externalFunctionsList
        .flatMap(e => externalFunctionFromAttrib(e).tap(logIfNone(s"Malformed external funcion: ${e.pprint}")))
        .toSet
      globalsList <- l.get("globals").flatMap(_.List)
      globals = globalsList
        .flatMap(g => specGlobalFromAttrib(g).tap(logIfNone(s"Malformed specglobal: ${g.pprint}")))
        .toSet
      funcEntriesList <- l.get("funcEntries").flatMap(_.List)
      funcEntries = funcEntriesList
        .flatMap(g => funcEntryFromAttrib(g).tap(logIfNone(s"Malformed FuncEntry: ${g.pprint}")))
        .toSet
      globalOffsetsList <- l.get("globalOffsets").flatMap(_.List)
      globalOffsets = globalOffsetsList
        .map(e =>
          e.List match {
            case Some(Seq(Attrib.Int(a), Attrib.Int(b))) => (a, b)
            case _ => throw Exception(s"Malformed global offsets ${globalOffsetsList}")
          }
        )
        .toMap
    } yield SymbolTableInfo(externalFunctions, globals, funcEntries, globalOffsets)
  }
}

/**
 * Memory data loaded from the IL attributes. Basically, InitialMemory minus the merged regions graph
 *
 * This stores [[bytes]] as a Base64-encoded gzip-compressed string containing the original bytes.
 * The goal of compression is just to redcue the representatino length of large sections of zeros and is more standard than a custom RLE or compression
 * algorithm implementation. It conveniently allows all byte strings to be stored the same way.
 */
case class MemoryAttribData(name: String, address: BigInt, size: Int, readOnly: Boolean, bytes: String) {

  def toMemorySection: MemorySection = {
    val decoded: Array[Byte] = {
      val dc = Base64.getDecoder()
      val by = dc.decode(bytes)

      if (MemoryAttribData.compress) {
        val f = ByteArrayInputStream(by)
        GZIPInputStream(f).readAllBytes()
      } else {
        by
      }
    }
    val bvbytes = decoded.map(b => BitVecLiteral(BigInt(b), 8)).toSeq
    MemorySection(name, address, size, bvbytes, readOnly, None)
  }

  def toAttrib: Attrib = {
    val byg = Attrib.List(bytes.grouped(MemoryAttribData.groupSize).map(Attrib.Str(_)).toVector)
    Attrib.Map(
      ListMap(
        ("name") -> Attrib.Str(name),
        ("address") -> Attrib.Int(address),
        ("size") -> Attrib.Int(size),
        ("readOnly") -> Attrib.Bool(readOnly),
        ("bytes") -> byg
      )
    )
  }
}

case object MemoryAttribData {

  val groupSize = 96
  val compress = true

  def of(m: MemorySection) = {
    val bytes = Array.from(m.bytes.map { case BitVecLiteral(v, 8) =>
      v.toByte
    })

    val comprBytes = if (compress) then {

      val f = ByteArrayOutputStream()
      val x = GZIPOutputStream(f)
      x.write(bytes)
      x.close()

      val r = f.toByteArray()
      r
    } else {
      bytes
    }

    val ec = Base64.getEncoder()
    val b64bytes = ec.encodeToString(comprBytes)

    val decoded: Array[Byte] = {
      val dc = Base64.getDecoder()
      val by = dc.decode(b64bytes)

      if (compress) {
        val f = ByteArrayInputStream(by)
        GZIPInputStream(f).readAllBytes()
      } else {
        by
      }
    }
    debugAssert(decoded.toList == bytes.toList)

    MemoryAttribData(m.name, m.address, m.size, m.readOnly, b64bytes)
  }

  def fromAttrib(a: Attrib) = {
    for {
      l <- a.Map
      name <- l.get("name").flatMap(_.Str)
      address <- l.get("address").flatMap(_.Int)
      size <- l.get("size").flatMap(_.Int)
      readOnly <- l.get("readOnly").flatMap(_.Bool)
      bytes <- for {
        b <- l.get("bytes")
        bl <- b.List
        strs = bl.map(_.Str.get).mkString("")
      } yield (strs)
    } yield (MemoryAttribData(name, address, size.toInt, readOnly, bytes))
  }

}
