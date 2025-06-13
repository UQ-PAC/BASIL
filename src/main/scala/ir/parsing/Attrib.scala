package ir.parsing

import boogie.SpecGlobal
import specification.{FuncEntry, ExternalFunction}
import scala.collection.immutable.ListMap
import util.Logger
import ir.{Literal, BitVecLiteral, TrueLiteral, FalseLiteral, IntLiteral, MemorySection, Sigil}
import java.util.Base64
import java.io._
import java.util.zip._

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

  import translating.indent

  def field(p: String) = {
    Sigil.BASIR.attrib + p
  }

  def quote(s: String) = "\"" + s + "\""

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
  def Int(i: BigInt) = Attrib.ValLiteral(IntLiteral(i))
  def Str(i: String) = Attrib.ValString(i)
  def Bool(i: Boolean) = Attrib.ValLiteral(if i then TrueLiteral else FalseLiteral)
}

case class FunDecl(irType: ir.IRType, body: Option[ir.LambdaExpr])
case class ProgSpec(val rely: List[ir.Expr] = List(), val guar: List[ir.Expr] = List()) {
  def merge(o: ProgSpec) = {
    ProgSpec(rely ++ o.rely, guar ++ o.guar)
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

def funcEntryFromAttrib(a: Attrib) = {
  for {
    l <- a.Map
    name <- l.get("name").flatMap(_.Str)
    address <- l.get("address").flatMap(_.Int)
    size <- l.get("size").flatMap(_.Int)
  } yield (FuncEntry(name, size.toInt, address))
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

case class SymbolTableInfo(
  externalFunctions: Set[ExternalFunction] = Set(),
  globals: Set[SpecGlobal] = Set(),
  funcEntries: Set[FuncEntry] = Set(),
  globalOffsets: Map[BigInt, BigInt] = Map()
) {
  /*
   * The subset of IRContext that we preserve through serialisation.
   */

  import ir.parsing.Attrib

  def merge(o: SymbolTableInfo) = {
    SymbolTableInfo(
      externalFunctions ++ o.externalFunctions,
      globals ++ o.globals,
      funcEntries ++ o.funcEntries,
      globalOffsets ++ o.globalOffsets
    )
  }

  def toAttrib = {

    val goffs = Attrib.List(globalOffsets.toVector.map { case (l, r) =>
      Attrib.List(Vector(Attrib.Int(l), Attrib.Int(r)))
    })

    Attrib.Map(
      ListMap(
        "externalFunctions" -> Attrib.List(externalFunctions.toVector.map(_.toAttrib)),
        "globals" -> Attrib.List(globals.toVector.map(_.toAttrib)),
        "funcEntries" -> Attrib.List(funcEntries.toVector.map(_.toAttrib)),
        "globalOffsets" -> goffs
      )
    )
  }

  def mergeFromAttrib(a: Attrib) = {
    for {
      l <- a.Map
      externalFunctionsList <- l.get("externalFunctions").flatMap(_.List)
      externalFunctions = externalFunctionsList
        .flatMap(e => {
          externalFunctionFromAttrib(e) match {
            case Some(e) => Seq(e)
            case None =>
              Logger.error(s"Malformed external funcion: ${e.pprint}")
              Seq()
          }
        })
        .toSet
      globalsList <- l.get("globals").flatMap(_.List)
      globals = globalsList
        .flatMap(g =>
          specGlobalFromAttrib(g) match {
            case Some(e) => Seq(e)
            case None =>
              Logger.error(s"Malformed specglobal: ${g.pprint}")
              Seq()
          }
        )
        .toSet
      funcEntriesList <- l.get("funcEntries").flatMap(_.List)
      funcEntries = funcEntriesList
        .flatMap(g =>
          funcEntryFromAttrib(g) match {
            case Some(e) => Seq(e)
            case None =>
              Logger.error(s"Malformed FuncEntry: ${g.pprint}")
              Seq()
          }
        )
        .toSet
      globalOffsetsList <- l.get("globalOffsets").flatMap(_.List)
      globalOffsets = globalOffsetsList
        .map(e =>
          e.List match {
            case Some(e) =>
              e.toList match {
                case a :: b :: Nil if a.Int.isDefined && b.Int.isDefined => a.Int.get -> b.Int.get
                case o => throw Exception(s"Malformed global offsets ${globalOffsetsList}")
              }
            case None => throw Exception(s"Malformed global offsets ${globalOffsetsList}")
          }
        )
        .toMap

    } yield (this.merge(SymbolTableInfo(externalFunctions, globals, funcEntries, globalOffsets)))
  }

}

object SymbolTableInfo {
  def from(e: util.IRContext) = {
    SymbolTableInfo(e.externalFunctions, e.globals, e.funcEntries, e.globalOffsets)
  }

  def empty = SymbolTableInfo(Set(), Set(), Set(), Map())
}

case class MemoryStatic(name: String, address: BigInt, size: Int, readOnly: Boolean, bytes: String) {

  /** 
  *  InitialMemory minus the merged regions graph
  *
  *  This stores [[bytes]] as a Base64-encoded gzip-compressed string containing the original bytes.
  *  The goal of compression is just to redcue the representatino length of large sections of zeros and is more standard than a custom RLE or compression
  *  algorithm implementation. It conveniently allows all byte strings to be stored the same way.
  *
  */

  def toMemorySection: MemorySection = {
    val decoded: Array[Byte] = {
      val dc = Base64.getDecoder()
      val by = dc.decode(bytes)

      if (MemoryStatic.compress) {
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
    Attrib.Map(
      ListMap(
        ("name") -> Attrib.Str(name),
        ("address") -> Attrib.Int(address),
        ("size") -> Attrib.Int(size),
        ("readOnly") -> Attrib.Bool(readOnly),
        ("bytes") -> Attrib.Str(bytes)
      )
    )
  }
}

case object MemoryStatic {

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
    assert(decoded.toList == bytes.toList)

    MemoryStatic(m.name, m.address, m.size, m.readOnly, b64bytes)
  }

  def fromAttrib(a: Attrib) = {
    for {
      l <- a.Map
      name <- l.get("name").flatMap(_.Str)
      address <- l.get("address").flatMap(_.Int)
      size <- l.get("size").flatMap(_.Int)
      readOnly <- l.get("readOnly").flatMap(_.Bool)
      bytes <- l.get("bytes").flatMap(_.Str)
    } yield (MemoryStatic(name, address, size.toInt, readOnly, bytes))
  }

}
