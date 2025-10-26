package API

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

case class ConfigPair(adt: String, relf: Option[String])

object ConfigScanner {

  def scan(baseDir: Path = Paths.get("src/test/correct")): List[ConfigPair] = {
    if (!Files.exists(baseDir) || !Files.isDirectory(baseDir)) return List.empty

    def scanDir(dir: Path): List[ConfigPair] = {
      val children = Files.list(dir).iterator().asScala.toList
      val adts = children.filter(f => f.toString.endsWith(".adt"))
      val relfs = children.filter(f => f.toString.endsWith(".relf")).map(_.toString).toSet

      val pairsHere = adts.map { adt =>
        val relf = relfs.find(r => Paths.get(r).getParent == adt.getParent)
        ConfigPair(adt.toString, relf)
      }

      val subdirPairs = children.filter(Files.isDirectory(_)).flatMap(scanDir)

      pairsHere ++ subdirPairs
    }

    scanDir(baseDir)
  }
}
