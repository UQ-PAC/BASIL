import mill._, mill.define._, scalalib._

import $file.antlr // https://index.scala-lang.org/ml86/mill-antlr

import os.Path
import $ivy.`com.lihaoyi::mill-contrib-scalapblib:$MILL_VERSION`
import contrib.scalapblib._

object basil extends RootModule with ScalaModule with antlr.AntlrModule with ScalaPBModule {
  // ammoniteVersion should be updated whenever scalaVersion is changed. see, for example,
  // https://mvnrepository.com/artifact/com.lihaoyi/ammonite_3.4.3 to list valid versions.
  def scalaVersion = "3.3.4"
  override def ammoniteVersion = "3.0.2"

  def scalacOptions: T[Seq[String]] = Seq("-deprecation")

  val javaTests = ivy"com.novocode:junit-interface:0.11"
  val scalaTests = ivy"org.scalatest::scalatest:3.2.19"
  val scalaCheck = ivy"org.scalatestplus::scalacheck-1-18:3.2.19.0"
  val scalactic = ivy"org.scalactic::scalactic:3.2.19"
  val antlrRuntime = ivy"org.antlr:antlr4-runtime:4.9"
  val sourceCode = ivy"com.lihaoyi::sourcecode:0.3.0"
  val mainArgs = ivy"com.lihaoyi::mainargs:0.5.1"
  val sprayJson = ivy"io.spray::spray-json:1.3.6"
  val scalapb = ivy"com.thesamet.scalapb::scalapb-runtime:0.11.15"
  val scalaCompiler = ivy"org.scala-lang::scala3-compiler:3.3.4"
  
  val apiDsl = ivy"org.http4s::http4s-dsl:0.23.23"
  val apiBlaze = ivy"org.http4s::http4s-blaze-server:0.23.17"
  val apiCircle = ivy"org.http4s::http4s-circe:0.23.23"
  val apiGeneric = ivy"io.circe::circe-generic:0.14.6"
  val slf4j = ivy"org.slf4j:slf4j-simple:2.0.12"
  val scalaXml = ivy"org.scala-lang.modules::scala-xml:2.1.0"

  def scalaPBVersion = "0.11.15"

  def mainClass = Some("Main")

  override def scalaPBSources = T.sources { Seq(PathRef(this.millSourcePath / "main" / "protobuf")) }
  def millSourcePath = super.millSourcePath / "src"
  def ivyDeps = Agg(scalactic, antlrRuntime, sourceCode, mainArgs, sprayJson, scalapb, scalaCompiler, apiDsl, apiBlaze, apiCircle, apiGeneric, slf4j, scalaXml)
  def sources = T.sources { Seq(PathRef(this.millSourcePath / "main" / "scala")) }

  override def antlrPackage: Option[String] = Some("Parsers")
  override def antlrGenerateVisitor = true
  override def antlrGrammarSources = T.sources {
    Seq(PathRef(millSourcePath / "main" / "antlr4"))
  }
  override def resources = T.sources {
    Seq(PathRef(millSourcePath / "main" / "resources"))
  }

  object test extends ScalaTests with TestModule.ScalaTest {
    override def ammoniteVersion = basil.ammoniteVersion

    def ivyDeps = Agg(scalaTests, scalaCheck, javaTests)
    def sources = T.sources { Seq(PathRef(this.millSourcePath / "scala")) }
  }

  /** Updates the expected
    */

  def updateExpectedBAP() = T.command {
    val correctPath = test.millSourcePath / "correct"
    val incorrectPath = test.millSourcePath / "incorrect"

    expectedUpdate(correctPath, true, true)
    expectedUpdate(incorrectPath, false, true)
  }

  def updateExpectedGTIRB() = T.command {
    val correctPath = test.millSourcePath / "correct"
    val incorrectPath = test.millSourcePath / "incorrect"

    expectedUpdate(correctPath, true, false)
    expectedUpdate(incorrectPath, false, false)
  }

  def updateExpectedExtraSpec() = T.command {
    val correctPath = test.millSourcePath / "extraspec_correct"
    val incorrectPath = test.millSourcePath / "extraspec_incorrect"
    expectedUpdate(correctPath, true, true)
    expectedUpdate(incorrectPath, false, true)
    expectedUpdate(correctPath, true, false)
    expectedUpdate(incorrectPath, false, false)
  }

  def expectedUpdate(path: Path, shouldVerify: Boolean, BAPVariant: Boolean): Unit = {
    val examples = os.list(path).filter(os.isDir)
    for (e <- examples) {
      val variations = os.list(e).filter(os.isDir)
      for (v <- variations) {
        val name = e.last
        val suffix = if (BAPVariant) {
          "_bap"
        } else {
          "_gtirb"
        }
        val expectedSuffix = if (BAPVariant) {
          ""
        } else {
          "_gtirb"
        }
        val outPath = v / (name + suffix + ".bpl")
        val expectedPath = v / (name + expectedSuffix + ".expected")
        val resultPath = v / (name + suffix + "_result.txt")
        if (os.exists(resultPath)) {
          val result = os.read(resultPath)
          val verified = result.strip().equals("Boogie program verifier finished with 0 errors")
          if (verified == shouldVerify) {
            if (os.exists(outPath) && !(os.exists(expectedPath) && filesContentEqual(outPath, expectedPath))) {
              println(s"updated $expectedPath")
              os.copy.over(outPath, expectedPath)
            }
          }
        }
      }
    }
  }

  def filesContentEqual(path1: Path, path2: Path): Boolean = {
    os.read.lines(path1) == os.read.lines(path2)
  }

  def scalafmt = mill.scalalib.scalafmt.ScalafmtModule
}
