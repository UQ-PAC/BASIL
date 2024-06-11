import scala.io.Source

ThisBuild / scalaVersion := "3.3.1"
ThisBuild / version := "0.0.1"
ThisBuild / organization := "uq.pac"

val javaTests = "com.novocode" % "junit-interface" % "0.11" % "test"
val scalaTests = "org.scalatest" %% "scalatest" % "3.2.10" % "test"
val scalactic = "org.scalactic" %% "scalactic" % "3.2.10"
val antlrRuntime = "org.antlr" % "antlr4-runtime" % "4.9.3"
val sourceCode = "com.lihaoyi" %% "sourcecode" % "0.3.0"
val mainArgs = "com.lihaoyi" %% "mainargs" % "0.5.1"

lazy val root = project
  .in(file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    name := "wptool-boogie",
    Antlr4 / antlr4Version := "4.9.3",
    Antlr4 / antlr4GenVisitor := true,
    Antlr4 / antlr4PackageName := Some("Parsers"),
    Compile / run / mainClass := Some("Main"),
    libraryDependencies += javaTests,
    libraryDependencies += antlrRuntime,
    libraryDependencies += scalactic,
    libraryDependencies += scalaTests,
    libraryDependencies += sourceCode,
    libraryDependencies += mainArgs,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

scalacOptions ++= Seq("-deprecation", "-feature")

Compile / PB.targets := Seq(
  scalapb.gen() -> (Compile / sourceManaged).value / "scalapb"
)
libraryDependencies ++= Seq(
  "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
)
libraryDependencies += "io.spray" %% "spray-json" % "1.3.6"

lazy val updateExpectedBAP = taskKey[Unit]("updates .expected for BAP test cases")

updateExpectedBAP := {
  val correctPath = baseDirectory.value / "src" / "test" / "correct"
  val incorrectPath = baseDirectory.value / "src" / "test" / "incorrect"

  def expectedUpdate(path: File, shouldVerify: Boolean): Unit = {
    val log = streams.value.log
    val examples = (path * "*") filter { _.isDirectory }
    for (e <- examples.get()) {
      val variations = (e * "*") filter { _.isDirectory }
      for (v <- variations.get()) {
        val name = e.getName
        val outPath = v / (name + "_bap.bpl")
        val expectedPath = v / (name + ".expected")
        val resultPath = v / (name + "_result.txt")
        if (resultPath.exists()) {
          val result = IO.read(resultPath)
          val verified = result.strip().equals("Boogie program verifier finished with 0 errors")
          if (verified == shouldVerify) {
            if (outPath.exists() && !(expectedPath.exists() && filesContentEqual(outPath, expectedPath))) {
              IO.copyFile(outPath, expectedPath)
            }
          }
        }
      }
    }
  }

  def filesContentEqual(path1: File, path2: File): Boolean = {
    val source1 = Source.fromFile(path1)
    val source2 = Source.fromFile(path2)
    val lines1 = source1.getLines
    val lines2 = source2.getLines
    while (lines1.hasNext && lines2.hasNext) {
      val line1 = lines1.next()
      val line2 = lines2.next()
      if (line1 != line2) {
        source1.close
        source2.close
        return false
      }
    }
    if (lines1.hasNext || lines2.hasNext) {
      source1.close
      source2.close
      return false
    }

    source1.close
    source2.close
    true
  }

  expectedUpdate(correctPath, true)
  expectedUpdate(incorrectPath, false)
}
