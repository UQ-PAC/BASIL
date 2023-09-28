import scala.sys.process.Process

ThisBuild / scalaVersion := "3.1.0"
ThisBuild / version := "0.0.1"
ThisBuild / organization := "uq.pac"

val javaTests = "com.novocode" % "junit-interface" % "0.11" % "test"
val scalaTests = "org.scalatest" %% "scalatest" % "3.2.10" % "test"
val scalactic = "org.scalactic" %% "scalactic" % "3.2.10"
val antlrRuntime = "org.antlr" % "antlr4-runtime" % "4.9.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

lazy val root = project
  .in(file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    name := "wptool-boogie",
    Antlr4 / antlr4Version := "4.9.3",
    Antlr4 / antlr4GenVisitor := true,
    Antlr4 / antlr4PackageName := Some("BilParser"),
    Compile / run / mainClass := Some("main"),
    libraryDependencies += javaTests,
    libraryDependencies += antlrRuntime,
    libraryDependencies += scalactic,
    libraryDependencies += scalaTests
  )

lazy val updateExpected = taskKey[Unit]("updates .expected for test cases")

updateExpected := {
  val correctPath = baseDirectory.value / "src" / "test" / "correct"
  val incorrectPath = baseDirectory.value / "src" / "test" / "incorrect"

  def expectedUpdate(path: File, shouldVerify: Boolean): Unit = {
    val log = streams.value.log
    val examples = (path * "*") filter { _.isDirectory }
    for (e <- examples.get()) {
      val variations = (e * "*") filter { _.isDirectory }
      for (v <- variations.get()) {
        val name = e.getName
        val outPath = v / (name + ".bpl")
        val expectedPath = v / (name + ".expected")
        val resultPath = v / (name + "_result.txt")
        if (resultPath.exists()) {
          val result = IO.read(resultPath)
          val verified = result.strip().equals("Boogie program verifier finished with 0 errors")
          if (verified == shouldVerify && outPath.exists()) {
            IO.copyFile(outPath, expectedPath)
          }
        }
      }
    }
  }

  expectedUpdate(correctPath, true)
  expectedUpdate(incorrectPath, false)
}

