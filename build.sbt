import scala.sys.process.Process

ThisBuild / scalaVersion := "3.1.0"
ThisBuild / version := "0.0.1"
ThisBuild / organization := "uq.pac"

val javaTests = "com.novocode" % "junit-interface" % "0.11" % "test"
val scalaTests = "org.scalatest" %% "scalatest" % "3.2.10" % "test"
val scalactic = "org.scalactic" %% "scalactic" % "3.2.10"
val antlrRuntime = "org.antlr" % "antlr4-runtime" % "4.9.3"

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

libraryDependencies ++= {
    // Determine OS version of JavaFX binaries
    val osName = System.getProperty("os.name") match {
        case n if n.startsWith("Linux")   => "linux"
        case n if n.startsWith("Mac")     => "mac"
        case n if n.startsWith("Windows") => "win"
        case _                            => throw new Exception("Unknown platform!")
    }

    Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
        .map(m => "org.openjfx" % s"javafx-$m" % "15.0.1" classifier osName)
}