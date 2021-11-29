import scala.sys.process.Process

val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    name := "wptool-boogie",
    version := "0.0.1",

    scalaVersion := scala3Version,
    Antlr4 / antlr4Version := "4.9.3",

    Compile / run / mainClass := Some("main"),

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.antlr" % "antlr4-runtime" % "4.9.3",

    Antlr4 / antlr4PackageName := Some("BilParser")

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


