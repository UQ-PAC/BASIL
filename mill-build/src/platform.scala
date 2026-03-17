package millbuild

// Interesting reading: https://nesbitt.io/2026/02/17/platform-strings.html

case class Platform(val arch: Platform.Arch, val os: Platform.Os) {
  def toPlatformString = arch.gnuName + "-" + os.osName
}

object Platform {
  enum Arch(val mavenName: String, val gnuName: String) {
    case X86_64 extends Arch("x64", "x86_64")
    case Aarch64 extends Arch("arm64", "aarch64")
  }

  enum Os(val libExtension: String, val osName: String) {
    case Linux extends Os("so", "linux")
    case Mac extends Os("dylib", "darwin")
    case Windows extends Os("dll", "windows")
  }

  export Arch.*
  export Os.*

  def detectArch() = System.getProperty("os.arch") match {
    case "amd64" => Right(Arch.X86_64)
    case "x86_64" => Right(Arch.X86_64)
    case "aarch64" => Right(Arch.Aarch64)
    case x => Left("unknown arch: " + x)
  }

  def detectOs() = System.getProperty("os.name") match {
    case "Linux" => Right(Os.Linux)
    case m if m.startsWith("Mac OS") => Right(Os.Mac)
    case w if w.startsWith("Windows") => Right(Os.Windows)
    case x => Left("unknown os: " + x)
  }

  def detect() = for {
    arch <- detectArch()
    os <- detectOs()
  } yield Platform(arch, os)

  def detectExn() = detect().fold(x => throw Exception(x), identity)
}

