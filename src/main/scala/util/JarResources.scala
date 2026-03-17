package util
import java.nio.file.{Files, StandardCopyOption}

object JarResources {

  /**
   * Loads the given library name from the Java resources classpath,
   * extracting it from the JAR if needed.
   *
   * This can be passed as the `pLoader` argument to
   * [[org.sosy_lab.java_smt.SolverContextFactory]]'s constructor to
   * allow it to load libraries from the JAR.
   */
  def loadNativeLibraryFromJar(libraryName: String) = {
    val fileName = System.mapLibraryName(libraryName)

    val platformFolder = millbuild.Platform.detectExn().toPlatformString
    val resourcePath = "/" + platformFolder + "/" + fileName

    val tempFile = Files.createTempFile("jar-resource", "-" + fileName)
    val stream = getClass.getResourceAsStream(resourcePath)

    if (stream == null) {
      throw new Exception("cannot load resource from jar: " + resourcePath)
    }

    Files.copy(stream, tempFile, StandardCopyOption.REPLACE_EXISTING)
    System.load(tempFile.toString)
  }
}
