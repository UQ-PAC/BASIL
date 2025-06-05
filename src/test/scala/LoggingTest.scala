import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

@test_util.tags.UnitTest
class LoggingTest extends AnyFunSuite, CaptureOutput {

  import java.nio.file.Files
  import java.io.File
  import scala.jdk.CollectionConverters.*

  import util.Logger

  test("GenericLogger.writeToFile") {
    val f = File.createTempFile("basil-test-logger-output", "")

    val s = "jfidosajfioas"
    Logger.writeToFile(f, s)

    assertResult(List(s)) {
      Files.readAllLines(f.toPath).asScala
    }
  }
}
