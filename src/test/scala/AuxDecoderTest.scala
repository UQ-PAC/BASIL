import org.scalatest.funsuite.AnyFunSuite
import test_util.{CaptureOutput}

import java.util.Base64
import com.google.protobuf.ByteString

@test_util.tags.UnitTest
class AuxDecoderTest extends AnyFunSuite, CaptureOutput {

  def bytes(x: String) =
    java.io.ByteArrayInputStream(x.getBytes)
  def bytes(x: Array[Byte]) =
    java.io.ByteArrayInputStream(x)
  def bytes(x: Int*) =
    java.io.ByteArrayInputStream(Array[Byte](x.map(_.toByte) : _*))

  test("reading unsigned long") {
    assertResult(255) {
      gtirb.AuxDecoder.readUint(8)(bytes(-1))
    }
    assertResult(1) {
      gtirb.AuxDecoder.readUint(16)(bytes(1, 0))
    }
    assertResult(256) {
      gtirb.AuxDecoder.readUint(16)(bytes(0, 1))
    }
  }

  lazy val functionEntriesData = "DgAAAAAAAAAKN8AQfJJCx6z/YC/0ajCuAQAAAAAAAAC63E936mNIwLttnN9qQwpSEC0VjLcjTrWfq3ProZkm4QEAAAAAAAAA8+/+FHF9QLyY8Cp3s20b7UK9rpkxpUlVn3nywB/Pa6MBAAAAAAAAABDsf7+EvUScja3NVtybLYB8D9RjUu1GUJuU7h50M6qCAQAAAAAAAADPt2wBnjVBh72u9sO3UmghhYEnBCBdRgqUP83Wk83TKgEAAAAAAAAABH/y67RjR/CceNXW4dxJX5v1FZgF+UVhpIPWkdaYGM8BAAAAAAAAABE9oSg0AkvBr4d3nZzAJBueV40Xb8lNGL/X4rr0M+yNAQAAAAAAAADwmKneRoNNo7x59rfe+fM2y2YqeTQETDizbRO9Gd5rTgEAAAAAAAAATtUSuPRtRpKUyIiDPj5JWdgBS4GeWUYpkXOWrjz19dsBAAAAAAAAAHtFQipdkEVBlIMEUcxGTzjjOXYE/IhEDYMFs5/1CguSAQAAAAAAAABEjFAwt29EyqDlVadlTiLU7efo2YpZRjKoGwMiNg5x7AEAAAAAAAAARcz/PMuZRmSsGVbCF4FnpfPpZJPuKUVCovWrMD5+jrUBAAAAAAAAAAHijBSRNEmvo+oayuSZKB31ZSjpkoFFbpQb5ZqbQCvBAQAAAAAAAABYmEPRwHtEIr+F+AjLrUdn9g3j8z3UR3uy4oljzAjVDgEAAAAAAAAAaXmP/QmsQ2uAJBCKxtQSsQ=="
  lazy val functionEntriesBytes = Base64.getDecoder().decode(functionEntriesData)
}
