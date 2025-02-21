import scala.util.matching.Regex
import java.util.regex.Pattern

import org.scalatest.funsuite.AnyFunSuite


class UniversalIndenterTest extends AnyFunSuite {

  import util.UniversalIndenter.{TokenType, Config}

  val config = Config(
    Seq(
      Pattern.compile(raw"\(") -> TokenType.Open,
      Pattern.compile(raw"\)") -> TokenType.Close,
      Pattern.compile(raw",") -> TokenType.Separator,
    ),
    1,
    2,
  )

  test("boop") {

    assert(util.UniversalIndenter(config).indent("(a,(bbb),(aaaaaaaaaa))").toList == List("a"))
  }
}
