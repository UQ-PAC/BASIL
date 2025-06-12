package util

import util.twine.{Twine, Indent, Lines}
import util.twine.Twine.{indent, indentNested}

import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

@test_util.tags.UnitTest
class StringUtilsTests extends AnyFunSuite with CaptureOutput {
  val n = System.lineSeparator()

  test("indent one line") {
    assert(indent(Twine("a")).mkString == "  a")
    assert(indent(Twine("a", " b")).mkString == "  a b")
  }
  test("indent of indent") {
    assert(indent(indent(Twine("a"))).mkString == "    a")
    assert(indent(indent(Twine(""))).mkString == "")
  }
  test("tricky indent with lines") {
    assert(indent(indent(Twine.lines(""))).mkString == "")
    assert(indent(indent(Twine.lines("a"))).mkString == "    a")
    assert(indent(indent(Twine.lines("", ""))).mkString == n)
  }
  test("indent two lines") {
    assert(indent(Twine.lines("a", "b")).mkString == s"  a$n  b")
  }
  test("indent three lines") {
    assert(indent(Twine.lines("a", "b", "c")).mkString == s"  a$n  b$n  c")
  }
  test("indent nested lines") {
    assert(indent(Twine.lines(Twine.lines("a", "b", "c"))).mkString == s"  a$n  b$n  c")
  }
  test("lines with blanks should insert newline but no trailing spaces") {
    assert(indent(Twine.lines("a", "", "c")).mkString == s"  a$n$n  c")
  }
  test("indentnested") {

    assert(indentNested("head(", List("a", "b", "c").map(Twine(_)), ")tail").mkString == """
head(
  a,
  b,
  c
)tail""".trim)

    assert(
      indentNested("<head>", List("a", "b", "c").map(Twine(_)), "<tail>", sep = "<sep>").mkString
        ==
          """<head>
  a<sep>
  b<sep>
  c
<tail>""",
      "usual case"
    )

    assert(
      indentNested("<head>", List(), "<tail>", sep = "<sep>").mkString
        ==
          "<head><tail>",
      "empty elems should insert no newlines"
    )

    assert(
      indentNested("<head>", List("a", "b", "c").map(Twine(_)), "<tail>", sep = "<sep>", headSep = true).mkString
        .replace(n, "<nl>")
        ==
          "<head><sep><nl>  a<sep><nl>  b<sep><nl>  c<nl><tail>",
      "usual case with headSep"
    )
  }

  test("escape for variable name") {
    assert(StringEscape.escapeForVariableName("$") == "Dollar")
    assert(StringEscape.escapeForVariableName("$::") == "DollarColonColon")
    assert(StringEscape.escapeForVariableName("a.b") == "aDotb")
    assert(StringEscape.escapeForVariableName("`%#*()!") == "U0060PercentHashTimesU0028U0029Bang");
    assert(StringEscape.escapeForVariableName("=~") == "EqTilde");
    assert(StringEscape.escapeForVariableName("a_b-") == "a_bMinus");
    assert(StringEscape.escapeForVariableName("0a_b") == "0a_b");
  }
}
