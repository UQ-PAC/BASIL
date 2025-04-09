import util.{Twine, indent, indentNested, StringEscape}

import org.scalatest.funsuite.AnyFunSuite

@test_util.tags.UnitTest
class StringUtilsTest extends AnyFunSuite with test_util.CaptureOutput {
  test("indent one line") {
    assert(indent(LazyList("a")) == LazyList("a"))
    assert(indent(LazyList("a", " b")) == LazyList("a", " b"))
  }
  test("indent two lines") {
    assert(indent(LazyList("a\n", "b")) == LazyList("a\n", "  ", "b"))
  }
  test("indent three lines") {
    assert(indent(LazyList("a\n", "b\n", "c")) == LazyList("a\n", "  ", "b\n", "  ", "c"))
  }
  test("indent custom prefix") {
    assert(indent(LazyList("a\n", "b\n", "c"), prefix = "X") == LazyList("a\n", "X", "b\n", "X", "c"))
  }
  test("indent composes") {
    val list = LazyList("a\n", "b\n", "c")
    assert(indent(list, prefix = "      ").mkString == indent(indent(indent(list))).mkString)
  }
  test("indent properties") {
    val list = LazyList("a\n", "b\n", "c", " a")
    val indented = indent(list)
    assert((indented intersect list) == list, "all input strings should occur in the output")
    assert((indented diff list).distinct == LazyList("  "), "the new strings in the output should only be indentation")

    // all the strings in the input also occur in the output, with reference equality.
    assert(
      list.forall(x => indented.exists(_ eq x)),
      "every input string should be contained in the output (by reference)"
    )
  }

  test("indentnested") {

    assert(indentNested("head(", List("a", "b", "c").map(LazyList(_)), ")tail").mkString == """
head(
  a,
  b,
  c
)tail""".trim)

    assert(
      indentNested("<head>", List("a", "b", "c").map(LazyList(_)), "<tail>", sep = "<sep>", newline = "<nl>").mkString
        ==
          "<head><nl>a<sep><nl>b<sep><nl>c<nl><tail>",
      "usual case"
    )

    assert(
      indentNested("<head>", List(), "<tail>", sep = "<sep>", newline = "<nl>").mkString
        ==
          "<head><tail>",
      "empty elems should insert no newlines"
    )

    assert(
      indentNested(
        "<head>",
        List("a", "b", "c").map(LazyList(_)),
        "<tail>",
        sep = "<sep>",
        newline = "<nl>",
        headSep = true
      ).mkString
        ==
          "<head><sep><nl>a<sep><nl>b<sep><nl>c<nl><tail>",
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
