import util.{Twine, indent, indentNested}

import org.scalatest.funsuite.AnyFunSuite

class StringUtilsTest extends AnyFunSuite {
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
}
