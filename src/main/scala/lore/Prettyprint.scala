package lore
// copied from https://gist.github.com/re-xyr/b0f1988e744c93d4c83a5f17f58484ea

import cats.parse.LocationMap
import cats.parse.Parser
import cats.parse.Parser.Expectation

class Prettyprint(filename: String, input: String) {
  private val locmap = LocationMap(input)

  def description(x: Parser.Expectation): String = x match {
    case Expectation.OneOfStr(_, strs) =>
      val strList = strs.map { x => s"'$x'" }.mkString(", ")
      s"expected one of $strList"
    case Expectation.InRange(_, lower, upper) =>
      if (lower == upper) then s"expected '$lower'"
      else s"expected '$lower' ~ '$upper'"
    case Expectation.StartOfString(_) =>
      "expected beginning of file"
    case Expectation.EndOfString(_, _) =>
      "expected end of file"
    case Expectation.Length(_, expected, actual) =>
      s"unexpected eof; expected ${expected - actual} more characters"
    case Expectation.ExpectedFailureAt(_, matched) =>
      s"unexpected '$matched'"
    case Expectation.Fail(_) =>
      "failed to parse"
    case Expectation.FailWith(_, message) =>
      message
    case Expectation.WithContext(contextStr, _) =>
      s"expected $contextStr"
  }

  def prettyprint(x: Parser.Expectation): String = {
    val (row, col) = locmap.toLineCol(x.offset).get
    val (r, c) = (row + 1, col + 1)
    val line = locmap.getLine(row).get
    val offending =
      s"${row.toString map { _ => ' ' }} | ${" " * col}^"
    s"""
      |$filename:$r:$c: error: ${description(x)}
      |$r | $line
      |$offending""".stripMargin
  }

  def prettyprint(x: Parser.Error): String =
    x.expected.map(prettyprint).toList.mkString("")
}
