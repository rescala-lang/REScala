package loci
package transmitter

import scala.collection.mutable
import scala.util.matching.Regex

object Parser {
  private val escape = """[\[\]\\:,]""".r
  private val unescape = """\\.""".r

  final class Deserializer private[Parser] (content: String, start: Int, end: Int) {
    private def parse(start: Int, end: Int): List[Deserializer] = {
      var depth = 0
      var first = start + 1
      var last = start + 1
      var escaped = false
      val buffer = mutable.ListBuffer.empty[Deserializer]

      if (start >= end - 1)
        throw new IllegalArgumentException("Malformed signature: "+
          "Expected '[...]' but found empty string")

      if (content(start) != '[' || content(end - 1) != ']')
        throw new IllegalArgumentException("Malformed signature: "+
          s"Expected '[...]' but found '${content.substring(start, end)}'")

      if (last < end - 1) {
        while (last < end - 1) {
          if (escaped)
            escaped = false
          else if (content(last) == '\\')
            escaped = true
          else if (content(last) == '[')
            depth += 1
          else if (content(last) == ']') {
            depth -= 1
            if (depth < 0)
              throw new IllegalArgumentException("Malformed signature: "+
                s"Superfluous ']' in ...${content.substring(start + 1, last + 1)}...")
          }
          else if (content(last) == ',' && depth == 0) {
            buffer += new Deserializer(content, first, last)
            first = last + 1
          }
          last += 1
        }

        if (depth != 0)
          throw new IllegalArgumentException("Malformed signature: "+
            s"Missing ']' in '${content.substring(start, end)}'")

        buffer += new Deserializer(content, first, last)
      }

      buffer.toList
    }

    @throws[IllegalArgumentException]("if the content cannot be parsed as tagged value")
    def asTag(tags: String*): (Int, Deserializer) = {
      var first = start
      var escaped = false

      while (first < end && (escaped || content(first) != ':')) {
        if (escaped)
          escaped = false
        else if (content(first) == '\\')
          escaped = true

        first += 1
      }

      if (first >= end)
        throw new IllegalArgumentException("Malformed signature: "+
          s"Expected tag name but found '${content.substring(start, end)}'")

      if (start >= first - 1)
        throw new IllegalArgumentException("Malformed signature: "+
          "Unexpected empty tag name")

      val tag = content.substring(start, first)
      val index = tags indexOf tag

      if (index == -1)
        throw new IllegalArgumentException("Malformed signature: "+
          s"Unexpected tag name '$tag' but expected one of ${tags.mkString("'", "', '", "'")}")

      index -> new Deserializer(content, first + 1, end)
    }

    @throws[IllegalArgumentException]("if the content cannot be parsed as a sequence of elements")
    def asElements(count: Int): List[Deserializer] = {
      val elements = parse(start, end)
      if (elements.size != count)
        throw new IllegalArgumentException("Malformed signature: "+
          s"Expected $count elements but found ${elements.size} elements in '${content.substring(start, end)}'")

      elements
    }

    @throws[IllegalArgumentException]("if the content cannot be parsed as a list")
    def asList: List[Deserializer] =
      parse(start, end)

    def asString: String = {
      val string = toString
      if (string == "[]")
        ""
      else
        unescape.replaceAllIn(
          string,
          matching => Regex.quoteReplacement(matching.matched(1).toString))
    }

    override def toString =
      content.substring(start, end)
  }

  def parse(content: String) = new Deserializer(content, 0, content.length)

  def apply(content: String) = parse(content)

  def tag(tag: String, value: Serializer) = new Serializer {
    if (tag.isEmpty)
      throw new IllegalArgumentException("Unexpected empty tag name")

    def toString(builder: StringBuilder) = {
      builder ++= escape(tag)
      builder += ':'
      value.toString(builder)
    }
  }

  def elements(elements: Serializer*) = new Serializer {
    def toString(builder: StringBuilder) = {
      var first = true
      builder += '['
      elements foreach { serializer =>
        if (first)
          first = false
        else
          builder += ','
        serializer.toString(builder)
      }
      builder += ']'
    }
  }

  def list(list: List[Serializer]) = elements(list: _*)

  def string(string: String) = new Serializer {
    def toString(builder: StringBuilder) = {
      if (string.isEmpty)
        builder ++= "[]"
      else
        builder ++= escape(string)
    }
  }

  private def escape(string: String): String =
    escape.replaceAllIn(
      string,
      matching => Regex.quoteReplacement(s"\\${matching.matched}"))

  abstract class Serializer private[Parser] {
    def toString(builder: StringBuilder): Unit

    override def toString: String = {
      val builder = new StringBuilder
      toString(builder)
      builder.toString
    }
  }
}
