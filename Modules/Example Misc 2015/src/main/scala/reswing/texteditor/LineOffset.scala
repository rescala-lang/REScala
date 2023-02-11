package reswing.texteditor

class LineIterator(it: Iterator[Char]) extends Iterator[String] {
  private var blank            = !it.hasNext
  private var ch: Option[Char] = None
  private def nextChar =
    if (ch.nonEmpty) { val c = ch.get; ch = None; c }
    else it.next()

  def hasNext = blank || !ch.isEmpty || it.hasNext
  def next(): String = {
    if (blank) { blank = false; return "" }

    val sb = new StringBuilder
    while (hasNext)
      nextChar match {
        case '\r' =>
          if (hasNext)
            it.next() match {
              case '\n' => blank = !hasNext; return sb.toString + "\r\n"
              case c    => ch = Some(c)
            }
          blank = !hasNext; return sb.toString + '\r'
        case '\n' =>
          blank = !hasNext; return sb.toString + '\n'
        case ch =>
          sb += ch
      }

    return sb.toString
  }
}

object LineIterator {
  def apply(it: Iterator[Char]) = new LineIterator(it)
  def apply(it: Iterable[Char]) = new LineIterator(it.iterator)
}

case class Position(row: Int, col: Int)

object Position {
  implicit def fromTuple(tuple: (Int, Int)): Position = Position(tuple._1, tuple._2)
}

object LineOffset {
  def position(it: Iterator[Char], offset: Int): Position = {
    var (row, col, prev) = (0, 0, ' ')
    for (ch <- it.slice(0, offset)) {
      if (ch != '\n' || prev != '\r')
        ch match {
          case '\n' | '\r' => col = 0; row += 1
          case _           => col += 1
        }
      prev = ch
    }
    Position(row, col)
  }

  def position(it: Iterable[Char], offset: Int): Position =
    position(it.iterator, offset)

  def offset(it: Iterator[Char], position: Position): Int = {
    var (row, col, off, prev) = (0, 0, 0, ' ')
    while (it.hasNext) {
      val ch = it.next()
      if (ch != '\n' || prev != '\r') {
        if (position == Position(row, col) || (position.row == row && (ch == '\n' || ch == '\r')))
          return off

        ch match {
          case '\n' | '\r' => col = 0; row += 1
          case _           => col += 1
        }
      }
      prev = ch
      off += 1
    }
    return off
  }

  def offset(it: Iterable[Char], position: Position): Int =
    offset(it.iterator, position)
}
