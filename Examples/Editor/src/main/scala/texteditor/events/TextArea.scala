package texteditor.events

import java.awt.datatransfer.{DataFlavor, StringSelection}
import java.awt.{Dimension, Graphics2D, Point, Rectangle, SystemColor, Toolkit}

import rescala._
import reswing.ReComponent
import texteditor.{JScrollableComponent, LineIterator, LineOffset, Position}

import scala.math.{max, min}
import scala.swing.event.{Key, KeyPressed, KeyTyped, MouseDragged, MouseEvent}
import scala.swing.{Component, Publisher}

class TextArea extends ReComponent {
  override protected lazy val peer = new Component with ComponentMixin {
    override lazy val peer: JScrollableComponent = new JScrollableComponent with SuperMixin
  }

  import peer.peer.metrics.stringWidth
  import peer.peer.{unitHeight => lineHeight}

  protected val padding = 5
  protected val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
  protected val buffer = new GapBuffer

  def this(text: String) {
    this
    buffer.insert(text)
  }

  def charCount = buffer.length

  def lineCount = LineIterator(buffer.iterator).size

  def wordCount = buffer.iterator.foldLeft((0, false)){(c, ch) =>
    val alphanum = Character.isLetterOrDigit(ch)
    (if (alphanum && !c._2) c._1 + 1 else c._1, alphanum)}._1

  def selected = {
    val (dot, mark) = (caret.dot, caret.mark)
    val (start, end) = (min(dot, mark), max(dot, mark))
    buffer.iterator.slice(start, end)
  }

  lazy val charCountChanged: Event[Int] = changed.map((_: Iterator[Char]).length) //#EVT //#EF
  lazy val lineCountChanged: Event[Int] = changed.map(LineIterator(_: Iterator[Char]).length) //#EVT //#EF
  lazy val wordCountChanged: Event[Int] = changed.map{_: Iterator[Char] => wordCount} //#EVT //#EF
  lazy val selectionChanged: Event[Iterable[Char]] = caret.changed.map{pos: (Int, Int) => //#EVT //#EF
    val (dot, mark) = pos
    val (start, end) = (min(dot, mark), max(dot, mark))
    new Iterable[Char] { def iterator = buffer.iterator.slice(start, end) } : Iterable[Char]
  }

  protected lazy val selectedAll = Evt[Unit] //#EVT
  protected lazy val pasted = Evt[Unit] //#EVT
  protected lazy val copied = Evt[Unit] //#EVT

  def selectAll() = selectedAll.fire()
  def paste() = pasted.fire()
  def copy() = copied.fire()

  // A caret has a position in the document referred to as a dot.
  // The dot is where the caret is currently located in the model.
  // There is a second position maintained by the caret that represents
  // the other end of a selection called mark.
  // If there is no selection the dot and mark will be equal.
  // [same semantics as for: javax.swing.text.Caret]
  object caret extends Publisher {
    private lazy val change = Evt[(Int, Int)]  //#EVT
    val changed: Event[(Int, Int)] = //#EVT
      // Caret updated by pressed mouse button, pressed arrow keys, Ctrl+A or select all event
      ((keys.pressed && {e => e.modifiers != Key.Modifier.Control && //#EF
          (e.key == Key.Left || e.key == Key.Right || e.key == Key.Up || e.key == Key.Down||
           e.key == Key.Home || e.key == Key.End)})
        .map{e: KeyPressed => //#EF
          val offset = e.key match {
            case Key.Left =>
              caret.offset - (if (posInLinebreak(caret.offset - 1)) 2 else 1)
            case Key.Right =>
              caret.offset + (if (posInLinebreak(caret.offset + 1)) 2 else 1)
            case Key.Up =>
              val position = Position(max(0, caret.position.row - 1), caret.position.col)
              LineOffset.offset(buffer.iterator, position)
            case Key.Down =>
              val position = Position(min(LineIterator(buffer.iterator).size - 1, caret.position.row + 1), caret.position.col)
              LineOffset.offset(buffer.iterator, position)
            case Key.Home =>
              var offset = 0
              for ((ch, i) <- buffer.iterator.zipWithIndex)
               if (i < caret.offset && (ch == '\r' || ch == '\n'))
                  offset = i + 1;
              offset
            case Key.End =>
              caret.offset +
	              buffer.iterator.drop(caret.offset).takeWhile{
	               ch => ch != '\r' && ch != '\n'
	              }.size
          }
          if (e.modifiers == Key.Modifier.Shift) (offset, caret.mark) else (offset, offset)
        } ||
      (keys.pressed && {e => e.modifiers == Key.Modifier.Control && e.key == Key.A}) //#EF
        .map{_: KeyPressed => (buffer.length, 0)} || //#EF //#EF
      (mouse.clicks.pressed || mouse.moves.dragged).map{e: MouseEvent => //#EF //#EF
          val position = positionFromPoint(e.point)
          val offset = LineOffset.offset(buffer.iterator, position)
          e match { case _: MouseDragged => (offset, caret.mark) case _ => (offset, offset) }
        } ||  //#EF
      selectedAll.map{_: Unit => (0, buffer.length)} || //#EF //#EF
      change)
    .map{pos: (Int, Int) => pos match { //#EF
      case (dot, mark) =>
        buffer.caret = dot
        if ((mark != buffer.caret || mark != markOffset) && mark >= 0 && mark <= buffer.length)
        	markOffset = mark
      }
      (buffer.caret, markOffset)
    }

    changed += { _ match { //#HDL
      case (dot, mark) =>
        buffer.caret = dot
        if ((mark != buffer.caret || mark != markOffset) && mark >= 0 && mark <= buffer.length)
        	markOffset = mark
      }
    }

    // dot as offset
    def dot = buffer.caret
    def dot_=(value: Int) = change.fire((value, mark))

    // dot as position (row and column)
    def dotPos = LineOffset.position(buffer.iterator, dot)
    def dotPos_=(value: Position) = dot = LineOffset.offset(buffer.iterator, value)

    // mark as offset
    protected[TextArea] var markOffset = 0
    def mark = markOffset
    def mark_=(value: Int) = change.fire((dot, value))

    // mark as position (row and column)
    def markPos = LineOffset.position(buffer.iterator, mark)
    def markPos_=(value: Position) = mark = LineOffset.offset(buffer.iterator, value)

    // caret location as offset
    def offset = dot
    def offset_=(value: Int) = change.fire((value, value))

    // caret location as position (row and column)
    def position = dotPos
    def position_=(value: Position) = offset = LineOffset.offset(buffer.iterator, value)

    protected[TextArea] val blink = new Timer(500).start
    protected[TextArea] val steady = new Timer(500, false)
    protected[TextArea] var blinkVisible = false
    protected[TextArea] def visible = peer.hasFocus && (steady.running || blinkVisible)
    blink.fired += { _ =>  //#HDL
        blinkVisible = !blinkVisible
        peer.repaint
    }
  }

  // Content change by pressed backspace, deletion or character key, Ctrl+V or paste event
  val changed: Event[Iterator[Char]] =  //#EVT
    ((((keys.pressed && {e => e.modifiers == Key.Modifier.Control && e.key == Key.V}) || pasted) && //#EF //#EF //#EF
      {_ => clipboard.getContents(null).isDataFlavorSupported(DataFlavor.stringFlavor)})
      .map{_: Any => //#EF
        (0, clipboard.getContents(null).getTransferData(DataFlavor.stringFlavor).asInstanceOf[String])} || //#EF
    (keys.typed && {e => e.modifiers != Key.Modifier.Control}) //#EF
      .map{(_: KeyTyped).char match {
        case '\b' => if (caret.dot != caret.mark) (0, "")
          else (-min(if (posInLinebreak(caret.dot - 1)) 2 else 1, caret.dot), "")
        case '\u007f' => if (caret.dot != caret.mark) (0, "")
          else ((if (posInLinebreak(caret.dot + 1)) 2 else 1), "")
        case c => (0, c.toString)
      }})
  .map{(change: (Int, String)) => change match {  //#EF // TODO: conceptually it is a handler...
    case (del, ins) =>
      val selStart = min(caret.dot, caret.mark)
      val selEnd = max(caret.dot, caret.mark)
      caret.offset = selStart
      buffer.remove(selEnd - selStart)

      if (del < 0)
        caret.offset = caret.offset + del
      buffer.remove(math.abs(del))
      buffer.insert(ins)
      caret.offset = caret.offset + ins.length
    }
    buffer.iterator
  }

  // Content copy by Ctrl+C or copy event
  copied || (keys.pressed && {e => e.modifiers == Key.Modifier.Control && e.key == Key.C}) += { _ => //#EF //#EF //#HDL
    if (caret.dot != caret.mark) {
      val (dot, mark) = (caret.dot, caret.mark)
      val (start, end) = (min(dot, mark), max(dot, mark))
      val s = new StringSelection(buffer.iterator.slice(start, end).mkString);
      clipboard.setContents(s, s);
    }
  }

  protected def posInLinebreak(p: Int) = p > 0 && p < buffer.length &&
    buffer(p - 1) == '\r' && buffer(p) == '\n'

  protected def pointFromPosition(position: Position) = {
    val line = LineIterator(buffer.iterator).drop(position.row).next
    val y = position.row * lineHeight
    val x = stringWidth(line.substring(0, position.col))
    new Point(x + padding, y)
  }

  protected def positionFromPoint(point: Point) = {
    val row = point.y / lineHeight
    val it = LineIterator(buffer.iterator).drop(row)
    val col =
      if (it.hasNext) {
        var prefix = ""
        var col = 0
        it.next.takeWhile{ ch =>
          if (stringWidth(prefix + ch) < point.x) { prefix += ch; col += 1; true } else false }
        col
      }
      else 0
    Position(row, col)
  }

  // handle focus, scroll and paint updates
  mouse.clicks.pressed += { _ => this.requestFocusInWindow } //#HDL

  caret.changed += { _ =>  //#HDL
    def it = LineIterator(buffer.iterator)
    peer.preferredSize = new Dimension(2 * padding + it.map(stringWidth).max, (it.size + 1) * lineHeight)

    val point = pointFromPosition(caret.dotPos)
    peer.peer.scrollRectToVisible(new Rectangle(point.x - 8, point.y, 16, 2 * lineHeight))

    caret.steady.restart
    peer.repaint
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(SystemColor.text)
    g.fillRect(0, 0, peer.size.width, peer.size.height + lineHeight)

    val selStart = min(caret.dot, caret.mark)
    val selEnd = max(caret.dot, caret.mark)

    var lineIndex = 0
    var charIndex = 0
    for (line <- LineIterator(buffer.iterator)) {
      var start, middle, end = ""
      var middleX, endX = 0

      if (selStart < charIndex + line.length && selEnd > charIndex) {
        val startIndex = if (selStart > charIndex) selStart - charIndex else 0
        val endIndex = if (selEnd < charIndex + line.length) selEnd - charIndex else line.length

        start = line.substring(0, startIndex)
        middle = line.substring(startIndex, endIndex)
        end = line.substring(endIndex)

        middleX = padding + stringWidth(start)
        endX = padding + stringWidth(start + middle)

        g.setColor(SystemColor.textHighlight)
        g.fillRect(middleX, lineIndex * lineHeight + lineHeight - font.now.getSize, endX - middleX, lineHeight)
      }
      else
        start = line

      lineIndex += 1
      charIndex += line.length

      g.setColor(SystemColor.textText)
      g.drawString(start, padding, lineIndex * lineHeight)
      g.drawString(end, endX, lineIndex * lineHeight)

      g.setColor(SystemColor.textHighlightText)
      g.drawString(middle, middleX, lineIndex * lineHeight)
    }

    if (caret.visible) {
      def point = pointFromPosition(caret.position)
      g.setColor(SystemColor.textText)
      g.drawLine(point.x, point.y + lineHeight - font.now.getSize, point.x, point.y + lineHeight)
    }
  }
}
