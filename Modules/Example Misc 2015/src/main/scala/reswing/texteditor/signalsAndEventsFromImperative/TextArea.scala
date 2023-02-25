package reswing.texteditor.signalsAndEventsFromImperative

import java.awt.{Dimension, Graphics2D, Point, Rectangle, SystemColor, Toolkit}
import java.awt.datatransfer.{DataFlavor, StringSelection}
import rescala.default._
import reswing.{ReComponent, ReSwingValue}
import reswing.texteditor.{JScrollableComponent, LineIterator, LineOffset, Position}

import scala.language.postfixOps
import scala.math.{max, min}
import scala.swing.Component
import scala.swing.event.Key

class TextArea extends ReComponent {
  object localPeer extends Component with ComponentMixin {
    override lazy val peer: JScrollableComponent with SuperMixin = new JScrollableComponent with SuperMixin
  }
  override protected lazy val peer: localPeer.type = localPeer

  protected def stringWidth = peer.peer.metrics.stringWidth _
  protected def lineHeight  = peer.peer.unitHeight

  protected val padding   = 5
  protected val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard

  protected lazy val buffer = new GapBuffer

  def this(text: String) = {
    this()
    buffer.insert(text)
  }

  override val preferredSize: ReSwingValue[Dimension] = Signal {
    def it = LineIterator(buffer.iterable.value)
    new Dimension(2 * padding + it.map(stringWidth(_)).max, (it.size + 1) * lineHeight)
  }
  preferredSize.using(() => peer.preferredSize, peer.preferredSize_= _, "preferredSize")

  val charCount = Signal { buffer.length.value }

  val lineCount = Signal { LineIterator(buffer.iterable.value).size }

  val wordCount = Signal {
    buffer.iterable.value.iterator.foldLeft((0, false)) { (c, ch) =>
      val alphanum = Character.isLetterOrDigit(ch)
      (if (alphanum && !c._2) c._1 + 1 else c._1, alphanum)
    }._1
  }

  val selected = Signal {
    val (it, dot, mark) = (buffer.iterable.value, caret.dot.value, caret.mark.value)
    val (start, end)    = (min(dot, mark), max(dot, mark))
    new Iterable[Char] { def iterator = it.iterator.slice(start, end) }: Iterable[Char]
  }

  def selectAll(): Unit = {
    caret.dot = charCount.readValueOnce
    caret.mark = 0
  }

  def paste(): Unit = {
    removeSelection()
    val c = clipboard.getContents(null);
    if (c.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = c.getTransferData(DataFlavor.stringFlavor).asInstanceOf[String]
      buffer.insert(str)
      caret.offset = caret.offset.readValueOnce + str.length
    }
  }

  def copy(): Unit = {
    if (selected.readValueOnce.nonEmpty) {
      val s = new StringSelection(selected.readValueOnce.mkString)
      clipboard.setContents(s, s)
    }
  }

  // A caret has a position in the document referred to as a dot.
  // The dot is where the caret is currently located in the model.
  // There is a second position maintained by the caret that represents
  // the other end of a selection called mark.
  // If there is no selection the dot and mark will be equal.
  // [same semantics as for: javax.swing.text.Caret]
  object caret {
    def dot               = buffer.caret
    def dot_=(value: Int) = buffer.caretChanged.fire(value)

    // dot as position (row and column)
    private val dotPosSignal = Signal { LineOffset.position(buffer.iterable.value, dot.value) }
    def dotPos                    = dotPosSignal
    def dotPos_=(value: Position) = dot = LineOffset.offset(buffer.iterable.readValueOnce, value)

    private val markVar = Var(0)

    // mark as offset
    private val markSignal = Signal { markVar.value }
    def mark               = markSignal
    def mark_=(value: Int) = if (value >= 0 && value <= buffer.length.readValueOnce) markVar set value

    // mark as position (row and column)
    private val markPosSignal = Signal { LineOffset.position(buffer.iterable.value, mark.value) }
    def markPos                    = markPosSignal
    def markPos_=(value: Position) = mark = LineOffset.offset(buffer.iterable.readValueOnce, value)

    // caret location as offset
    def offset = dot
    def offset_=(value: Int): Unit = {
      dot = value
      mark = value
    }

    // caret location as position (row and column)
    def position                    = dotPos
    def position_=(value: Position) = offset = LineOffset.offset(buffer.iterable.readValueOnce, value)

    protected[TextArea] val blink  = new Timer(500) start
    protected[TextArea] val steady = new Timer(500, false)
    protected[TextArea] val visible = blink.fired.toggle(
      Signal { hasFocus.value },
      Signal { hasFocus.value && steady.running.value }
    )
  }

  protected def posInLinebreak(p: Int) =
    p > 0 && p < buffer.length.readValueOnce &&
    buffer(p - 1) == '\r' && buffer(p) == '\n'

  protected def removeSelection(): Unit = {
    val selStart = min(caret.dot.readValueOnce, caret.mark.readValueOnce)
    val selEnd   = max(caret.dot.readValueOnce, caret.mark.readValueOnce)
    caret.offset = selStart
    buffer.remove(selEnd - selStart)
  }

  protected def pointFromPosition(position: Position) = {
    val line = LineIterator(buffer.iterable.readValueOnce).drop(position.row).next()
    val y    = position.row * lineHeight
    val x    = stringWidth(line.substring(0, math.min(position.col, line.length)))
    new Point(x + padding, y)
  }

  protected def positionFromPoint(point: Point) = {
    val row = point.y / lineHeight
    val it  = LineIterator(buffer.iterable.readValueOnce).drop(row)
    val col =
      if (it.hasNext) {
        var prefix = ""
        var col    = 0
        it.next().takeWhile { ch =>
          if (stringWidth(prefix + ch) < point.x) { prefix += ch; col += 1; true }
          else false
        }
        col
      } else 0
    Position(row, col)
  }

  keys.pressed observe { e =>
    def shift = e.modifiers == Key.Modifier.Shift
    if (e.modifiers == Key.Modifier.Control)
      e.key match {
        case Key.V => paste()
        case Key.C => copy()
        case Key.A => selectAll()
        case _     =>
      }
    else
      e.key match {
        case Key.Left =>
          val offset = caret.offset.readValueOnce - (if (posInLinebreak(caret.offset.readValueOnce - 1)) 2 else 1)
          if (shift) caret.dot = offset else caret.offset = offset
        case Key.Right =>
          val offset = caret.offset.readValueOnce + (if (posInLinebreak(caret.offset.readValueOnce + 1)) 2 else 1)
          if (shift) caret.dot = offset else caret.offset = offset
        case Key.Up =>
          val position = Position(max(0, caret.position.readValueOnce.row - 1), caret.position.readValueOnce.col)
          if (shift) caret.dotPos = position else caret.position = position
        case Key.Down =>
          val position = Position(
            min(lineCount.readValueOnce - 1, caret.position.readValueOnce.row + 1),
            caret.position.readValueOnce.col
          )
          if (shift) caret.dotPos = position else caret.position = position
        case Key.Home =>
          var offset = 0
          for ((ch, i) <- buffer.iterable.readValueOnce.iterator.zipWithIndex)
            if (i < caret.offset.readValueOnce && (ch == '\r' || ch == '\n'))
              offset = i + 1;
          if (shift) caret.dot = offset else caret.offset = offset
        case Key.End =>
          val offset =
            caret.offset.readValueOnce +
            buffer.iterable.readValueOnce.iterator.drop(caret.offset.readValueOnce).takeWhile {
              ch => ch != '\r' && ch != '\n'
            }.size
          if (shift) caret.dot = offset else caret.offset = offset
        case _ =>
      }
  }

  keys.typed observe { e =>
    if (e.modifiers != Key.Modifier.Control)
      e.char match {
        case '\u007f' => // Del key
          if (selected.readValueOnce.isEmpty) {
            val count = if (posInLinebreak(caret.dot.readValueOnce + 1)) 2 else 1
            buffer.remove(count);
          } else removeSelection()
        case '\b' => // Backspace key
          if (selected.readValueOnce.isEmpty) {
            val count = min(if (posInLinebreak(caret.dot.readValueOnce - 1)) 2 else 1, caret.dot.readValueOnce)
            caret.offset = caret.offset.readValueOnce - count
            buffer.remove(count);
          } else removeSelection()
        case c => // character input
          removeSelection()
          buffer.insert(c.toString)
          caret.offset = caret.offset.readValueOnce + 1
      }
  }

  mouse.clicks.pressed observe { e =>
    this.requestFocusInWindow()
    caret.position = positionFromPoint(e.point)
  }

  mouse.moves.dragged observe { e =>
    caret.dotPos = positionFromPoint(e.point)
  }

  // handle scroll and paint updates
  caret.position.changed observe { _ =>
    val point = pointFromPosition(caret.position.readValueOnce)
    peer.peer.scrollRectToVisible(new Rectangle(point.x - 8, point.y, 16, 2 * lineHeight))
    caret.steady.restart
    ()
  }

  buffer.length.changed || caret.visible.changed ||
  caret.dot.changed || caret.mark.changed observe { _ => this.repaint() }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(SystemColor.text)
    g.fillRect(0, 0, size.readValueOnce.width, size.readValueOnce.height + lineHeight)

    val selStart = min(caret.dot.readValueOnce, caret.mark.readValueOnce)
    val selEnd   = max(caret.dot.readValueOnce, caret.mark.readValueOnce)

    var lineIndex = 0
    var charIndex = 0
    for (line <- LineIterator(buffer.iterable.readValueOnce)) {
      var start, middle, end = ""
      var middleX, endX      = 0

      if (selStart < charIndex + line.length && selEnd > charIndex) {
        val startIndex = if (selStart > charIndex) selStart - charIndex else 0
        val endIndex   = if (selEnd < charIndex + line.length) selEnd - charIndex else line.length

        start = line.substring(0, startIndex)
        middle = line.substring(startIndex, endIndex)
        end = line.substring(endIndex)

        middleX = padding + stringWidth(start)
        endX = padding + stringWidth(start + middle)

        g.setColor(SystemColor.textHighlight)
        g.fillRect(
          middleX,
          lineIndex * lineHeight + lineHeight - font.readValueOnce.getSize,
          endX - middleX,
          lineHeight
        )
      } else
        start = line

      lineIndex += 1
      charIndex += line.length

      g.setColor(SystemColor.textText)
      g.drawString(start, padding, lineIndex * lineHeight)
      g.drawString(end, endX, lineIndex * lineHeight)

      g.setColor(SystemColor.textHighlightText)
      g.drawString(middle, middleX, lineIndex * lineHeight)
    }

    if (caret.visible.readValueOnce) {
      def point = pointFromPosition(caret.position.readValueOnce)
      g.setColor(SystemColor.textText)
      g.drawLine(point.x, point.y + lineHeight - font.readValueOnce.getSize, point.x, point.y + lineHeight)
    }
  }
}

object TextArea {
  implicit def toComponent(input: TextArea): Component = input.peer
}
