package texteditor.signalsAndEventsFromEventsOnly

import java.awt.datatransfer.{DataFlavor, StringSelection}
import java.awt.{Dimension, Graphics2D, Point, Rectangle, SystemColor, Toolkit}

import rescala._
import reswing.{ReComponent, ReSwingValue}
import texteditor.{JScrollableComponent, LineIterator, LineOffset, Position}

import scala.language.implicitConversions
import scala.math.{max, min}
import scala.swing.Component
import scala.swing.event.{Key, KeyPressed, KeyTyped, MouseDragged, MouseEvent}

class TextArea extends ReComponent {
  override protected lazy val peer = new Component with ComponentMixin {
    override lazy val peer: JScrollableComponent = new JScrollableComponent with SuperMixin
  }

  protected def stringWidth = peer.peer.metrics.stringWidth _
  protected def lineHeight = peer.peer.unitHeight

  protected val padding = 5
  protected val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
  protected lazy val buffer = new GapBuffer

  def this(text: String) {
    this
    buffer.insert(text)
  }

  // Used in from the superclass, note the override.
  override val preferredSize: ReSwingValue[Dimension] = Signal {
    //#SIG
    def it = LineIterator(buffer.iterable())
    new Dimension(2 * padding + it.map(stringWidth(_)).max, (it.size + 1) * lineHeight)
  }
  preferredSize using(peer.preferredSize _, peer.preferredSize_= _, "preferredSize")

  val charCount = Signal {buffer.length()} //#SIG

  val lineCount = Signal {LineIterator(buffer.iterable()).size} //#SIG

  val wordCount = Signal {
    buffer.iterable().iterator.foldLeft((0, false)) { (c, ch) => //#SIG
      val alphanum = Character.isLetterOrDigit(ch)
      (if (alphanum && !c._2) c._1 + 1 else c._1, alphanum)
    }._1
  }

  val selected = Signal {
    //#SIG
    val (it, dot, mark) = (buffer.iterable(), caret.dot(), caret.mark())
    val (start, end) = (min(dot, mark), max(dot, mark))
    new Iterable[Char] {def iterator = it.iterator.slice(start, end)}: Iterable[Char]
  }

  protected val selectedAll = Evt[Unit]
  //#EVT
  protected val pasted = Evt[Unit]
  //#EVT
  protected val copied = Evt[Unit] //#EVT

  def selectAll() = selectedAll.fire()
  def paste() = pasted.fire()
  def copy() = copied.fire()

  // A caret has a position in the document referred to as a dot.
  // The dot is where the caret is currently located in the model.
  // There is a second position maintained by the caret that represents
  // the other end of a selection called mark.
  // If there is no selection the dot and mark will be equal.
  // [same semantics as for: javax.swing.text.Caret]
  object caret {
    // dot as offset
    private val dotSignal = Signal {buffer.caret()}
    //#SIG
    def dot = dotSignal
    def dot_=(value: Int) = buffer.caretChanged.fire(value)

    // dot as position (row and column)
    private val dotPosSignal = Signal {LineOffset.position(buffer.iterable(), dot())}
    //#SIG
    def dotPos = dotPosSignal
    def dotPos_=(value: Position) = dot = LineOffset.offset(buffer.iterable.now, value)

    private val markVar = Var(0) //#VAR

    // mark as offset
    private val markSignal = Signal {markVar()}
    //#SIG
    def mark = markSignal
    def mark_=(value: Int) = if (value >= 0 && value <= buffer.length.now) markVar set value

    // mark as position (row and column)
    private val markPosSignal = Signal {LineOffset.position(buffer.iterable(), mark())}
    //#SIG
    def markPos = markPosSignal
    def markPos_=(value: Position) = mark = LineOffset.offset(buffer.iterable.now, value)

    // caret location as offset
    def offset = dot
    def offset_=(value: Int): Unit = {
      dot = value
      mark = value
    }

    // caret location as position (row and column)
    def position = dotPos
    def position_=(value: Position) = offset = LineOffset.offset(buffer.iterable.now, value)

    protected[TextArea] val blink = new Timer(500).start
    protected[TextArea] val steady = new Timer(500, false)
    protected[TextArea] val visible: Signal[Boolean] = blink.fired.toggle(//#IF
      Signal {hasFocus()}, //#SIG
      Signal {hasFocus() && steady.running()}) //#SIG
  }

  protected def posInLinebreak(p: Int) = p > 0 && p < buffer.length.now &&
    buffer(p - 1) == '\r' && buffer(p) == '\n'

  protected def pointFromPosition(position: Position) = {
    val line = LineIterator(buffer.iterable.now).drop(position.row).next
    val y = position.row * lineHeight
    val x = stringWidth(line.substring(0, math.min(position.col, line.length)))
    new Point(x + padding, y)
  }

  protected def positionFromPoint(point: Point) = {
    val row = point.y / lineHeight
    val it = LineIterator(buffer.iterable.now).drop(row)
    val col =
      if (it.hasNext) {
        var prefix = ""
        var col = 0
        it.next.takeWhile { ch =>
          if (stringWidth(prefix + ch) < point.x) {prefix += ch; col += 1; true}
          else false
        }
        col
      }
      else 0
    Position(row, col)
  }

  // Caret updated by pressed mouse button, pressed arrow keys, Ctrl+A or select all event
  (keys.pressed && { e => e.modifiers != Key.Modifier.Control && //#EF
    (e.key == Key.Left || e.key == Key.Right || e.key == Key.Up || e.key == Key.Down ||
      e.key == Key.Home || e.key == Key.End)
  })
    .map { e: KeyPressed => //#EF
      val offset = e.key match {
        case Key.Left =>
          caret.offset.value - (if (posInLinebreak(caret.offset.value - 1)) 2 else 1)
        case Key.Right =>
          caret.offset.value + (if (posInLinebreak(caret.offset.value + 1)) 2 else 1)
        case Key.Up =>
          val position = Position(max(0, caret.position.value.row - 1), caret.position.value.col)
          LineOffset.offset(buffer.iterable.value, position)
        case Key.Down =>
          val position = Position(min(lineCount.value - 1, caret.position.value.row + 1), caret.position.value.col)
          LineOffset.offset(buffer.iterable.value, position)
        case Key.Home =>
          var offset = 0
          for ((ch, i) <- buffer.iterable.value.iterator.zipWithIndex)
            if (i < caret.offset.value && (ch == '\r' || ch == '\n'))
              offset = i + 1;
          offset
        case Key.End =>
          caret.offset.value +
            buffer.iterable.value.iterator.drop(caret.offset.value).takeWhile {
              ch => ch != '\r' && ch != '\n'
            }.size
      }
      if (e.modifiers == Key.Modifier.Shift) (offset, caret.mark.value) else (offset, offset)
    } || //#EF
    (keys.pressed && { e => e.modifiers == Key.Modifier.Control && e.key == Key.A }) //#EF
      .map { _: KeyPressed => (charCount.value, 0) } || //#EF
    (mouse.clicks.pressed || mouse.moves.dragged).map { e: MouseEvent => //#EF //#EF //#EF
      val position = positionFromPoint(e.point)
      val offset = LineOffset.offset(buffer.iterable.value, position)
      e match {case _: MouseDragged => (offset, caret.mark.value) case _ => (offset, offset)}
    } || //#EF
    selectedAll.map { _: Unit => (charCount.value, 0) } += //#HDL
    {
      case (dot, mark) =>
        caret.dot = dot
        caret.mark = mark
    }

  // Content change by pressed backspace, deletion or character key, Ctrl+V or paste event
  (((keys.pressed && { e => e.modifiers == Key.Modifier.Control && e.key == Key.V }) || pasted) && //#EF //#EF //#EF
    { _ => clipboard.getContents(null).isDataFlavorSupported(DataFlavor.stringFlavor) })
    .map { _: Any => //#EF
      (0, clipboard.getContents(null).getTransferData(DataFlavor.stringFlavor).asInstanceOf[String])
    } || //#EF
    (keys.typed && { e => e.modifiers != Key.Modifier.Control }) //#EF
      .map {
      (_: KeyTyped).char match {
        //#EF
        case '\b' => if (selected.value.nonEmpty) (0, "")
        else (-min(if (posInLinebreak(caret.dot.value - 1)) 2 else 1, caret.dot.value), "")
        case '\u007f' => if (selected.value.nonEmpty) (0, "")
        else (if (posInLinebreak(caret.dot.value + 1)) 2 else 1, "")
        case c => (0, c.toString)
      }
    } += //#HDL
    {
      _ match {
        case (del, ins) =>
          val selStart = min(caret.dot.now, caret.mark.now)
          val selEnd = max(caret.dot.now, caret.mark.now)
          caret.offset = selStart
          buffer.remove(selEnd - selStart)

          if (del < 0)
            caret.offset = caret.offset.now + del
          buffer.remove(math.abs(del))
          buffer.insert(ins)
          caret.offset = caret.offset.now + ins.length
      }
    }

  // Content copy by Ctrl+C or copy event
  copied || (keys.pressed && { e => e.modifiers == Key.Modifier.Control && e.key == Key.C }) += //#EF //#EF //#HDL
    { _ =>
      if (selected.now.nonEmpty) {
        val s = new StringSelection(selected.now.mkString)
        clipboard.setContents(s, s)
      }
    }

  // handle focus, scroll and paint updates
  mouse.clicks.pressed += { _ => this.requestFocusInWindow } //#HDL

  buffer.length.changed || caret.dot.changed += { _ => //#EF //#HDL  //#IF //#IF
    val point = pointFromPosition(caret.position.now)
    peer.peer.scrollRectToVisible(new Rectangle(point.x - 8, point.y, 16, 2 * lineHeight))
    caret.steady.restart
  }
  // TODO: Compare with the event version
  buffer.length.changed || caret.visible.changed || //#EF //#EF //#IF //#IF
    caret.dot.changed || caret.mark.changed += { _ => this.repaint }
  //#IF //#IF //#EF //#HDL

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(SystemColor.text)
    g.fillRect(0, 0, size.now.width, size.now.height + lineHeight)

    val selStart = min(caret.dot.now, caret.mark.now)
    val selEnd = max(caret.dot.now, caret.mark.now)

    var lineIndex = 0
    var charIndex = 0
    for (line <- LineIterator(buffer.iterable.now)) {
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

    if (caret.visible.now) {
      def point = pointFromPosition(caret.position.now)
      g.setColor(SystemColor.textText)
      g.drawLine(point.x, point.y + lineHeight - font.now.getSize, point.x, point.y + lineHeight)
    }
  }
}

object TextArea {
  implicit def toComponent(input: TextArea): Component = input.peer
}
