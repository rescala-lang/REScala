package texteditor.signalsAndEventsFromImperative

import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Point
import java.awt.Rectangle
import java.awt.SystemColor
import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.StringSelection

import scala.math.max
import scala.math.min
import scala.swing.Component
import scala.swing.event.Key

import macro.SignalMacro.{SignalM => Signal}
import react.SignalSynt
import react.Var
import reswing.ImperativeSignal
import reswing.ReComponent
import texteditor.JScrollableComponent
import texteditor.LineIterator
import texteditor.LineOffset
import texteditor.Position

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
  
  override lazy val preferredSize: ImperativeSignal[Dimension] = Signal{
    def it = LineIterator(buffer.iterable())
    new Dimension(2 * padding + it.map(stringWidth(_)).max, (it.size + 1) * lineHeight)
  }
  
  val charCount = Signal{ buffer.length() }
  
  val lineCount = Signal{ LineIterator(buffer.iterable()).size }
  
  val wordCount = Signal{ buffer.iterable().iterator.foldLeft((0, false)){(c, ch) =>
    val alphanum = Character.isLetterOrDigit(ch)
    (if (alphanum && !c._2) c._1 + 1 else c._1, alphanum)}._1
  }
    
  val selected = Signal{
    val (it, dot, mark) = (buffer.iterable(), caret.dot(), caret.mark())
    val (start, end) = (min(dot, mark), max(dot, mark))
    new Iterable[Char] { def iterator = it.iterator.slice(start, end) } : Iterable[Char]
  }
  
  def selectAll {
    caret.dot = charCount.getValue
    caret.mark = 0
  }
  
  def paste {
    removeSelection
    val c = clipboard.getContents(null);
    if (c.isDataFlavorSupported(DataFlavor.stringFlavor)) {
      val str = c.getTransferData(DataFlavor.stringFlavor).asInstanceOf[String]
      buffer.insert(str)
      caret.offset = caret.offset.getValue + str.length
    }
  }
  
  def copy {
    if (selected.getValue.nonEmpty) {
      val s = new StringSelection(selected.getValue.mkString)
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
    // dot as offset
    private val dotSignal = Signal{ buffer.caret() }
    def dot = dotSignal
    def dot_=(value: Int) = buffer.caretChanged(value)
    
    // dot as position (row and column)
    private val dotPosSignal = Signal{ LineOffset.position(buffer.iterable(), dot()) }
    def dotPos = dotPosSignal
    def dotPos_=(value: Position) = dot = LineOffset.offset(buffer.iterable.getValue, value)
    
    private val markVar = Var(0)
    
    // mark as offset
    private val markSignal = Signal{ markVar() }
    def mark = markSignal
    def mark_=(value: Int) = if (value >= 0 && value <= buffer.length.getValue) markVar() = value
    
    // mark as position (row and column)
    private val markPosSignal = Signal{ LineOffset.position(buffer.iterable(), mark()) }
    def markPos = markPosSignal
    def markPos_=(value: Position) = mark = LineOffset.offset(buffer.iterable.getValue, value)
    
    // caret location as offset
    def offset = dot
    def offset_=(value: Int) {
      dot = value
      mark = value
    }
    
    // caret location as position (row and column)
    def position = dotPos
    def position_=(value: Position) = offset = LineOffset.offset(buffer.iterable.getValue, value)
    
    protected[TextArea] val blink = new Timer(500) start
    protected[TextArea] val steady = new Timer(500, false)
    protected[TextArea] val visible = Signal{ hasFocus() }.toggle(blink.fired)(
        Signal{ hasFocus() && steady.running() })
  }
  
  protected def posInLinebreak(p: Int) = p > 0 && p < buffer.length.getValue &&
    buffer(p - 1) == '\r' && buffer(p) == '\n'
  
  protected def removeSelection {
    val selStart = min(caret.dot.getValue, caret.mark.getValue)
    val selEnd = max(caret.dot.getValue, caret.mark.getValue)
    caret.offset = selStart
    buffer.remove(selEnd - selStart)
  }
  
  protected def pointFromPosition(position: Position) = {
    val line = LineIterator(buffer.iterable.getValue).drop(position.row).next
    val y = position.row * lineHeight
    val x = stringWidth(line.substring(0, position.col))
    new Point(x + padding, y)
  }
  
  protected def positionFromPoint(point: Point) = {
    val row = point.y / lineHeight
    val it = LineIterator(buffer.iterable.getValue).drop(row)
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
  
  keys.pressed += { e =>
    def shift = e.modifiers == Key.Modifier.Shift
    if (e.modifiers == Key.Modifier.Control)
      e.key match {
        case Key.V => paste
        case Key.C => copy
        case Key.A => selectAll
        case _ =>
      }
    else
      e.key match {
        case Key.Left =>
          val offset = caret.offset.getValue - (if (posInLinebreak(caret.offset.getValue - 1)) 2 else 1)
          if (shift) caret.dot = offset else caret.offset = offset
        case Key.Right =>
          val offset = caret.offset.getValue + (if (posInLinebreak(caret.offset.getValue + 1)) 2 else 1)
          if (shift) caret.dot = offset else caret.offset = offset
        case Key.Up =>
          val position = Position(max(0, caret.position.getValue.row - 1), caret.position.getValue.col)
          if (shift) caret.dotPos = position else caret.position = position
        case Key.Down =>
          val position = Position(min(lineCount.getValue - 1, caret.position.getValue.row + 1), caret.position.getValue.col)
          if (shift) caret.dotPos = position else caret.position = position
        case Key.Home =>
          var offset = 0
          for ((ch, i) <- buffer.iterable.getValue.iterator.zipWithIndex)
            if (i < caret.offset.getValue && (ch == '\r' || ch == '\n'))
              offset = i + 1;
          if (shift) caret.dot = offset else caret.offset = offset
        case Key.End =>
          val offset = 
            caret.offset.getValue +
	          buffer.iterable.getValue.iterator.drop(caret.offset.getValue).takeWhile{
	            ch => ch != '\r' && ch != '\n'
	          }.size
	      if (shift) caret.dot = offset else caret.offset = offset
        case _ =>
      }
  }
  
  keys.typed += { e =>
    if (e.modifiers != Key.Modifier.Control)
      e.char match {
        case '\u007f' => // Del key
          if (selected.getValue.isEmpty) {
            val count = if (posInLinebreak(caret.dot.getValue + 1)) 2 else 1
            buffer.remove(count);
          }
          else removeSelection
        case '\b' => // Backspace key
          if (selected.getValue.isEmpty) {
            val count = min(if (posInLinebreak(caret.dot.getValue - 1)) 2 else 1, caret.dot.getValue)
            caret.offset = caret.offset.getValue - count
            buffer.remove(count);
          }
          else removeSelection
        case c => // character input
          removeSelection
          buffer.insert(c.toString)
          caret.offset = caret.offset.getValue + 1
      }
  }
  
  mouse.clicks.pressed += { e =>
    this.requestFocusInWindow
    caret.position = positionFromPoint(e.point)
  }
  
  mouse.moves.dragged += { e =>
    caret.dotPos = positionFromPoint(e.point)
  }
  
  // handle scroll and paint updates
  caret.position.changed += {_ =>
    val point = pointFromPosition(caret.position.getValue)
    peer.peer.scrollRectToVisible(new Rectangle(point.x - 8, point.y, 16, 2 * lineHeight))
    caret.steady.restart
  }
  
  buffer.length.changed || caret.visible.changed ||
    caret.dot.changed || caret.mark.changed += {_ => this.repaint }
  
  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(SystemColor.text)
    g.fillRect(0, 0, size.getValue.width, size.getValue.height + lineHeight)
    
    val selStart = min(caret.dot.getValue, caret.mark.getValue)
    val selEnd = max(caret.dot.getValue, caret.mark.getValue)
    
    var lineIndex = 0
    var charIndex = 0
    for (line <- LineIterator(buffer.iterable.getValue)) {
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
        g.fillRect(middleX, lineIndex * lineHeight + lineHeight - this.font.getSize, endX - middleX, lineHeight)
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
    
    if (caret.visible.getValue) {
      def point = pointFromPosition(caret.position.getValue)
      g.setColor(SystemColor.textText)
      g.drawLine(point.x, point.y + lineHeight - this.font.getSize, point.x, point.y + lineHeight)
    }
  }
}

object TextArea {
  implicit def toComponent(input : TextArea) : Component = input.peer
}
