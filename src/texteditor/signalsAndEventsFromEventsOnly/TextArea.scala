package texteditor.signalsAndEventsFromEventsOnly

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
import scala.swing.event.KeyPressed
import scala.swing.event.KeyTyped
import scala.swing.event.MouseDragged
import scala.swing.event.MouseEvent

import makro.SignalMacro.{SignalM => Signal}
import rescala.Var
import rescala.Signal
import rescala.events.ImperativeEvent
import reswing.ReComponent
import reswing.ReSwingValue
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
  
  // Used in from the superclass, note the override.
  override val preferredSize: ReSwingValue[Dimension] = Signal{ //#SIG
    def it = LineIterator(buffer.iterable())
    new Dimension(2 * padding + it.map(stringWidth(_)).max, (it.size + 1) * lineHeight)
  }
  preferredSize using (peer.preferredSize _, peer.preferredSize_= _, "preferredSize")
  
  val charCount = Signal{ buffer.length() } //#SIG
  
  val lineCount = Signal{ LineIterator(buffer.iterable()).size } //#SIG
  
  val wordCount = Signal{ buffer.iterable().iterator.foldLeft((0, false)){(c, ch) => //#SIG
    val alphanum = Character.isLetterOrDigit(ch)
    (if (alphanum && !c._2) c._1 + 1 else c._1, alphanum)}._1
  }
  
  val selected = Signal{  //#SIG
    val (it, dot, mark) = (buffer.iterable(), caret.dot(), caret.mark())
    val (start, end) = (min(dot, mark), max(dot, mark))
    new Iterable[Char] { def iterator = it.iterator.slice(start, end) } : Iterable[Char]
  }
  
  protected val selectedAll = new ImperativeEvent[Unit]  //#EVT
  protected val pasted = new ImperativeEvent[Unit]  //#EVT
  protected val copied = new ImperativeEvent[Unit]   //#EVT
  
  def selectAll = selectedAll()
  def paste = pasted()
  def copy = copied()
  
  // A caret has a position in the document referred to as a dot.
  // The dot is where the caret is currently located in the model.
  // There is a second position maintained by the caret that represents
  // the other end of a selection called mark.
  // If there is no selection the dot and mark will be equal.
  // [same semantics as for: javax.swing.text.Caret]
  object caret {
    // dot as offset
    private val dotSignal = Signal{ buffer.caret() } //#SIG
    def dot = dotSignal
    def dot_=(value: Int) = buffer.caretChanged(value)
    
    // dot as position (row and column)
    private val dotPosSignal = Signal{ LineOffset.position(buffer.iterable(), dot()) } //#SIG
    def dotPos = dotPosSignal
    def dotPos_=(value: Position) = dot = LineOffset.offset(buffer.iterable.get, value)
    
    private val markVar = Var(0) //#VAR
    
    // mark as offset
    private val markSignal = Signal{ markVar() } //#SIG
    def mark = markSignal
    def mark_=(value: Int) = if (value >= 0 && value <= buffer.length.get) markVar() = value
    
    // mark as position (row and column)
    private val markPosSignal = Signal{ LineOffset.position(buffer.iterable(), mark()) } //#SIG
    def markPos = markPosSignal
    def markPos_=(value: Position) = mark = LineOffset.offset(buffer.iterable.get, value)
    
    // caret location as offset
    def offset = dot
    def offset_=(value: Int) {
      dot = value
      mark = value
    }
    
    // caret location as position (row and column)
    def position = dotPos
    def position_=(value: Position) = offset = LineOffset.offset(buffer.iterable.get, value)
    
    protected[TextArea] val blink = new Timer(500) start
    protected[TextArea] val steady = new Timer(500, false)
    protected[TextArea] val visible: Signal[Boolean] = Signal{ hasFocus() }.toggle(blink.fired)(  //#SIG  //#IF
        Signal{ hasFocus() && steady.running() }) //#SIG
  }
  
  protected def posInLinebreak(p: Int) = p > 0 && p < buffer.length.get &&
    buffer(p - 1) == '\r' && buffer(p) == '\n'
  
  protected def pointFromPosition(position: Position) = {
    val line = LineIterator(buffer.iterable.get).drop(position.row).next
    val y = position.row * lineHeight
    val x = stringWidth(line.substring(0, position.col))
    new Point(x + padding, y)
  }
  
  protected def positionFromPoint(point: Point) = {
    val row = point.y / lineHeight
    val it = LineIterator(buffer.iterable.get).drop(row)
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
  
  // Caret updated by pressed mouse button, pressed arrow keys, Ctrl+A or select all event
  (keys.pressed && {e => e.modifiers != Key.Modifier.Control &&  //#EF
      (e.key == Key.Left || e.key == Key.Right || e.key == Key.Up || e.key == Key.Down ||
       e.key == Key.Home || e.key == Key.End)})
    .map{e: KeyPressed =>  //#EF
      val offset = e.key match {
        case Key.Left =>
          caret.offset.get - (if (posInLinebreak(caret.offset.get - 1)) 2 else 1)
        case Key.Right =>
          caret.offset.get + (if (posInLinebreak(caret.offset.get + 1)) 2 else 1)
        case Key.Up =>
          val position = Position(max(0, caret.position.get.row - 1), caret.position.get.col)
          LineOffset.offset(buffer.iterable.get, position)
        case Key.Down =>
          val position = Position(min(lineCount.get - 1, caret.position.get.row + 1), caret.position.get.col)
          LineOffset.offset(buffer.iterable.get, position)
        case Key.Home =>
          var offset = 0
          for ((ch, i) <- buffer.iterable.get.iterator.zipWithIndex)
            if (i < caret.offset.get && (ch == '\r' || ch == '\n'))
              offset = i + 1;
          offset
        case Key.End =>
          caret.offset.get +
	          buffer.iterable.get.iterator.drop(caret.offset.get).takeWhile{
	            ch => ch != '\r' && ch != '\n'
	          }.size
      }
      if (e.modifiers == Key.Modifier.Shift) (offset, caret.mark.get) else (offset, offset)
    } ||  //#EF
  (keys.pressed && {e => e.modifiers == Key.Modifier.Control && e.key == Key.A}) //#EF
    .map{_: KeyPressed => (charCount.get, 0)} ||  //#EF
  (mouse.clicks.pressed || mouse.moves.dragged).map{e: MouseEvent => //#EF //#EF //#EF
      val position = positionFromPoint(e.point)
      val offset = LineOffset.offset(buffer.iterable.get, position)
      e match { case _: MouseDragged => (offset, caret.mark.get) case _ => (offset, offset) }
    } ||  //#EF
  selectedAll.map{_: Unit => (charCount.get, 0)} +=  //#HDL
  { _ match {
    case (dot, mark) =>
      caret.dot = dot
      caret.mark = mark
    }
  }
  
  // Content change by pressed backspace, deletion or character key, Ctrl+V or paste event
  (((keys.pressed && {e => e.modifiers == Key.Modifier.Control && e.key == Key.V}) || pasted) && //#EF //#EF //#EF
    {_ => clipboard.getContents(null).isDataFlavorSupported(DataFlavor.stringFlavor)})
    .map{_: Any =>  //#EF
      (0, clipboard.getContents(null).getTransferData(DataFlavor.stringFlavor).asInstanceOf[String])} || //#EF
  (keys.typed && {e => e.modifiers != Key.Modifier.Control})  //#EF
    .map{(_: KeyTyped).char match {  //#EF
      case '\b' => if (selected.get.nonEmpty) (0, "")
        else (-min(if (posInLinebreak(caret.dot.get - 1)) 2 else 1, caret.dot.get), "")
      case '\u007f' => if (selected.get.nonEmpty) (0, "")
        else ((if (posInLinebreak(caret.dot.get + 1)) 2 else 1), "")
      case c => (0, c.toString)
    }} +=   //#HDL
  { _ match {
    case (del, ins) =>
      val selStart = min(caret.dot.get, caret.mark.get)
      val selEnd = max(caret.dot.get, caret.mark.get)
      caret.offset = selStart
      buffer.remove(selEnd - selStart)
      
      if (del < 0)
        caret.offset = caret.offset.get + del
      buffer.remove(math.abs(del))
      buffer.insert(ins)
      caret.offset = caret.offset.get + ins.length
    }
  }
  
  // Content copy by Ctrl+C or copy event
  copied || (keys.pressed && {e => e.modifiers == Key.Modifier.Control && e.key == Key.C}) += //#EF //#EF //#HDL
  { _ =>
    if (selected.get.nonEmpty) {
      val s = new StringSelection(selected.get.mkString);
      clipboard.setContents(s, s);
    }
  }
  
  // handle focus, scroll and paint updates
  mouse.clicks.pressed += { _ => this.requestFocusInWindow }  //#HDL
  
  buffer.length.changed || caret.dot.changed += {_ =>  //#EF //#HDL  //#IF //#IF
    val point = pointFromPosition(caret.position.get)
    peer.peer.scrollRectToVisible(new Rectangle(point.x - 8, point.y, 16, 2 * lineHeight))
    caret.steady.restart
  }
  // TODO: Compare with the event version
  buffer.length.changed || caret.visible.changed || //#EF //#EF //#IF //#IF
    caret.dot.changed || caret.mark.changed += {_ => this.repaint }  //#IF //#IF //#EF //#HDL
  
  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(SystemColor.text)
    g.fillRect(0, 0, size.get.width, size.get.height + lineHeight)
    
    val selStart = min(caret.dot.get, caret.mark.get)
    val selEnd = max(caret.dot.get, caret.mark.get)
    
    var lineIndex = 0
    var charIndex = 0
    for (line <- LineIterator(buffer.iterable.get)) {
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
        g.fillRect(middleX, lineIndex * lineHeight + lineHeight - font.get.getSize, endX - middleX, lineHeight)
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
    
    if (caret.visible.get) {
      def point = pointFromPosition(caret.position.get)
      g.setColor(SystemColor.textText)
      g.drawLine(point.x, point.y + lineHeight - font.get.getSize, point.x, point.y + lineHeight)
    }
  }
}

object TextArea {
  implicit def toComponent(input : TextArea) : Component = input.peer
}
