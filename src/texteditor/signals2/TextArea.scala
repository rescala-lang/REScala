package texteditor.signals2

import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Point
import java.awt.Rectangle
import java.awt.SystemColor
import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.StringSelection

import scala.events.Event
import scala.events.ImperativeEvent
import scala.events.behaviour.Signal
import scala.math.max
import scala.math.min
import scala.swing.Component
import scala.swing.event.Key
import scala.swing.event.KeyPressed
import scala.swing.event.KeyTyped
import scala.swing.event.MouseDragged
import scala.swing.event.MouseEvent

import reswing.ReComponent
import texteditor.JScrollableComponent
import texteditor.LineIterator
import texteditor.LineOffset
import texteditor.Position

class TextArea(text: String) extends ReComponent {
  override protected lazy val peer = new Component with ComponentMixin {
    override lazy val peer: JScrollableComponent = new JScrollableComponent with SuperMixin
  }
  
  import peer.peer.metrics.stringWidth
  import peer.peer.{unitHeight => lineHeight}
  
  protected val padding = 5
  protected val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
  
  def this() = this("")
  
  Signal{
    def it = LineIterator(content())
    new Dimension(2 * padding + it.map(stringWidth(_)).max, (it.size + 1) * lineHeight)
  }.changed += { v => peer preferredSize = v }
  
  // keeps track of the current text content and the number of modifications
  protected lazy val contentModification: Signal[(Int, String)] =
    contentChanged.fold((0, text)){(content, change) =>
      val (del, ins) = change
      val (count, content0) = content
      
      val selStart = min(caret.dot.getValue, caret.mark.getValue)
      val selEnd = max(caret.dot.getValue, caret.mark.getValue)
      val content1 = if (selStart == selEnd) content0 else
        content0.substring(0, selStart) + content0.substring(selEnd)
      
      val delStart = if (del < 0) selStart + del else selStart
      val delEnd = if (del < 0) selStart else selStart + del
      val content2 = if (delStart == delEnd) content1 else
        content1.substring(0, delStart) + content1.substring(delEnd)
      
      (count + 1, content2.substring(0, delStart) + ins + content2.substring(delStart))
    }
  
  // text content
  protected lazy val content: Signal[String] = Signal{ contentModification()._2 }
  
  lazy val charCount = Signal{ content().length }
  
  lazy val lineCount = Signal{ LineIterator(content()).size }
  
  lazy val wordCount = Signal{ content().foldLeft((0, false)){(c, ch) =>
    val alphanum = Character.isLetterOrDigit(ch)
    (if (alphanum && !c._2) c._1 + 1 else c._1, alphanum)}._1
  }
  
  lazy val selected = Signal{
    val (it, dot, mark) = (content(), caret.dot(), caret.mark())
    val (start, end) = (min(dot, mark), max(dot, mark))
    new Iterable[Char] { def iterator = it.iterator.slice(start, end) } : Iterable[Char]
  }
  
  protected lazy val selectedAll = new ImperativeEvent[Unit]
  protected lazy val pasted = new ImperativeEvent[Unit]
  protected lazy val copied = new ImperativeEvent[Unit]
  
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
    protected[TextArea] lazy val changed = new ImperativeEvent[(Int, Int)]
    protected[TextArea] lazy val caret = caretChanged.fold((0, 0)){(oldpos, newpos) =>
      val (olddot, oldmark) = oldpos
      val (newdot, newmark) = newpos
      
      val dot = if (newdot >= 0 && newdot <= content.getValue.length) newdot else olddot
      val mark = if (newmark >= 0 && newmark <= content.getValue.length) newmark else oldmark
      (dot, mark)
    }
    
    // dot as offset
    lazy val dot = Signal{ caret()._1 }
    def changeDot(value: Int) = changed(value, mark.getValue)
    
    // dot as position (row and column)
    lazy val dotPos = Signal{ LineOffset.position(content(), dot()) }
    def changeDotPos(value: Position) = changeDot(LineOffset.offset(content.getValue, value))
    
    // mark as offset
    lazy val mark = Signal{ caret()._2 }
    def changeMark(value: Int) = changed(dot.getValue, value)
    
    // mark as position (row and column)
    lazy val markPos = Signal{ LineOffset.position(content(), mark()) }
    def changeMarkPos(value: Position) = changeMark(LineOffset.offset(content.getValue, value))
    
    // caret location as offset
    def offset = dot
    def changeOffset(value: Int) = changed(value, value)
    
    // caret location as position (row and column)
    def position = dotPos
    def changePosition(value: Position) = changeOffset(LineOffset.offset(content.getValue, value))
    
    protected[TextArea] val blink = new Timer(500) start
    protected[TextArea] val steady = new Timer(500, false)
    protected[TextArea] val visible = Signal{ hasFocus() }.toggle(blink.fired)(
        Signal{ hasFocus() && steady.running() })
  }
  
  protected def posInLinebreak(p: Int) = p > 0 && p < content.getValue.length &&
    content.getValue(p - 1) == '\r' && content.getValue(p) == '\n'
  
  protected def pointFromPosition(position: Position) = {
    val line = LineIterator(content.getValue).drop(position.row).next
    val y = position.row * lineHeight
    val x = stringWidth(line.substring(0, position.col))
    new Point(x + padding, y)
  }
  
  protected def positionFromPoint(point: Point) = {
    val row = point.y / lineHeight
    val it = LineIterator(content.getValue).drop(row)
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
  protected lazy val caretChanged: Event[(Int, Int)] =
    (keys.pressed && {e => e.modifiers != Key.Modifier.Control &&
        (e.key == Key.Left || e.key == Key.Right || e.key == Key.Up || e.key == Key.Down ||
         e.key == Key.Home || e.key == Key.End)})
      .map{e: KeyPressed =>
        val offset = e.key match {
          case Key.Left =>
            caret.offset.getValue - (if (posInLinebreak(caret.offset.getValue - 1)) 2 else 1)
          case Key.Right =>
            caret.offset.getValue + (if (posInLinebreak(caret.offset.getValue + 1)) 2 else 1)
          case Key.Up =>
            val position = Position(max(0, caret.position.getValue.row - 1), caret.position.getValue.col)
            LineOffset.offset(content.getValue, position)
          case Key.Down =>
            val position = Position(min(lineCount.getValue - 1, caret.position.getValue.row + 1), caret.position.getValue.col)
            LineOffset.offset(content.getValue, position)
          case Key.Home =>
           var offset = 0
            for ((ch, i) <- content.getValue.zipWithIndex)
              if (i < caret.offset.getValue && (ch == '\r' || ch == '\n'))
                offset = i + 1;
            offset
          case Key.End =>
            caret.offset.getValue +
	            content.getValue.drop(caret.offset.getValue).takeWhile{
	              ch => ch != '\r' && ch != '\n'
	           }.size
        }
        if (e.modifiers == Key.Modifier.Shift) (offset, caret.mark.getValue) else (offset, offset)
      } ||
    (keys.pressed && {e => e.modifiers == Key.Modifier.Control && e.key == Key.A})
      .map{_: KeyPressed => (charCount.getValue, 0)} ||
    (mouse.clicks.pressed || mouse.moves.dragged).map{e: MouseEvent =>
        val position = positionFromPoint(e.point)
        val offset = LineOffset.offset(content.getValue, position)
        e match { case _: MouseDragged => (offset, caret.mark.getValue) case _ => (offset, offset) }
      } ||
    contentChanged.then(contentModification.changed)
      .map{change: ((Int, String), _) =>
        val ((del, ins), _) = change
        
        val selStart = min(caret.dot.getValue, caret.mark.getValue)
        val offset = ins.length + (if (del < 0) selStart + del else selStart)
        
        (offset, offset)
      } ||
    selectedAll.map{_: Unit => (0, charCount.getValue)} ||
    caret.changed
  
  // Content change by pressed backspace, deletion or character key, Ctrl+V or paste event
  protected lazy val contentChanged: Event[(Int, String)] =
    (((keys.pressed && {e => e.modifiers == Key.Modifier.Control && e.key == Key.V}) || pasted) &&
      {_ => clipboard.getContents(null).isDataFlavorSupported(DataFlavor.stringFlavor)})
      .map{_: Any =>
        (0, clipboard.getContents(null).getTransferData(DataFlavor.stringFlavor).asInstanceOf[String])} ||
    (keys.typed && {e => e.modifiers != Key.Modifier.Control})
      .map{(_: KeyTyped).char match {
        case '\b' => if (selected.getValue.nonEmpty) (0, "")
          else (-min(if (posInLinebreak(caret.dot.getValue - 1)) 2 else 1, caret.dot.getValue), "")
        case '\u007f' => if (selected.getValue.nonEmpty) (0, "")
          else ((if (posInLinebreak(caret.dot.getValue + 1)) 2 else 1), "")
        case c => (0, c.toString)
      }}
  
  // Content copy by Ctrl+C or copy event
  copied || (keys.pressed && {e => e.modifiers == Key.Modifier.Control && e.key == Key.C}) += { _ =>
    if (selected.getValue.nonEmpty) {
      val s = new StringSelection(selected.getValue.mkString);
      clipboard.setContents(s, s);
    }
  }
  
  // handle focus, scroll and paint updates
  mouse.clicks.pressed += { _ => this.requestFocusInWindow }
  
  caret.position.changed += {_ =>
    val point = pointFromPosition(caret.position.getValue)
    peer.peer.scrollRectToVisible(new Rectangle(point.x - 8, point.y, 16, 2 * lineHeight))
    caret.steady.restart
  }
  
  content.changed || caret.visible.changed ||
    caret.dot.changed || caret.mark.changed += {_ => this.repaint }
  
  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    g.setColor(SystemColor.text)
    g.fillRect(0, 0, size.getValue.width, size.getValue.height + lineHeight)
    
    val selStart = min(caret.dot.getValue, caret.mark.getValue)
    val selEnd = max(caret.dot.getValue, caret.mark.getValue)
    
    var lineIndex = 0
    var charIndex = 0
    for (line <- LineIterator(content.getValue)) {
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
