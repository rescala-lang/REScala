package reswing

import scala.events.behaviour.Var
import scala.events.behaviour.Signal
import scala.swing.TextArea
import scala.swing.event.ValueChanged

class ReTextArea(text0: String, rows0: Int, columns0: Int) extends ReTextComponent {
  override protected lazy val peer =
    new TextArea(text0, rows0, columns0) with TextAreaMixin
  
  protected trait TextAreaMixin extends TextArea with TextComponentMixin {
    val reLineCount = new ReactiveWrapper(null, lineCount)
  }
  
  def this(text: String) = this(text, 0, 0)
  def this(rows: Int, columns: Int) = this("", rows, columns)
  def this() = this("", 0, 0)
  
  def lineCount = peer.reLineCount.signal
  
  peer.reactions += {
    case e @ ValueChanged(_) => peer.reLineCount.value = peer.lineCount
  }
}

object ReTextArea {
  implicit def toTextArea(input : ReTextArea) : TextArea = input.peer
}
