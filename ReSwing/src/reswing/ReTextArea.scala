package reswing

import java.awt.Dimension

import scala.swing.TextArea
import scala.swing.event.ValueChanged

class ReTextArea(text: String = "", rows: Int = 0, columns: Int = 0) extends ReTextComponent {
  override protected lazy val peer = new TextArea(text, rows, columns) with ComponentMixin
  
  val lineCount: ImperativeSignal[Int] = ImperativeSignal.noSignal(peer.lineCount)
  peer.reactions += {
    case e @ ValueChanged(_) => lineCount(peer.lineCount)
  }
}

object ReTextArea {
  implicit def toTextArea(input : ReTextArea) : TextArea = input.peer
  
  def apply(
      text: String = "",
      rows: Int = 0,
      columns: Int = 0,
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal) = Macros.applyBody
}
