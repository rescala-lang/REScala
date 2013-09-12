package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.TextArea
import scala.swing.event.ValueChanged

class ReTextArea(
    text: ReSwingValue[String] = ReSwingValue.noValue,
    background: ReSwingValue[Color] = ReSwingValue.noValue,
    foreground: ReSwingValue[Color] = ReSwingValue.noValue,
    font: ReSwingValue[Font] = ReSwingValue.noValue,
    enabled: ReSwingValue[Boolean] = ReSwingValue.noValue,
    minimumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    maximumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    preferredSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    rows: Int = 0,
    columns: Int = 0)
  extends
    ReTextComponent(text, background, foreground, font, enabled,
                    minimumSize, maximumSize, preferredSize) {
  
  override protected lazy val peer = new TextArea(text.getValue, rows, columns) with ComponentMixin
  
  val lineCount: ReSwingValue[Int] = peer.lineCount
  
  peer.reactions += {
    case e @ ValueChanged(_) => lineCount() = peer.lineCount
  }
}

object ReTextArea {
  implicit def toTextArea(component: ReTextArea): TextArea = component.peer
}
