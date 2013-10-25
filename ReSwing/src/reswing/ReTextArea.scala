package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.TextArea
import scala.swing.event.ValueChanged

class ReTextArea(
    text: ReSwingValue[String] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = (),
    rows: Int = 0,
    columns: Int = 0)
  extends
    ReTextComponent(text, background, foreground, font, enabled,
                    minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer = new TextArea(text.getValue, rows, columns) with ComponentMixin
  
  val lineCount = ReSwingValue using (peer.lineCount _, classOf[ValueChanged])
}

object ReTextArea {
  implicit def toTextArea(component: ReTextArea): TextArea = component.peer
}
