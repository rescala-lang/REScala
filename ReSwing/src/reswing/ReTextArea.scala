package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.TextArea
import scala.swing.event.ValueChanged
import react.events.Event

class ReTextArea(
    text: ReSwingValue[String] = (),
    editable: ReSwingValue[Boolean] = (),
    cut: ReSwingEvent[Unit] = (),
    copy: ReSwingEvent[Unit] = (),
    paste: ReSwingEvent[Unit] = (),
    selectAll: ReSwingEvent[Unit] = (),
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
    ReTextComponent(text, editable, cut, copy, paste, selectAll,
                    background, foreground, font, enabled,
                    minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer =
    new TextArea(if (text == null) null else text.getValue, rows, columns) with ComponentMixin
  
  val lineCount = ReSwingValue using (peer.lineCount _, classOf[ValueChanged])
}

object ReTextArea {
  implicit def toTextArea(component: ReTextArea): TextArea = component.peer
}
