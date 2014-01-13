package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.TextField
import scala.swing.event.EditDone
import react.events.Event

class ReTextField(
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
    columns: Int = 0)
  extends
    ReTextComponent(text, editable, cut, copy, paste, selectAll,
                    background, foreground, font, enabled,
                    minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer =
    new TextField(if (text == null) null else text.getValue, columns) with ComponentMixin
  
  val editDone = ReSwingEvent using classOf[EditDone]
}

object ReTextField {
  implicit def toTextField(component: ReTextField): TextField = component.peer
}
