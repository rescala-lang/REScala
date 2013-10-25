package reswing

import scala.language.implicitConversions
import scala.swing.AbstractButton
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.event.ButtonClicked

class ReAbstractButton(
    val text: ReSwingValue[String] = (),
    background: ReSwingValue[Color] = (),
    foreground: ReSwingValue[Color] = (),
    font: ReSwingValue[Font] = (),
    enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReComponent(background, foreground, font, enabled,
                minimumSize, maximumSize, preferredSize) {
  override protected lazy val peer = new AbstractButton with ComponentMixin
  
  text using (peer.text _, peer.text_= _, "text")
  
  val clicked = ReSwingEvent using classOf[ButtonClicked]
}

object ReAbstractButton {
  implicit def toAbstractButton(component: ReAbstractButton): AbstractButton = component.peer
}
