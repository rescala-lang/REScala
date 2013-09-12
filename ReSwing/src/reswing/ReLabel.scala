package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.Label

class ReLabel(
    val text: ReSwingValue[String] = ReSwingValue.noValue,
    background: ReSwingValue[Color] = ReSwingValue.noValue,
    foreground: ReSwingValue[Color] = ReSwingValue.noValue,
    font: ReSwingValue[Font] = ReSwingValue.noValue,
    enabled: ReSwingValue[Boolean] = ReSwingValue.noValue,
    minimumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    maximumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    preferredSize: ReSwingValue[Dimension] = ReSwingValue.noValue)
  extends
    ReComponent(background, foreground, font, enabled,
                minimumSize, maximumSize, preferredSize) {
  
  override protected lazy val peer = new Label with ComponentMixin
  
  text using (peer.text _, peer.text_= _, "text")
}

object ReLabel {
  implicit def toLabel(component: ReLabel): Label = component.peer
}
