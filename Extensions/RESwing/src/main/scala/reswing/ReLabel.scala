package reswing

import javax.swing.Icon

import scala.language.implicitConversions
import scala.swing.{Alignment, Color, Dimension, Font, Label}

class ReLabel(
    val text: ReSwingValue[String] = (),
    val horizontalAlignment: ReSwingValue[Alignment.Value] = (),
    val verticalAlignment: ReSwingValue[Alignment.Value] = (),
    val horizontalTextPosition: ReSwingValue[Alignment.Value] = (),
    val verticalTextPosition: ReSwingValue[Alignment.Value] = (),
    val icon: ReSwingValue[Icon] = (),
    val disabledIcon: ReSwingValue[Icon] = (),
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
  override protected lazy val peer = new Label with ComponentMixin

  text using (peer.text _, peer.text_= _, "text")

  horizontalAlignment using (peer.horizontalAlignment _,
                             peer.horizontalAlignment= _, "horizontalAlignment")
  verticalAlignment using (peer.verticalAlignment _,
                           peer.verticalAlignment= _, "verticalAlignment")
  horizontalTextPosition using (peer.horizontalTextPosition _,
                                peer.horizontalTextPosition= _, "horizontalTextPosition")
  verticalTextPosition using (peer.verticalTextPosition _,
                              peer.verticalTextPosition_= _, "verticalTextPosition")

  icon using (peer.icon _, peer.icon_= _, "icon")
  disabledIcon using (peer.disabledIcon _, peer.disabledIcon_= _, "disabledIcon")
}

object ReLabel {
  implicit def toLabel(component: ReLabel): Label = component.peer
}
