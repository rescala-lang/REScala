package reswing

import scala.language.implicitConversions
import scala.swing.AbstractButton
import scala.swing.Alignment
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.event.ButtonClicked

import javax.swing.Icon

class ReAbstractButton(
    val text: ReSwingValue[String] = (),
    val selected: ReSwingValue[Boolean] = (),
    val horizontalAlignment: ReSwingValue[Alignment.Value] = (),
    val verticalAlignment: ReSwingValue[Alignment.Value] = (),
    val horizontalTextPosition: ReSwingValue[Alignment.Value] = (),
    val verticalTextPosition: ReSwingValue[Alignment.Value] = (),
    val icon: ReSwingValue[Icon] = (),
    val pressedIcon: ReSwingValue[Icon] = (),
    val selectedIcon: ReSwingValue[Icon] = (),
    val disabledIcon: ReSwingValue[Icon] = (),
    val disabledSelectedIcon: ReSwingValue[Icon] = (),
    val rolloverIcon: ReSwingValue[Icon] = (),
    val rolloverSelectedIcon: ReSwingValue[Icon] = (),
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
  selected using (peer.selected _, peer.selected_= _, classOf[ButtonClicked])
  
  horizontalAlignment using (peer.horizontalAlignment _,
                             peer.horizontalAlignment= _, "horizontalAlignment")
  verticalAlignment using (peer.verticalAlignment _,
                           peer.verticalAlignment= _, "verticalAlignment")
  horizontalTextPosition using (peer.horizontalTextPosition _,
                                peer.horizontalTextPosition= _, "horizontalTextPosition")
  verticalTextPosition using (peer.verticalTextPosition _,
                              peer.verticalTextPosition_= _, "verticalTextPosition")
  
  icon using (peer.icon _, peer.icon_= _, "icon")
  pressedIcon using (peer.pressedIcon _, peer.pressedIcon_= _, "pressedIcon")
  selectedIcon using (peer.selectedIcon _, peer.selectedIcon_= _, "selectedIcon")
  disabledIcon using (peer.disabledIcon _, peer.disabledIcon_= _, "disabledIcon")
  disabledSelectedIcon using (peer.disabledSelectedIcon _, peer.disabledSelectedIcon_= _, "disabledSelectedIcon")
  rolloverIcon using (peer.rolloverIcon _, peer.rolloverIcon_= _, "rolloverIcon")
  rolloverSelectedIcon using (peer.rolloverSelectedIcon _, peer.rolloverSelectedIcon_= _, "rolloverSelectedIcon")
  
  val clicked = ReSwingEvent using classOf[ButtonClicked]
}

object ReAbstractButton {
  implicit def toAbstractButton(component: ReAbstractButton): AbstractButton = component.peer
}
