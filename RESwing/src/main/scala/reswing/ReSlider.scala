package reswing

import scala.language.implicitConversions
import scala.swing.Alignment
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.Label
import scala.swing.Slider
import scala.swing.event.ValueChanged

class ReSlider(
    val min: ReSwingValue[Int] = (),
    val max: ReSwingValue[Int] = (),
    val value: ReSwingValue[Int] = (),
    val extent: ReSwingValue[Int] = (),
    val paintLabels: ReSwingValue[Boolean] = (),
    val paintTicks: ReSwingValue[Boolean] = (),
    val paintTrack: ReSwingValue[Boolean] = (),
    val snapToTicks: ReSwingValue[Boolean] = (),
    val minorTickSpacing: ReSwingValue[Int] = (),
    val majorTickSpacing: ReSwingValue[Int] = (),
    val labels: ReSwingValue[Map[Int, Label]] = (),
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
  override protected lazy val peer = new Slider with ComponentMixin
  
  min using (peer.min _, peer.min_= _, "minimum")
  max using (peer.max _, peer.max_= _, "maximum")
  value using (peer.value _, peer.value_= _, classOf[ValueChanged])
  extent using (peer.extent _, peer.extent_= _)
  
  paintLabels using (peer.paintLabels _, peer.paintLabels_= _, "paintLabels")
  paintTicks using (peer.paintTicks _, peer.paintTicks_= _, "paintTicks")
  paintTrack using (peer.paintTrack _, peer.paintTrack_= _, "paintTrack")
  
  snapToTicks using (peer.snapToTicks _, peer.snapToTicks_= _, "snapToTicks")
  
  minorTickSpacing using (peer.minorTickSpacing _, peer.minorTickSpacing_= _, "minorTickSpacing")
  majorTickSpacing using (peer.majorTickSpacing _, peer.majorTickSpacing_= _, "majorTickSpacing")
  
  labels using (
      { () => if (peer.peer.getLabelTable != null) peer.labels.toMap else null },
      peer.labels_= _,
      "labelTable")
}

object ReSlider {
  implicit def toSlider(component: ReSlider): Slider = component.peer
}
