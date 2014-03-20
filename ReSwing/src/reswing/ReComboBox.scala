package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.ComboBox
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.event.SelectionChanged

class ReComboBox[A](
    items: Seq[A],
    `selection.index`: ReSwingValue[Int] = (),
    `selection.item`: ReSwingValue[A] = (),
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
  override protected lazy val peer = new ComboBox[A](items) with ComponentMixin
  
  final case class Leaf
  
  class ReSelection(
      val index: ReSwingValue[Int],
      val item: ReSwingValue[A]) {
    protected[ReComboBox] val peer = ReComboBox.this.peer.selection
    
    index using (peer.index _, peer.index= _, (peer, classOf[SelectionChanged]))
    item using (peer.item _, peer.item= _, (peer, classOf[SelectionChanged]))
    
    val changed = ReSwingEvent using (peer, classOf[SelectionChanged])
  }
  
  object ReSelection {
    implicit def toSelection(selection: ReSelection) = selection.peer
  }
  
  object selection extends ReSelection(`selection.index`, `selection.item`)
}

object ReComboBox {
  implicit def toComboBox[A](component: ReComboBox[A]): ComboBox[A] = component.peer
}
