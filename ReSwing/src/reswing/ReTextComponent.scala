package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.TextComponent
import scala.swing.event.CaretUpdate
import scala.swing.event.ValueChanged
import react.events.Event

class ReTextComponent(
    val text: ReSwingValue[String] = (),
    val editable: ReSwingValue[Boolean] = (),
    cut: Event[Unit] = (),
    copy: Event[Unit] = (),
    paste: Event[Unit] = (),
    selectAll: Event[Unit] = (),
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
  override protected lazy val peer = new TextComponent with ComponentMixin
  
  val selected = ReSwingValue using (peer.selected _, (caret.peer, classOf[CaretUpdate]))
  
  (text using (peer.text _, peer.text_= _, classOf[ValueChanged])
        force ("editable", peer.editable_= _, false))
  
  editable using (peer.editable _, peer.editable_= _, "editable")
  
  cut using peer.cut _
  copy using peer.copy _
  paste using peer.paste _
  selectAll using peer.selectAll _
  
  class ReCaret {
    protected[ReTextComponent] val peer = ReTextComponent.this.peer.caret
    
    val dot = ReSwingValue using (peer.dot _, (peer, classOf[CaretUpdate]))
    val mark = ReSwingValue using (peer.mark _, (peer, classOf[CaretUpdate]))
    val position = ReSwingValue using (peer.position _, (peer, classOf[CaretUpdate]))
  }
  
  object ReCaret {
    implicit def toCaret(caret: ReCaret) = caret.peer
  }
  
  object caret extends ReCaret
}

object ReTextComponent {
  implicit def toTextComponent(component: ReTextComponent): TextComponent = component.peer
}
