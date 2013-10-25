package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.TextComponent
import scala.swing.event.CaretUpdate
import scala.swing.event.ValueChanged

class ReTextComponent(
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
  override protected lazy val peer = new TextComponent with ComponentMixin
  
  val selected = ReSwingValue using (peer.selected _, classOf[CaretUpdate])
  
  (text using (peer.text _, peer.text_= _, classOf[ValueChanged])
        force ("editable", peer.editable_= _, false))
  
  class ReCaret {
    protected lazy val peer = ReTextComponent.this.peer.caret
    
    val dot = ReSwingValue using (peer.dot _, classOf[CaretUpdate])
    val mark = ReSwingValue using (peer.mark _, classOf[CaretUpdate])
    val position = ReSwingValue using (peer.position _, classOf[CaretUpdate])
  }
  
  object ReCaret {
    implicit def toCaret(caret: ReCaret) = caret.peer
  }
  
  object caret extends ReCaret
}

object ReTextComponent {
  implicit def toTextComponent(component: ReTextComponent): TextComponent = component.peer
}
