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
  
  val selected: ReSwingValue[String] = ()
  
  (selected using (peer.selected _, (peer, classOf[CaretUpdate])))
  
  (text using (peer.text _, peer.text_= _, (peer, classOf[ValueChanged]))
        force ("editable", peer.editable_= _, false))
  
  class ReCaret {
    protected lazy val peer = ReTextComponent.this.peer.caret
    
    val dot: ReSwingValue[Int] = ()
    val mark: ReSwingValue[Int] = ()
    val position: ReSwingValue[Int] = ()
    
    dot using (peer.dot _, (peer, classOf[CaretUpdate]))
    mark using (peer.mark _, (peer, classOf[CaretUpdate]))
    position using (peer.position _, (peer, classOf[CaretUpdate]))
  }
  
  object ReCaret {
    implicit def toCaret(caret: ReCaret) = caret.peer
  }
  
  object caret extends ReCaret
}

object ReTextComponent {
  implicit def toTextComponent(component: ReTextComponent): TextComponent = component.peer
}
