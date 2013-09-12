package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.TextComponent
import scala.swing.event.CaretUpdate
import scala.swing.event.ValueChanged

class ReTextComponent(
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
  
  override protected lazy val peer = new TextComponent with ComponentMixin
  
  text using (peer.text _, peer.text_= _, "text")
  text force ("editable", peer.editable_= _, false)
  
  val selected: ReSwingValue[String] = peer.selected
  
  peer.reactions += {
    case e @ ValueChanged(_) => text() = peer.text
  }
  
  peer.caret.reactions += {
    case e @ CaretUpdate(_) => selected() = peer.selected
  }
  
  class ReCaret {
    protected lazy val peer = ReTextComponent.this.peer.caret
    
    val dot: ReSwingValue[Int] = peer.dot
    val mark: ReSwingValue[Int] = peer.mark
    val position: ReSwingValue[Int] = peer.position
  
    peer.reactions += {
      case e @ CaretUpdate(_) =>
        dot() = peer.dot; mark() = peer.mark; position() = peer.position
    }
  }
  
  object ReCaret {
    implicit def toCaret(caret: ReCaret) = caret.peer
  }
  
  object caret extends ReCaret
}

object ReTextComponent {
  implicit def toTextComponent(component: ReTextComponent): TextComponent = component.peer
}
