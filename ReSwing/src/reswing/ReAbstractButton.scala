package reswing

import java.awt.Dimension

import scala.events.ImperativeEvent
import scala.swing.AbstractButton
import scala.swing.event.ButtonClicked

class ReAbstractButton extends ReComponent {
  override protected lazy val peer = new AbstractButton with AbstractButtonMixin
  
  protected trait AbstractButtonMixin extends AbstractButton with ComponentMixin {
    override def text_=(s : String) = Macros.defaultSetterOverride
  }
  
  lazy val text = ImperativeSignal.noSignal[String]
  connectSignal(text, peer.text, peer.text_=)
  
  val clicked = new ImperativeEvent[ButtonClicked]
  peer.reactions += {
    case e @ ButtonClicked(_) => clicked(e)
  }
}

object ReAbstractButton {
  implicit def toButton(input : ReAbstractButton) : AbstractButton = input.peer
  
  def apply(
      text: ImperativeSignal[String] = ImperativeSignal.noSignal,
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal) =
        Macros.defaultObjectCreation
}
