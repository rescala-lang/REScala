package reswing

import scala.events.behaviour.Signal
import scala.events.ImperativeEvent
import scala.swing.AbstractButton
import scala.swing.event.ButtonClicked


class ReAbstractButton extends ReComponent {
  override protected lazy val peer = new AbstractButton with AbstractButtonMixin
  
  protected trait AbstractButtonMixin extends AbstractButton with ComponentMixin {
    val reText = new ReactiveWrapper(text_=, text)
    override def text_=(s : String) {
      super.text = s
      reText.value = s
    }
  }
  
  def text = peer.reText.signal
  def text_=(s: Signal[String]) = peer.reText.signal = s
  
  val clicked = new ImperativeEvent[ButtonClicked]
  peer.reactions += {
    case e @ ButtonClicked(_) => clicked(e)
  }
}

object ReAbstractButton {
  implicit def toButton(input : ReAbstractButton) : AbstractButton = input.peer
}
