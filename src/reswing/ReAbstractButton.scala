package reswing

import scala.events.behaviour.Signal
import scala.events.ImperativeEvent
import scala.swing.AbstractButton
import scala.swing.event.ButtonClicked
import java.awt.Dimension

class ReAbstractButton(
    val text: ImperativeSignal[String] = ImperativeSignal.noSignal,
    minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
    maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
    preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal)
  extends ReComponent(
    minimumSize = minimumSize,
    maximumSize = maximumSize,
    preferredSize = preferredSize) {
  
  override protected lazy val peer = new AbstractButton with AbstractButtonMixin
  
  protected trait AbstractButtonMixin extends AbstractButton with ComponentMixin {
    override def text_=(s : String) {
      super.text = s
      ReAbstractButton.this.text(s)
    }
  }
  
  connectSignal(text, peer text, peer text_=)
  
  val clicked = new ImperativeEvent[ButtonClicked]
  peer.reactions += {
    case e @ ButtonClicked(_) => clicked(e)
  }
}

object ReAbstractButton {
  implicit def toButton(input : ReAbstractButton) : AbstractButton = input.peer
}
