package reswing

import scala.events.behaviour.Signal
import scala.events.ImperativeEvent
import scala.swing.AbstractButton
import scala.swing.event.ButtonClicked
import java.awt.Dimension

class ReAbstractButton extends ReComponent {
  override protected lazy val peer = new AbstractButton with AbstractButtonMixin
  
  protected trait AbstractButtonMixin extends AbstractButton with ComponentMixin {
    override def text_=(s : String) {
      super.text = s
      ReAbstractButton.this.text(s)
    }
  }
  
  lazy val text = ImperativeSignal.noSignal[String]
  connectSignal(text, peer text, peer text_=)
  
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
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal) = {
    def text0 = text
    def minimumSize0 = minimumSize
    def maximumSize0 = maximumSize
    def preferredSize0 = preferredSize
    new ReAbstractButton {
      override lazy val minimumSize = minimumSize0
      override lazy val maximumSize = maximumSize0
      override lazy val preferredSize = preferredSize0
      override lazy val text = text0
    }: ReAbstractButton
  }
}
