package reswing

import java.awt.Dimension

import scala.swing.Label

class ReLabel extends ReComponent {
  override protected lazy val peer = new Label with LabelMixin

  protected trait LabelMixin extends Label with ComponentMixin {
    override def text_=(s : String) = Macros.defaultSetterOverride
  }
  
  lazy val text = ImperativeSignal.noSignal[String]
  connectSignal(text, peer.text, peer.text_=)
}

object ReLabel {
  implicit def toLabel(input : ReLabel) : Label = input.peer
  
  def apply(
      text: ImperativeSignal[String] = ImperativeSignal.noSignal,
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal) =
        Macros.defaultObjectCreation
}
