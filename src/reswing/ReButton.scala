package reswing

import java.awt.Dimension

import scala.swing.Action
import scala.swing.Button

import reswing.ImperativeSignal.toSignal

class ReButton(action: Action = null) extends ReAbstractButton {
  override protected lazy val peer = new Button(text getValue) with AbstractButtonMixin
  
  if (action != null)
    peer action = action
}

object ReButton {
  implicit def toButton(input : ReButton) : Button = input.peer
  
  def apply(
      text: ImperativeSignal[String] = ImperativeSignal.noSignal,
      action: Action = null,
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal) = {
    def text0 = text
    def minimumSize0 = minimumSize
    def maximumSize0 = maximumSize
    def preferredSize0 = preferredSize
    new ReButton(action) {
      override lazy val minimumSize = minimumSize0
      override lazy val maximumSize = maximumSize0
      override lazy val preferredSize = preferredSize0
      override lazy val text = text0
    }: ReButton
  }
}
