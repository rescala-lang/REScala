package reswing

import scala.swing.Button
import scala.swing.Action

class ReButton(text0: String) extends ReAbstractButton {
  override protected lazy val peer = new Button(text0) with AbstractButtonMixin

  def this() = this("")
  def this(a: Action) = {
    this("")
    peer.action = a
  }
}

object ReButton {
  implicit def toButton(input : ReButton) : Button = input.peer
}
