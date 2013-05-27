package reswing

import java.awt.Dimension

import scala.swing.Action
import scala.swing.MenuItem

class ReMenuItem(action: Action = null) extends ReAbstractButton {
  override protected lazy val peer = new MenuItem(text.getValue) with AbstractButtonMixin
  
  if (action != null)
    peer.action = action
}

object ReMenuItem {
  implicit def toMenuItem(input : ReMenuItem) : MenuItem = input.peer
  
  def apply(
      text: ImperativeSignal[String] = ImperativeSignal.noSignal,
      action: Action = null,
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      enabled: ImperativeSignal[Boolean] = ImperativeSignal.noSignal) =
        Macros.defaultObjectCreation
}
