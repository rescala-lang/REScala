package reswing

import java.awt.Dimension

import scala.swing.Component
import scala.swing.Menu

class ReMenu extends ReMenuItem with ReSequentialContainer {
  override protected lazy val peer = new Menu(text.getValue) with AbstractButtonMixin
}

object ReMenu {
  implicit def toMenuItem(input : ReMenu) : Menu = input.peer
  
  def apply(
      text: ImperativeSignal[String] = ImperativeSignal.noSignal,
      contents: ImperativeSignal[Seq[Component]] = ImperativeSignal.noSignal,
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      enabled: ImperativeSignal[Boolean] = ImperativeSignal.noSignal) =
        Macros.defaultObjectCreation
}
