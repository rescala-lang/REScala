package reswing

import java.awt.Dimension

import scala.swing.Component
import scala.swing.MenuBar

class ReMenuBar extends ReComponent with ReSequentialContainer {
  override protected lazy val peer = new MenuBar with ComponentMixin
}

object ReMenuBar {
  implicit def toLabel(input : ReMenuBar) : MenuBar = input.peer
  
  def apply(
      contents: ImperativeSignal[Seq[Component]] = ImperativeSignal.noSignal,
      minimumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      maximumSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      preferredSize: ImperativeSignal[Dimension] = ImperativeSignal.noSignal,
      enabled: ImperativeSignal[Boolean] = ImperativeSignal.noSignal) =
        Macros.defaultObjectCreation
}
