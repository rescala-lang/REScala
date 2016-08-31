import javax.swing.SwingUtilities

import scala.swing.{Component, Swing}

package object reswing {
  type CompItem = Component
  object CompItem {
    def apply(elem: Component) = elem
  }

  type CompList = Seq[CompItem]
  object CompList {
    def apply(elems: CompItem*) = Seq(elems: _*)
  }

  def inSyncEDT(op: => Unit) =
    if (SwingUtilities.isEventDispatchThread)
      op
    else
      Swing onEDT op
}
