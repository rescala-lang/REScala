package reswing

import scala.language.implicitConversions
import scala.swing.Component
import scala.swing.SequentialContainer
import scala.swing.event.ComponentAdded
import scala.swing.event.ComponentRemoved
import scala.swing.RootPanel

trait ReRootPanel extends ReUIElement {
  protected def peer: RootPanel
  
  def contents: ReSwingValue[Component]
  
  contents using (
      { () => peer.contents.headOption getOrElse null },
      peer.contents_= _,
      (peer, classOf[ComponentAdded]), (peer, classOf[ComponentRemoved]))
}

object ReRootPanel {
  implicit def toRootPanel(component: ReRootPanel): RootPanel = component.peer
}
