package reswing

import scala.swing.event.{ComponentAdded, ComponentRemoved}
import scala.swing.{Component, RootPanel}

trait ReRootPanel extends ReUIElement {
  protected def peer: RootPanel

  def contents: ReSwingValue[Component]

  contents.using(
    { () => peer.contents.headOption getOrElse null },
    peer.contents_=,
    classOf[ComponentAdded],
    classOf[ComponentRemoved]
  )
}

object ReRootPanel {
  implicit def toRootPanel(component: ReRootPanel): RootPanel = component.peer
}
