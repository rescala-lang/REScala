package ex2013reswing

import scala.swing.event.{ComponentAdded, ComponentRemoved}
import scala.swing.{Component, LayoutContainer}

trait ReLayoutContainer[Constraints] extends ReUIElement {
  protected def peer: LayoutContainer

  private val peerLayout = peer.layout.asInstanceOf[
    scala.collection.mutable.Map[Component, Constraints]
  ]

  private def peerContents = peerLayout.toMap

  private def peerContents_=(components: Map[Component, Constraints]): Unit = {
    peerLayout.clear()
    peerLayout ++= components
    peer.repaint()
    peer.peer.validate()
  }

  def contents: ReSwingValue[Map[Component, Constraints]]

  contents.using({ () => peerContents }, peerContents_=, classOf[ComponentAdded], classOf[ComponentRemoved])

  object layout {
    def update(component: Component, constraints: Constraints) =
      peerContents += (component -> constraints)
  }
}

object ReLayoutContainer {
  implicit def toLayoutContainer(component: ReLayoutContainer[?]): LayoutContainer =
    component.peer
}
