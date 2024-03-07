package reswing

import scala.swing.{Component, SequentialContainer}
import scala.swing.event.{ComponentAdded, ComponentRemoved}

trait ReSequentialContainer extends ReUIElement {
  protected def peer: SequentialContainer

  private def peerContents: CompList = peer.contents.toSeq: CompList

  private def peerContents_=(components: CompList): Unit = {
    peer.contents.clear()
    peer.contents ++= components
    peer.repaint()
    peer.peer.validate
  }

  def contents: ReSwingValue[CompList]

  contents.using({ () => peerContents }, peerContents_=, classOf[ComponentAdded], classOf[ComponentRemoved])

  protected implicit class AddContent(contents: ReSwingValue[CompList]) {
    def +=(component: Component): Unit = peerContents :+= component
  }
}

object ReSequentialContainer {
  implicit def toSequentialContainer(component: ReSequentialContainer): SequentialContainer =
    component.peer
}
