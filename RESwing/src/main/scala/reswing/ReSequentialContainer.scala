package reswing

import scala.language.implicitConversions
import scala.swing.Component
import scala.swing.SequentialContainer
import scala.swing.event.ComponentAdded
import scala.swing.event.ComponentRemoved

trait ReSequentialContainer extends ReUIElement {
  protected def peer: SequentialContainer

  private def peerContents = peer.contents: CompList

  private def peerContents_=(components: CompList): Unit = {
    peer.contents.clear
    peer.contents ++= components
    peer.repaint
    peer.peer.validate
  }

  def contents: ReSwingValue[CompList]

  contents using (peerContents _, peerContents_= _,
                  classOf[ComponentAdded], classOf[ComponentRemoved])

  protected implicit class AddContent(contents: ReSwingValue[CompList]) {
    def +=(component: Component) = peerContents :+= component
  }
}

object ReSequentialContainer {
  implicit def toSequentialContainer(component: ReSequentialContainer): SequentialContainer =
    component.peer
}
