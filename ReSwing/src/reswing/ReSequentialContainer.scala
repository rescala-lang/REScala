package reswing

import scala.language.implicitConversions
import scala.swing.Component
import scala.swing.SequentialContainer
import scala.swing.event.ComponentAdded
import scala.swing.event.ComponentRemoved

trait ReSequentialContainer extends ReComponent {
  override protected def peer: Component with SequentialContainer with ComponentMixin
  
  def contents: ReSwingValue[CompList]

  contents using (
      peer.contents _,
      { components =>
        peer.contents.clear
        peer.contents ++= components
        peer.repaint
        peer.peer.validate
      },
      (peer, classOf[ComponentAdded]), (peer, classOf[ComponentRemoved]))
  
  protected implicit def addContent(contents: ReSwingValue[CompList]) =
    new AddContent(contents)
  
  protected class AddContent(contents: ReSwingValue[CompList]) {
    def +=(comp: Component) { contents() = contents.getValue :+ comp }
  }
}

object ReSequentialContainer {
  implicit def toButton(component: ReSequentialContainer): SequentialContainer = component.peer
}
