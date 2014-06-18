package reswing

import scala.language.implicitConversions
import scala.swing.Component
import scala.swing.SequentialContainer
import scala.swing.event.ComponentAdded
import scala.swing.event.ComponentRemoved

trait ReSequentialContainer extends ReUIElement {
  protected def peer: SequentialContainer
  
  def contents: ReSwingValue[CompList]

  contents using (
      peer.contents _,
      { components =>
        peer.contents.clear
        peer.contents ++= components
        peer.repaint
        peer.peer.validate
      },
      classOf[ComponentAdded], classOf[ComponentRemoved])
  
  protected implicit def addContent(contents: ReSwingValue[CompList]) =
    new AddContent(contents)
  
  protected class AddContent(contents: ReSwingValue[CompList]) {
    def +=(comp: Component) { contents() = contents.getValue :+ comp }
  }
}

object ReSequentialContainer {
  implicit def toSequentialContainer(component: ReSequentialContainer): SequentialContainer =
    component.peer
}
