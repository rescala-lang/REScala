package reswing

import scala.swing.Component
import scala.swing.event.ComponentAdded
import scala.swing.event.ComponentRemoved
import scala.swing.SequentialContainer

trait ReSequentialContainer extends ReComponent {
  override protected def peer: Component with ComponentMixin with SequentialContainer.Wrapper
  
  lazy val contents = ImperativeSignal.noSignal[Seq[Component]]
  
  connectSignal(contents, peer.contents, { components: Seq[Component] =>
    peer.contents.clear
    peer.contents ++= components
  })
  
  peer.reactions += {
    case e @ ComponentAdded(_, _) => contents(peer.contents)
    case e @ ComponentRemoved(_, _) => contents(peer.contents)
  }
}
