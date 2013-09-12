package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Component
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.Graphics2D
import scala.swing.event._

abstract class ReComponent(
    val background: ReSwingValue[Color] = (),
    val foreground: ReSwingValue[Color] = (),
    val font: ReSwingValue[Font] = (),
    val enabled: ReSwingValue[Boolean] = (),
    minimumSize: ReSwingValue[Dimension] = (),
    maximumSize: ReSwingValue[Dimension] = (),
    preferredSize: ReSwingValue[Dimension] = ())
  extends
    ReUIElement(minimumSize, maximumSize, preferredSize) {
  
  protected def peer: Component with ComponentMixin
  
  val hasFocus: ReSwingValue[Boolean] = ()
  
  hasFocus using (peer.hasFocus _, (peer, classOf[FocusGained]),
                                   (peer, classOf[FocusLost]))
  
  background using (peer.background _, peer.background_= _, "background")
  foreground using (peer.foreground _, peer.foreground_= _, "foreground")
  font using (peer.font _, peer.font_= _, "font")
  enabled using (peer.enabled _, peer.enabled_= _, "enabled")
  
  object mouse {
    object clicks {
      val clicked = new ReSwingEvent[MouseClicked]
      val pressed = new ReSwingEvent[MousePressed]
      val released = new ReSwingEvent[MouseReleased]
      
      peer.mouse.clicks.listenTo(peer.mouse.clicks)
      peer.mouse.clicks.reactions += {
        case e @ MouseClicked(_, _, _, _, _) => clicked(e)
        case e @ MousePressed(_, _, _, _, _) => pressed(e)
        case e @ MouseReleased(_, _, _, _, _) => released(e)
      }
    }
    
    object moves {
      val dragged = new ReSwingEvent[MouseDragged]
      val entered = new ReSwingEvent[MouseEntered]
      val exited = new ReSwingEvent[MouseExited]
      val moved = new ReSwingEvent[MouseMoved]
      
      peer.mouse.moves.listenTo(peer.mouse.moves)
      peer.mouse.moves.reactions += {
        case e @ MouseDragged(_, _, _) => dragged(e)
        case e @ MouseEntered(_, _, _) => entered(e)
        case e @ MouseExited(_, _, _) => exited(e)
        case e @ MouseMoved(_, _, _) => moved(e)
      }
    }
    
    object wheel {
      val moved = new ReSwingEvent[MouseWheelMoved]
      
      peer.mouse.wheel.listenTo(peer.mouse.wheel)
      peer.mouse.wheel.reactions += {
        case e @ MouseWheelMoved(_, _, _, _) => moved(e)
      }
    }
  }
  
  object keys {
    val pressed = new ReSwingEvent[KeyPressed]
    val released = new ReSwingEvent[KeyReleased]
    val typed = new ReSwingEvent[KeyTyped]
    
    peer.keys.listenTo(peer.keys)
    peer.keys.reactions += {
      case e @ KeyPressed(_, _, _, _) => pressed(e)
      case e @ KeyReleased(_, _, _, _) => released(e)
      case e @ KeyTyped(_, _, _, _) => typed(e)
    }
  }
  
  protected trait ComponentMixin extends Component {
    override def paintComponent(g: Graphics2D) = ReComponent.this.paintComponent(g)
    def __super__paintComponent(g: Graphics2D) = super.paintComponent(g)
    override def paintBorder(g: Graphics2D) = ReComponent.this.paintBorder(g)
    def __super__paintBorder(g: Graphics2D) = super.paintBorder(g)
    override def paintChildren(g: Graphics2D) = ReComponent.this.paintChildren(g)
    def __super__paintChildren(g: Graphics2D) = super.paintChildren(g)
    override def paint(g: Graphics2D) = {
      ReComponent.this.location() = location
      ReComponent.this.bounds() = bounds
      ReComponent.this.size() = size
      ReComponent.this.paint(g)
    }
    def __super__paint(g: Graphics2D) = super.paint(g)
  }

  protected def paintComponent(g: Graphics2D) = peer.__super__paintComponent(g)
  protected def paintBorder(g: Graphics2D) = peer.__super__paintBorder(g)
  protected def paintChildren(g: Graphics2D) = peer.__super__paintChildren(g)
  def paint(g: Graphics2D) = peer.__super__paint(g)
}

object ReComponent {
  implicit def toComponent(component: ReComponent): Component = component.peer
}
