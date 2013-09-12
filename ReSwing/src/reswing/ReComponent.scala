package reswing

import scala.language.implicitConversions
import scala.swing.Color
import scala.swing.Component
import scala.swing.Dimension
import scala.swing.Font
import scala.swing.Graphics2D
import scala.swing.event._

abstract class ReComponent(
    val background: ReSwingValue[Color] = ReSwingValue.noValue,
    val foreground: ReSwingValue[Color] = ReSwingValue.noValue,
    val font: ReSwingValue[Font] = ReSwingValue.noValue,
    val enabled: ReSwingValue[Boolean] = ReSwingValue.noValue,
    minimumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    maximumSize: ReSwingValue[Dimension] = ReSwingValue.noValue,
    preferredSize: ReSwingValue[Dimension] = ReSwingValue.noValue)
  extends
    ReUIElement(minimumSize, maximumSize, preferredSize) {
  
  protected def peer: Component with ComponentMixin
  
  background using (peer.background _, peer.background_= _, "background")
  foreground using (peer.foreground _, peer.foreground_= _, "foreground")
  font using (peer.font _, peer.font_= _, "font")
  enabled using (peer.enabled _, peer.enabled_= _, "enabled")
  
  val hasFocus: ReSwingValue[Boolean] = peer.hasFocus
  
  peer.reactions += {
    case e @ (FocusGained(_, _,_) | FocusLost(_, _, _)) => hasFocus() = peer.hasFocus
  }
  
  peer.listenTo(peer.keys, peer.mouse.clicks, peer.mouse.moves, peer.mouse.wheel);
  
  object mouse {
    object clicks {
      val clicked = new ReSwingEvent[MouseClicked]
      val pressed = new ReSwingEvent[MousePressed]
      val released = new ReSwingEvent[MouseReleased]
      
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
      
      peer.mouse.moves.reactions += {
        case e @ MouseDragged(_, _, _) => dragged(e)
        case e @ MouseEntered(_, _, _) => entered(e)
        case e @ MouseExited(_, _, _) => exited(e)
        case e @ MouseMoved(_, _, _) => moved(e)
      }
    }
    
    object wheel {
      val moved = new ReSwingEvent[MouseWheelMoved]
      
      peer.mouse.wheel.reactions += {
        case e @ MouseWheelMoved(_, _, _, _) => moved(e)
      }
    }
  }
  
  object keys {
    val pressed = new ReSwingEvent[KeyPressed]
    val released = new ReSwingEvent[KeyReleased]
    val typed = new ReSwingEvent[KeyTyped]
    
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
