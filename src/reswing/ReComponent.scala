package reswing

import java.awt.Dimension
import java.awt.Graphics2D

import scala.events.ImperativeEvent
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import scala.swing.Component
import scala.swing.event._

abstract class ReComponent() {
  protected def peer: Component with ComponentMixin
  
  protected trait ComponentMixin extends Component {
    val reMinimumSize = new ReactiveWrapper(minimumSize_=, minimumSize)
    override def minimumSize_=(x: Dimension) {
      super.minimumSize = x
      reMinimumSize.value = x
    }
    
    val reMaximumSize = new ReactiveWrapper(maximumSize_=, maximumSize)
    override def maximumSize_=(x: Dimension) {
      super.maximumSize = x
      reMaximumSize.value = x
    }
    
    val rePreferredSize = new ReactiveWrapper(preferredSize_=, preferredSize)
    override def preferredSize_=(x: Dimension) {
      super.preferredSize = x
      rePreferredSize.value = x
    }
    
    val reLocation = Var(location)
    val reBounds = Var(bounds)
    val reSize = Var(size)
    val reHasFocus = Var(hasFocus)
    
    override def paintComponent(g: Graphics2D) = ReComponent.this.paintComponent(g)
    def __super__paintComponent(g: Graphics2D) = super.paintComponent(g)
    override def paintBorder(g: Graphics2D) = ReComponent.this.paintBorder(g)
    def __super__paintBorder(g: Graphics2D) = super.paintBorder(g)
    override def paintChildren(g: Graphics2D) = ReComponent.this.paintChildren(g)
    def __super__paintChildren(g: Graphics2D) = super.paintChildren(g)
    override def paint(g: Graphics2D) = {
      reLocation() = location
      reBounds() = bounds
      reSize() = size
      ReComponent.this.paint(g)
    }
    def __super__paint(g: Graphics2D) = super.paint(g)
  }
 
  def minimumSize = peer.reMinimumSize.signal
  def minimumSize_=(x: Signal[Dimension]) = peer.reMinimumSize.signal = x
  
  def maximumSize = peer.reMaximumSize.signal
  def maximumSize_=(x: Signal[Dimension]) = peer.reMaximumSize.signal = x
  
  def preferredSize = peer.rePreferredSize.signal
  def preferredSize_=(x: Signal[Dimension]) = peer.rePreferredSize.signal = x
  
  val location = Signal{ peer.reLocation() }
  val bounds = Signal{ peer.reBounds() }
  val size = Signal{ peer.reSize() }
  val hasFocus = Signal{ peer.reHasFocus() }
  
  protected def paintComponent(g: Graphics2D) = peer.__super__paintComponent(g)
  protected def paintBorder(g: Graphics2D) = peer.__super__paintBorder(g)
  protected def paintChildren(g: Graphics2D) = peer.__super__paintChildren(g)
  def paint(g: Graphics2D) = peer.__super__paint(g)
  
  val backgroundChanged = new ImperativeEvent[BackgroundChanged]
  val fontChanged = new ImperativeEvent[FontChanged]
  val foregroundChanged = new ImperativeEvent[ForegroundChanged]
  
  peer.listenTo(peer, peer.keys, peer.mouse.clicks, peer.mouse.moves, peer.mouse.wheel);
  
  peer.reactions += {
    case e @ BackgroundChanged(_) => backgroundChanged(e)
    case e @ (FocusGained(_, _,_) | FocusLost(_, _, _)) => peer.reHasFocus() = peer.hasFocus
    case e @ FontChanged(_) => fontChanged(e)
    case e @ ForegroundChanged(_) => foregroundChanged(e)
    case e @ UIElementMoved(_) => peer.reLocation() = peer.location
    case e @ UIElementResized(_) => peer.reSize() = peer.size; peer.reBounds() = peer.bounds
  }
  
  object mouse {
    object clicks {
      val clicked = new ImperativeEvent[MouseClicked]
      val pressed = new ImperativeEvent[MousePressed]
      val released = new ImperativeEvent[MouseReleased]
      
      peer.mouse.clicks.reactions += {
        case e @ MouseClicked(_, _, _, _, _) => clicked(e)
        case e @ MousePressed(_, _, _, _, _) => pressed(e)
        case e @ MouseReleased(_, _, _, _, _) => released(e)
      }
    }
    
    object moves {
      val dragged = new ImperativeEvent[MouseDragged]
      val entered = new ImperativeEvent[MouseEntered]
      val exited = new ImperativeEvent[MouseExited]
      val moved = new ImperativeEvent[MouseMoved]
      
      peer.mouse.moves.reactions += {
        case e @ MouseDragged(_, _, _) => dragged(e)
        case e @ MouseEntered(_, _, _) => entered(e)
        case e @ MouseExited(_, _, _) => exited(e)
        case e @ MouseMoved(_, _, _) => moved(e)
      }
    }
    
    object wheel {
      val moved = new ImperativeEvent[MouseWheelMoved]
      
      peer.mouse.wheel.reactions += {
        case e @ MouseWheelMoved(_, _, _, _) => moved(e)
      }
    }
  }
  
  object keys {
    val pressed = new ImperativeEvent[KeyPressed]
    val released = new ImperativeEvent[KeyReleased]
    val typed = new ImperativeEvent[KeyTyped]
    
    peer.keys.reactions += {
      case e @ KeyPressed(_, _, _, _) => pressed(e)
      case e @ KeyReleased(_, _, _, _) => released(e)
      case e @ KeyTyped(_, _, _, _) => typed(e)
    }
  }
}

object ReComponent {
  implicit def toComponent(input : ReComponent) : Component = input.peer
}
