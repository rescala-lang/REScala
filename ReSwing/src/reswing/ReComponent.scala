package reswing

import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Point
import java.awt.Rectangle

import scala.events.ImperativeEvent
import scala.swing.Component
import scala.swing.event._

import reswing.ImperativeSignal.toSignal

abstract class ReComponent {
  protected def connectSignal[T](signal: ImperativeSignal[T], init: T, setter: T => Unit) {
    if (signal.inputSignal != null) {
      setter(signal.getValue)
      signal.inputSignal.changed += setter
    }
    else
      signal(init)
  }
  
  protected def peer: Component with ComponentMixin
  
  protected trait ComponentMixin extends Component {
    override def minimumSize_=(x: Dimension) = Macros.defaultSetterOverride
    override def maximumSize_=(x: Dimension) = Macros.defaultSetterOverride
    override def preferredSize_=(x: Dimension) = Macros.defaultSetterOverride
    
    override def paintComponent(g: Graphics2D) = ReComponent.this.paintComponent(g)
    def __super__paintComponent(g: Graphics2D) = super.paintComponent(g)
    override def paintBorder(g: Graphics2D) = ReComponent.this.paintBorder(g)
    def __super__paintBorder(g: Graphics2D) = super.paintBorder(g)
    override def paintChildren(g: Graphics2D) = ReComponent.this.paintChildren(g)
    def __super__paintChildren(g: Graphics2D) = super.paintChildren(g)
    override def paint(g: Graphics2D) = {
      ReComponent.this.location(location)
      ReComponent.this.bounds(bounds)
      ReComponent.this.size(size)
      ReComponent.this.paint(g)
    }
    def __super__paint(g: Graphics2D) = super.paint(g)
  }
  
  protected def paintComponent(g: Graphics2D) = peer.__super__paintComponent(g)
  protected def paintBorder(g: Graphics2D) = peer.__super__paintBorder(g)
  protected def paintChildren(g: Graphics2D) = peer.__super__paintChildren(g)
  def paint(g: Graphics2D) = peer.__super__paint(g)
  
  val backgroundChanged = new ImperativeEvent[BackgroundChanged]
  val fontChanged = new ImperativeEvent[FontChanged]
  val foregroundChanged = new ImperativeEvent[ForegroundChanged]
  
  val location = ImperativeSignal.noSignal(peer.location)
  val bounds = ImperativeSignal.noSignal(peer.bounds)
  val size = ImperativeSignal.noSignal(peer.size)
  val hasFocus = ImperativeSignal.noSignal(peer.hasFocus)
  
  lazy val minimumSize = ImperativeSignal.noSignal[Dimension]
  lazy val maximumSize = ImperativeSignal.noSignal[Dimension]
  lazy val preferredSize = ImperativeSignal.noSignal[Dimension]
  
  connectSignal(minimumSize, peer.minimumSize, peer.minimumSize_=)
  connectSignal(maximumSize, peer.maximumSize, peer.maximumSize_=)
  connectSignal(preferredSize, peer.preferredSize, peer.preferredSize_=)
  
  peer.listenTo(peer, peer.keys, peer.mouse.clicks, peer.mouse.moves, peer.mouse.wheel);
  
  peer.reactions += {
    case e @ BackgroundChanged(_) => backgroundChanged(e)
    case e @ (FocusGained(_, _,_) | FocusLost(_, _, _)) => hasFocus(peer.hasFocus)
    case e @ FontChanged(_) => fontChanged(e)
    case e @ ForegroundChanged(_) => foregroundChanged(e)
    case e @ UIElementMoved(_) => location(peer.location)
    case e @ UIElementResized(_) => size(peer.size); bounds(peer.bounds)
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
