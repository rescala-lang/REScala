package examples.pong

import react.events.ImperativeEvent
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{SignalM => Signal}
import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}
import java.awt.Point
import scala.swing.Swing
import scala.swing.event._

object PongStarter {
	def main(args: Array[String]){
		val app = new PongWindow
		app.main(args)
		while (true) {
			Thread sleep 20
			app.tick()
		}
	}
}

class PongWindow extends SimpleSwingApplication {
  val Size = 20
  val Max_X = 800
  val Max_Y = 400
  
  val tick = new ImperativeEvent[Unit]  
  tick += {_: Unit => frame.repaint()}
  
     /* EScala events */
    val mouseMovedE = new ImperativeEvent[Point]()
    val mousePressedE = new ImperativeEvent[Point]()
    val mouseDraggedE = new ImperativeEvent[Point]()
    val mouseReleasedE = new ImperativeEvent[Point]()
    val cKeyTypedE = new ImperativeEvent[Unit]()
    
    /* Bind the EScala events to the Swing events */
    reactions += {
      case e: MouseMoved  => { mouseMovedE(e.point) }
      case e: MousePressed  => mousePressedE(e.point)
      case e: MouseDragged  => { mouseDraggedE(e.point) }
      case e: MouseReleased => mouseReleasedE(e.point)
      case KeyTyped(_,'c',_,_) => cKeyTypedE()
    }
    
    /* Compose reactive values */
    val mouseChangePosition = mouseMovedE || mouseDraggedE
    val mousePressedOrReleased = mousePressedE || mouseReleasedE
    val mousePosMoving: Signal[Point] = mouseChangePosition.latest(new Point(0, 0))
    val pressed: Signal[Boolean] = mousePressedOrReleased.toggle(Signal{false}, Signal{true})
  
  val ball = new Ball(tick, null, null)
  
  // drawing code
  def top = frame  
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(Max_X, Max_Y)
      override def paintComponent(g: Graphics2D) {
	    g.fillOval(ball.x.getVal, ball.y.getVal, Size, Size)
      }
    }    
  }
}
