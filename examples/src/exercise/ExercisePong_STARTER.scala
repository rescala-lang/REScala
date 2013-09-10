package exercise

import react.events.ImperativeEvent
import react.SignalSynt
import react.Var
import react.Signal
import macro.SignalMacro.{ SignalM => Signal }
import swing.{ Panel, MainFrame, SimpleSwingApplication }
import java.awt.{ Color, Graphics2D, Dimension }
import java.awt.Point
import scala.swing.Swing
import scala.swing.event._
import java.awt.Font


/**
 * Exercise note: Do not edit any code in this file.
 * Only the file Pong.scala needs to be changed.
 */

object PongStarter {
  def main(args: Array[String]) {
    val app = new PongWindow
    app.main(args)
    while (true) {
      Thread sleep 30
      app.tick()
    }
  }
}

class PongWindow extends SimpleSwingApplication {

  val tick = new ImperativeEvent[Unit]
  tick += { _: Unit => frame.repaint() }
  
  val mouse = new Mouse
  val ball = new Pong(tick, mouse)

  // drawing code
  def top = frame
  val frame: MainFrame = new MainFrame {
    title = "Pong"
    resizable = false
    contents = new Panel() {
      listenTo(mouse.moves, mouse.clicks)

      /** forward mouse events to EScala wrapper class. Should be replaced once reactive GUI lib is complete */
      reactions += {
        case e: MouseMoved => { PongWindow.this.mouse.mouseMovedE(e.point) }
        case e: MousePressed => PongWindow.this.mouse.mousePressedE(e.point)
        case e: MouseDragged => { PongWindow.this.mouse.mouseDraggedE(e.point) }
        case e: MouseReleased => PongWindow.this.mouse.mouseReleasedE(e.point)
      }

      preferredSize = new Dimension(Pong.Max_X, Pong.Max_Y)
      val scoreFont = new Font("Tahoma", java.awt.Font.PLAIN, 32)
      override def paintComponent(g: Graphics2D) {
        g.setColor(java.awt.Color.DARK_GRAY)
        g.fillOval(ball.x.getVal, ball.y.getVal, Ball.Size, Ball.Size)
        
        g.fillRect(ball.leftRacket.area.getVal.x, 
        		   ball.leftRacket.area.getVal.y,
        		   ball.leftRacket.area.getVal.width,
        		   ball.leftRacket.area.getVal.height
            )
        g.fillRect(ball.rightRacket.area.getVal.x, 
        		   ball.rightRacket.area.getVal.y,
        		   ball.rightRacket.area.getVal.width,
        		   ball.rightRacket.area.getVal.height
            )        
            
        
        g.setColor(new Color(200, 100, 50))
        g.setFont(scoreFont)
        g.drawString(ball.score.getVal, Pong.Max_X / 2 - 50, 40)
      }
    }
  }
}

class Mouse {
  
	 /* EScala events */
	val mouseMovedE = new ImperativeEvent[Point]()
	val mousePressedE = new ImperativeEvent[Point]()
	val mouseDraggedE = new ImperativeEvent[Point]()
	val mouseReleasedE = new ImperativeEvent[Point]()
	
	
	/* Compose reactive values */
	val mouseChangePosition = mouseMovedE || mouseDraggedE
	val mousePressedOrReleased = mousePressedE || mouseReleasedE
	val position: Signal[Point] = mouseChangePosition.latest(new Point(0, 0))
	val pressed: Signal[Boolean] = mousePressedOrReleased.toggle(Signal{false}, Signal{true}) // TODO: solve this more robust
	
}
