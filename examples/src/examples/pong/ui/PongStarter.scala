package examples.pong.ui

import examples.pong._
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

object PongStarter extends SimpleSwingApplication {
  /* Uncomment to enable logging: */
  //react.ReactiveEngine.log.enableAllLogging
  
  lazy val application = new PongWindow
  def top = application.frame
  
  override def main(args: Array[String]) {
    super.main(args)
    while (true) {
	  Swing onEDTWait { application.tick() }
      Thread sleep 20
    }
  }
}

class PongWindow {

  val tick = new ImperativeEvent[Unit]
  tick += { _: Unit => frame.repaint() }
  
  val mouse = new Mouse
  val ball = new Pong(tick, mouse)

  // drawing code
  val frame: MainFrame = new MainFrame {
    title = "Pong"
    resizable = false
    contents = new Panel() {
      
       /** forward mouse events to EScala wrapper class. Should be replaced once reactive GUI lib is complete */
      listenTo(mouse.moves, mouse.clicks)
      reactions += PongWindow.this.mouse.react

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
