package examples.pong.ui

import examples.pong._
import rescala.events.ImperativeEvent
import rescala.SignalSynt
import rescala.Var
import rescala.Signal
import makro.SignalMacro.{ SignalM => Signal }
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
        g.fillOval(ball.x.get, ball.y.get, Ball.Size, Ball.Size)
        
        g.fillRect(ball.leftRacket.area.get.x, 
        		   ball.leftRacket.area.get.y,
        		   ball.leftRacket.area.get.width,
        		   ball.leftRacket.area.get.height
            )
        g.fillRect(ball.rightRacket.area.get.x, 
        		   ball.rightRacket.area.get.y,
        		   ball.rightRacket.area.get.width,
        		   ball.rightRacket.area.get.height
            )        
            
        
        g.setColor(new Color(200, 100, 50))
        g.setFont(scoreFont)
        g.drawString(ball.score.get, Pong.Max_X / 2 - 50, 40)
      }
    }
  }
}
