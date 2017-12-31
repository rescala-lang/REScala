package examples.pong.ui

import java.awt.{Color, Dimension, Font, Graphics2D}

import examples.Mouse
import examples.pong._
import rescala._

import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}


/**
  * Exercise note: Do not edit any code in this file.
  * Only the file Pong.scala needs to be changed.
  */

object PongStarter extends SimpleSwingApplication {
  /* Uncomment to enable logging: */
  //react.ReactiveEngine.log.enableAllLogging

  lazy val application = new PongWindow
  def top = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while (true) {
      Swing onEDTWait {application.tick.fire()}
      Thread sleep 20
    }
  }
}

class PongWindow {

  val tick = Evt[Unit]
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
      override def paintComponent(g: Graphics2D): Unit = {
        g.setColor(java.awt.Color.DARK_GRAY)
        g.fillOval(ball.x.now, ball.y.now, Ball.Size, Ball.Size)

        g.fillRect(ball.leftRacket.area.now.x,
          ball.leftRacket.area.now.y,
          ball.leftRacket.area.now.width,
          ball.leftRacket.area.now.height
        )
        g.fillRect(ball.rightRacket.area.now.x,
          ball.rightRacket.area.now.y,
          ball.rightRacket.area.now.width,
          ball.rightRacket.area.now.height
        )


        g.setColor(new Color(200, 100, 50))
        g.setFont(scoreFont)
        g.drawString(ball.score.now, Pong.Max_X / 2 - 50, 40)
      }
    }
  }
}
