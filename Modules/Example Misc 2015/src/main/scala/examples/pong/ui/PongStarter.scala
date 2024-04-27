package examples.pong.ui

import examples.Mouse
import examples.pong.*
import reactives.default.*

import java.awt.{Color, Dimension, Font, Graphics2D}
import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

/** Exercise note: Do not edit any code in this file.
  * Only the file Pong.scala needs to be changed.
  */

object PongStarter extends SimpleSwingApplication {
  /* Uncomment to enable logging: */
  // react.ReactiveEngine.log.enableAllLogging

  lazy val application = new PongWindow
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while (true) {
      Swing onEDTWait { application.tick.fire() }
      Thread `sleep` 20
    }
  }
}

class PongWindow {

  val tick = Evt[Unit]()
  tick observe { (_: Unit) => frame.repaint() }

  val mouse = new Mouse
  val ball  = new Pong(tick, mouse)

  // drawing code
  val frame: MainFrame = new MainFrame {
    title = "Pong"
    resizable = false
    contents = new Panel() {

      /** forward mouse events to EScala wrapper class. Should be replaced once reactive GUI lib is complete */
      listenTo(this.mouse.moves, this.mouse.clicks)
      reactions += PongWindow.this.mouse.react

      preferredSize = new Dimension(Pong.Max_X, Pong.Max_Y)
      val scoreFont = new Font("Tahoma", java.awt.Font.PLAIN, 32)
      override def paintComponent(g: Graphics2D): Unit =
        transaction(ball.x, ball.y, ball.leftRacket.area, ball.rightRacket.area) { t =>
          g.setColor(java.awt.Color.DARK_GRAY)
          g.fillOval(t.now(ball.x), t.now(ball.y), Ball.Size, Ball.Size)

          g.fillRect(
            t.now(ball.leftRacket.area).x,
            t.now(ball.leftRacket.area).y,
            t.now(ball.leftRacket.area).width,
            t.now(ball.leftRacket.area).height
          )
          g.fillRect(
            t.now(ball.rightRacket.area).x,
            t.now(ball.rightRacket.area).y,
            t.now(ball.rightRacket.area).width,
            t.now(ball.rightRacket.area).height
          )

          g.setColor(new Color(200, 100, 50))
          g.setFont(scoreFont)
          g.drawString(t.now(ball.score), Pong.Max_X / 2 - 50, 40)
        }
    }
  }
}
