package examples.bouncing

import java.awt.{Dimension, Graphics2D, Point}

import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object ImperativeVersion extends SimpleSwingApplication {
  lazy val application = new ImperativeVersion
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while (true) {
      Swing onEDTWait { application.tick() }
      Thread `sleep` 20
    }
  }
}

class ImperativeVersion {
  val Size  = 50
  val Max_X = 600
  val Max_Y = 600

  val position = new Point(20, 20)
  val speed    = new Point(10, 8)

  def tick(): Unit = {
    position.translate(speed.x, speed.y)
    if (position.x < 0 || position.x + Size > Max_X) speed.x = -speed.x
    if (position.y < 0 || position.y + Size > Max_Y) speed.y = -speed.y
    frame.repaint()
  }

  // drawing code
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D): Unit = {
        g.fillOval(position.x, position.y, Size, Size)
      }
    }
  }
}
