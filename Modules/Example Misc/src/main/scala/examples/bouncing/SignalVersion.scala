package examples.bouncing

import reactives.default.*

import java.awt.{Dimension, Graphics2D, Point}
import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object SignalVersion extends SimpleSwingApplication {
  lazy val application = new SignalVersion
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while true do {
      Swing onEDTWait { application.tick.transform(_ + 1) }
      Thread `sleep` 20
    }
  }
}

class SignalVersion {
  val Size         = 50
  val Max_X        = 600
  val Max_Y        = 600
  val initPosition = new Point(20, 10)
  val speed        = new Point(10, 8)

  val tick = Var(0)

  // Signals for x and y position
  // entirely functionally dependent on time (ticks)
  val x = Signal {
    val width = Max_X - Size
    val d     = speed.x * tick.value + initPosition.x
    if (d / width) % 2 == 0 then d % width else width - d % width
  }
  val y = Signal {
    val width = Max_Y - Size
    val d     = speed.y * tick.value + initPosition.y
    if (d / width) % 2 == 0 then d % width else width - d % width
  }

  tick.changed observe ((_: Int) => frame.repaint())

  // drawing code
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D): Unit =
        transaction(x, y) { t ?=>
          g.fillOval(t.now(x), t.now(y), Size, Size)
        }
    }
  }
}
