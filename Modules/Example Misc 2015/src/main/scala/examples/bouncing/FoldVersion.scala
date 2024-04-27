package examples.bouncing

import reactives.default.*

import java.awt.{Dimension, Graphics2D, Point}
import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object FoldVersion extends SimpleSwingApplication {
  lazy val application = new FoldVersion
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while (true) {
      Swing onEDTWait { application.tick.fire() }
      Thread `sleep` 20
    }
  }
}

class FoldVersion {
  val Size         = 50
  val Max_X        = 600
  val Max_Y        = 600
  val initPosition = new Point(20, 10)
  val speed        = new Point(10, 8)

  val tick = Evt[Unit]()
  // Implementing switch with fold
  val xx: Signal[Int] = tick.fold(initPosition.x) { (pos, _) =>
    pos + (if (xSwitch.now) speed.x else -speed.x)
  }

  val yy: Signal[Int] = tick.fold(initPosition.y) { (pos, _) =>
    pos + (if (ySwitch.now) speed.y else -speed.y)
  }

  val xSwitch = (xx.changed && (x => x < 0 || x + Size > Max_X)).fold(false) { (a, _) => !a }
  val ySwitch = (yy.changed && (y => y < 0 || y + Size > Max_Y)).fold(false) { (a, _) => !a }

  tick observe { (_: Unit) => frame.repaint() }

  // drawing code
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D): Unit =
        transaction(xx, yy) { t =>
          g.fillOval(t.now(xx), t.now(yy), Size, Size)
        }
    }
  }
}
