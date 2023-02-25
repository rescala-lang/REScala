package examples.catchup

import java.awt._

import examples.Mouse
import rescala.default._

import scala.swing.event._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object CatchUp extends SimpleSwingApplication {
  lazy val application = new CatchUp
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while (true) {
      Swing onEDTWait { application.tick.fire() }
      Thread sleep 50
    }
  }
}

class CatchUp {

  val Max_X = 800
  val Max_Y = 700
  val Range = 100

  val SizeCatch = 100
  val SizeUp    = 40
  val SizeY     = 32

  val tick = Evt[Unit]()
  val time = tick.iterate(0.0) { (acc: Double) => (acc + 0.1) % (math.Pi * 2) }

  // Mouse position
  val mouse  = new Mouse
  val mouseX = Signal { mouse.position.value.getX.toInt }
  val mouseY = Signal { mouse.position.value.getY.toInt }

  val xOffset = Signal { math.sin(time.value) * Range }
  val yOffset = Signal { math.cos(time.value) * Range }

  val x = Signal { mouseX.value + xOffset.value.toInt }
  val y = Signal { mouseY.value + yOffset.value.toInt }

  // Old mouse position, some time ago
  val mouseDelayed: Signal[Point] = Signal {
    mouse.position.changed.list(20).value.headOption match {
      case None    => mouse.position.value
      case Some(v) => v
    }
  }
  val delayedX = Signal { mouseDelayed.value.getX.toInt }
  val delayedY = Signal { mouseDelayed.value.getY.toInt }

  val catchBox = Signal { new Rectangle(x.value, y.value, SizeCatch, SizeY) }
  val upBox    = Signal { new Rectangle(delayedX.value, delayedY.value, SizeUp, SizeY) }

  val caught       = Signal { catchBox.value.intersects(upBox.value) }
  val hits         = caught.changed.filter(_ == true)
  val numberOfHits = hits.count()

  val scoreString = Signal { "You caught up " + numberOfHits.value + " times." }

  // GUI redrawing code
  val stateChanged = mouse.position.changed.||[Any](tick)
  stateChanged observe { (_) => frame.repaint() }

  // GUI
  val frame: MainFrame = new MainFrame {
    title = "Catch up!"
    resizable = false
    contents = new Panel() {
      listenTo(this.mouse.moves, this.mouse.clicks)

      /** forward mouse events to EScala wrapper class.
        * Should be replaced once reactive GUI lib is complete
        */
      reactions += {
        case e: MouseMoved    => { CatchUp.this.mouse.mouseMovedE.fire(e.point) }
        case e: MousePressed  => CatchUp.this.mouse.mousePressedE.fire(e.point)
        case e: MouseDragged  => { CatchUp.this.mouse.mouseDraggedE.fire(e.point) }
        case e: MouseReleased => CatchUp.this.mouse.mouseReleasedE.fire(e.point)
      }

      preferredSize = new Dimension(Max_X, Max_Y)
      val myFont = new Font("Tahoma", java.awt.Font.PLAIN, SizeY)
      override def paintComponent(g: Graphics2D): Unit = {
        transaction(catchBox, caught, upBox, scoreString) { t =>
          // val fontMetrics = g.getFontMetrics(myFont)
          g.setColor(java.awt.Color.DARK_GRAY)
          g.fill(t.now(catchBox))
          if (t.now(caught))
            g.setColor(java.awt.Color.RED)
          g.fill(t.now(upBox))
          g.setColor(java.awt.Color.WHITE)
          g.setFont(myFont)
          g.drawString("CATCH", t.now(catchBox).getX.toInt, t.now(catchBox).getY.toInt + SizeY - 5)
          g.drawString("UP", t.now(upBox).getX.toInt, t.now(upBox).getY.toInt + SizeY - 5)

          g.setColor(new Color(200, 100, 50))
          g.drawString(t.now(scoreString), Max_X / 2 - 100, 40)
        }
      }
    }
  }
}
