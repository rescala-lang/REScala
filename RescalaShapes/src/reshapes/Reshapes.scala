package reshapes

import scala.swing._
import scala.events.scalareact
import scala.events.behaviour.Var
import util.Random
import scala.swing.event._
import reshapes.figures.Line

object Reshapes extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "ReShapes";
    preferredSize = new Dimension(1000, 500)

    val lineBtn = new Button { text = "Line" }
    val rectBtn = new Button { text = "Rectangle" }
    val circleBtn = new Button { text = "Circle" }
    val drawPanel = new Panel() {
      opaque = true
      background = new Color(255, 255, 255)
      val line = new Line

      override def paint(g: Graphics2D) = {
        g.setColor(java.awt.Color.WHITE)
        g.fillRect(0, 0, size.getWidth().toInt, size.getHeight().toInt)

        g.setColor(java.awt.Color.BLACK)
        line.draw(g)
      }

      listenTo(mouse.clicks)
      listenTo(mouse.moves)

      reactions += {
        case e: MousePressed =>
          println("pressed " + e.point)
          line.start = e.point
          line.end = e.point
          repaint()
        case e: MouseReleased =>
          println("released " + e.point)
          line.end = e.point
          this.repaint()
        case e: MouseDragged =>
          line.end = e.point
          repaint()
      }
    }

    val toolbox = new BoxPanel(Orientation.Horizontal) {
      contents += lineBtn
      contents += rectBtn
      contents += circleBtn
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += toolbox
      contents += drawPanel
    }
  }
}