package reshapes
import scala.swing._
import scala.swing.event._
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import reshapes.figures._

class DrawingPanel extends Panel {
  opaque = true
  background = new Color(255, 255, 255)

  var currentPath: List[Point] = List()
  var line = new Line()

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
      currentPath = List()
      currentPath = currentPath ::: List(e.point)
      line.update(currentPath)
      repaint()
    case e: MouseDragged =>
      currentPath = currentPath ::: List(e.point)
      line.update(currentPath)
      repaint()
    case e: MouseReleased =>
      currentPath = currentPath ::: List(e.point)
      println(currentPath)
      line.update(currentPath)
      repaint()
  }
}