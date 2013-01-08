package reshapes
import scala.swing._
import scala.swing.event._
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import reshapes.figures._
import events.ImperativeEvent
import java.awt.BasicStroke

/**
 * Represents the panel where all shapes are drawn onto.
 */
class DrawingPanel(events: EventHolder) extends Panel {
  opaque = true

  var currentPath: List[Point] = List()
  var shapes = List[Drawable]()
  val currentShape = Signal { events.selectedShape() }

  override def paint(g: Graphics2D) = {
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, size.getWidth().toInt, size.getHeight().toInt)

    g.setColor(java.awt.Color.BLACK)
    events.allShapes.getValue.map(x => x.draw(g))
  }

  listenTo(mouse.clicks)
  listenTo(mouse.moves)

  reactions += {
    case e: MousePressed =>
      events.selectedShape() = currentShape.getValue.getClass().newInstance()
      events.allShapes() = currentShape.getValue :: events.allShapes.getValue
      currentPath = List(e.point)
      currentShape.getValue.update(currentPath)
      repaint()
    case e: MouseDragged =>
      currentPath = currentPath ::: List(e.point)
      currentShape.getValue.update(currentPath)
      repaint()
    case e: MouseReleased =>
      currentPath = currentPath ::: List(e.point)
      currentShape.getValue.update(currentPath)
      repaint()
  }
}