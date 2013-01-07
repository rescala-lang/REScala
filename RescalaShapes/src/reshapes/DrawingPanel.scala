package reshapes
import scala.swing._
import scala.swing.event._
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import reshapes.figures._
import events.ImperativeEvent

class DrawingPanel(shapeSelectedEvent: ImperativeEvent[Drawable]) extends Panel {
  opaque = true
  background = new Color(255, 255, 255)

  var currentPath: List[Point] = List()
  var shapes = List[Drawable]()
  var currentShape: Drawable = new Line

  shapeSelectedEvent += (shape => newShape(shape))

  def newShape(shape: Drawable): Unit = {
    shapes ::= shape
    currentShape = shape
  }

  override def paint(g: Graphics2D) = {
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, size.getWidth().toInt, size.getHeight().toInt)

    g.setColor(java.awt.Color.BLACK)
    shapes.map(x => x.draw(g))
  }

  listenTo(mouse.clicks)
  listenTo(mouse.moves)

  reactions += {
    case e: MousePressed =>
      currentPath = List()
      currentPath = currentPath ::: List(e.point)
      currentShape.update(currentPath)
      repaint()
    case e: MouseDragged =>
      currentPath = currentPath ::: List(e.point)
      currentShape.update(currentPath)
      repaint()
    case e: MouseReleased =>
      currentPath = currentPath ::: List(e.point)
      currentShape.update(currentPath)
      repaint()
      newShape(currentShape.getClass().newInstance())
  }
}