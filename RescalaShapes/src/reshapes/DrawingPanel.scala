package reshapes
import scala.swing._
import scala.swing.event._
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import reshapes.figures._
import events.ImperativeEvent
import java.awt.BasicStroke
import reshapes.command.CreateShapeCommand

/**
 * Represents the panel where all shapes are drawn onto.
 */
class DrawingPanel(events: EventHolder) extends Panel {
  opaque = true

  var currentPath: List[Point] = List()
  var shapes = List[Drawable]()
  val currentShape = Signal { events.nextShape() }

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
      currentPath = List(e.point)
      events.mode match {
        case Drawing() =>
          events.nextShape() = currentShape.getValue.getClass().newInstance()
          var command = new CreateShapeCommand(events, currentShape.getValue)
          command.execute()
          events.Commands() = command :: events.Commands.getValue
        case _ =>
      }
      repaint()
    case e: MouseDragged =>
      currentPath = currentPath ::: List(e.point)
      events.mode match {
        case Drawing() =>
          currentShape.getValue.update(currentPath)
        case Selection() =>
          events.selectedShape.getValue.move(currentPath.reverse(1), e.point)
      }
      repaint()
  }

  events.canvasChange += (_ => repaint())
}