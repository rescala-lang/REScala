package reshapes.panels
import scala.swing._
import scala.swing.event._
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import reshapes.figures._
import events.ImperativeEvent
import java.awt.BasicStroke
import reshapes.command.CreateShapeCommand
import scala.util.Marshal
import reshapes.command.EditShapeCommand
import reshapes.Events
import reshapes.Drawing
import reshapes.Selection

/**
 * Represents the panel where all shapes are drawn onto.
 */
class DrawingPanel() extends Panel {
  opaque = true

  var currentPath: List[Point] = List()
  var shapes = List[Drawable]()
  val currentShape = Signal { Events.nextShape() }
  var shapeBeforeEdit: Drawable = null

  override def paint(g: Graphics2D) = {
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, size.getWidth().toInt, size.getHeight().toInt)

    g.setColor(java.awt.Color.BLACK)
    Events.allShapes.getValue.map(x => x.draw(g))
  }

  listenTo(mouse.clicks)
  listenTo(mouse.moves)
  listenTo(keys)

  reactions += {
    case e: MousePressed =>
      currentPath = List(e.point)
      Events.mode match {
        case Drawing() =>
          Events.nextShape() = currentShape.getValue.getClass().newInstance()
          var command = new CreateShapeCommand(currentShape.getValue)
          command.execute()
        case Selection() =>
          shapeBeforeEdit = Marshal.load[Drawable](Marshal.dump[Drawable](Events.selectedShape.getValue))
        case _ =>
      }
      repaint()
    case e: MouseDragged =>
      currentPath = currentPath ::: List(e.point)
      Events.mode match {
        case Drawing() =>
          currentShape.getValue.update(currentPath)
        case Selection() =>
          Events.selectedShape.getValue.moveOrResize(currentPath.reverse(1), e.point)
        case _ =>
      }
      repaint()
    case e: MouseReleased =>
      Events.mode match {
        case Selection() =>
          var command = new EditShapeCommand(shapeBeforeEdit, Events.selectedShape.getValue)
        case _ =>
      }
  }

  Events.canvasChange += (_ => repaint())
}