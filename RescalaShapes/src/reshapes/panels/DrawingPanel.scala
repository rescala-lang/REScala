package reshapes.panels
import scala.swing._
import scala.swing.event._
import scala.events.behaviour.Signal
import scala.events.behaviour.Var
import reshapes.figures._
import events.ImperativeEvent
import java.awt.BasicStroke
import reshapes.command.CreateShape
import scala.util.Marshal
import reshapes.command._
import reshapes.Drawing
import reshapes.Selection
import reshapes.Events
import reshapes.util.MathUtil
import scala.collection.mutable.MutableList
import scala.events.scalareact

trait ShowIntersection extends DrawingPanel {
  override def paint(g: Graphics2D) = {
    super.paint(g)

    g.setColor(new Color(255, 0, 0))
    g.setStroke(new BasicStroke())
    getIntersectionPoints map (point => g.drawOval(point._1 - 3, point._2 - 3, 6, 6))
  }

  def getIntersectionPoints(): List[(Int, Int)] = {
    val points = MutableList[(Int, Int)]()

    if (events.allShapes.getValue.size == 0)
      return points.toList

    for (shape <- events.allShapes.getValue) {
      for (otherShape <- events.allShapes.getValue.filter(s => s != shape)) {
        for (line <- shape.toLines()) {
          for (otherline <- otherShape.toLines()) {
            val intersection = MathUtil.getIntersectionsOfTwoLines(line, otherline)
            if (intersection != null)
              points += intersection
          }
        }
      }
    }

    points.toList
  }
}

/**
 * Represents the panel where all shapes are drawn onto.
 */
class DrawingPanel(var events: Events) extends Panel {
  opaque = true

  var currentPath = new Var[List[Point]](List())
  var shapes = List[Drawable]()
  val currentShape = Signal { events.nextShape() }
  var currentlyDrawing: Drawable = null
  var shapeBeforeEdit: Drawable = null

  override def paint(g: Graphics2D) = {
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, size.getWidth().toInt, size.getHeight().toInt)

    g.setColor(java.awt.Color.BLACK)
    events.allShapes.getValue.map(x => x.draw(g))
    if (currentlyDrawing != null) {
      currentlyDrawing.draw(g)
    }
  }

  listenTo(mouse.clicks)
  listenTo(mouse.moves)

  reactions += {
    case e: MousePressed =>
      currentPath() = List(e.point)
      events.mode match {
        case Drawing() =>
          currentlyDrawing = currentShape.getValue.getClass().newInstance()
          currentlyDrawing.strokeWidth = events.strokeWidth.getValue
          currentlyDrawing.color = events.color.getValue
        case Selection() =>
          shapeBeforeEdit = Marshal.load[Drawable](Marshal.dump[Drawable](events.selectedShape.getValue)) // hack to get a object copy
        case _ =>
      }
      repaint()
    case e: MouseDragged =>
      currentPath() = currentPath.getValue ::: List(e.point)
      events.mode match {
        case Drawing() =>
          currentlyDrawing.update(currentPath.getValue)
        case Selection() =>
          val shape = events.selectedShape.getValue
          if ((MathUtil.isInCircle(shape.start, 6, e.point) || MathUtil.isInCircle(shape.end, 6, e.point))
            && shape.isInstanceOf[Resizable]) {
            shape.asInstanceOf[Resizable].resize(currentPath.getValue.reverse(1), e.point)
          } else if (shape.isInstanceOf[Movable]) {
            shape.asInstanceOf[Movable].move(currentPath.getValue.reverse(1), e.point)
          }
        case _ =>
      }
      repaint()
    case e: MouseReleased =>
      events.mode match {
        case Drawing() =>
          new CreateShape(currentlyDrawing).execute()
          currentlyDrawing = null
        case Selection() =>
          var command = new EditShape(shapeBeforeEdit, events.selectedShape.getValue)
          command.execute()
        case _ =>
      }
  }

  events.canvasChange += (_ => repaint())
}