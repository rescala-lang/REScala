package reshapes.ui.panels
import java.awt.BasicStroke

import scala.annotation.serializable
import scala.collection.mutable.MutableList
import scala.swing.event.MouseDragged
import scala.swing.event.MousePressed
import scala.swing.event.MouseReleased
import scala.swing.Color
import scala.swing.Graphics2D
import scala.swing.Point
import scala.swing.Panel
import scala.util.Marshal

import reshapes.command.CreateShape
import reshapes.command.EditShape
import reshapes.figures.Movable
import reshapes.figures.Resizable
import reshapes.figures.Shape
import reshapes.util.MathUtil
import reshapes.Drawing
import reshapes.DrawingSpaceState
import reshapes.Reshapes
import reshapes.Selection

/**
 * Represents the panel where all shapes are drawn onto.
 */
class DrawingPanel(val event: DrawingSpaceState) extends Panel {
  opaque = true

  private var currentPath = List.empty[Point]
  private var shapes = List[Shape]()
  private var currentlyDrawing: Shape = null
  private var shapeBeforeEdit: Shape = null
  private var resizingMode = false

  override def paint(g: Graphics2D) = {
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, size.getWidth().toInt, size.getHeight().toInt)

    g.setColor(java.awt.Color.BLACK)
    Reshapes.currentEvents.allShapes.map(x => x.draw(g))
    if (currentlyDrawing != null) {
      currentlyDrawing.draw(g)
    }
  }

  listenTo(mouse.clicks)
  listenTo(mouse.moves)

  reactions += {
    case e: MousePressed =>
      currentPath = List(e.point)
      Reshapes.currentEvents.mode match {
        case Drawing() =>
          currentlyDrawing = Reshapes.currentEvents.nextShape.getClass().newInstance()
          currentlyDrawing.strokeWidth = Reshapes.currentEvents.strokeWidth
          currentlyDrawing.color = Reshapes.currentEvents.color
        case Selection() =>
          // hack to get a object copy
          shapeBeforeEdit = Marshal.load[Shape](Marshal.dump[Shape](Reshapes.currentEvents.selectedShape))
          Reshapes.currentEvents.mode match {
            case Drawing() =>
              currentlyDrawing.update(currentPath)
            case Selection() =>
              val shape = Reshapes.currentEvents.selectedShape
              resizingMode = MathUtil.isInCircle(shape.start, 6, e.point) || MathUtil.isInCircle(shape.end, 6, e.point)
            case _ =>
          }
        case _ =>
      }
      repaint()
    case e: MouseDragged =>
      currentPath = currentPath ::: List(e.point)
      Reshapes.currentEvents.mode match {
        case Drawing() =>
          currentlyDrawing.update(currentPath)
        case Selection() =>
          val shape = Reshapes.currentEvents.selectedShape
          if (resizingMode && shape.isInstanceOf[Resizable]) {
            shape.asInstanceOf[Resizable].resize(currentPath.reverse(1), e.point)
          } else if (shape.isInstanceOf[Movable]) {
            shape.asInstanceOf[Movable].move(currentPath.reverse(1), e.point)
          }
        case _ =>
      }
      repaint()
    case e: MouseReleased =>
      Reshapes.currentEvents.mode match {
        case Drawing() =>
          new CreateShape(currentlyDrawing).execute()
          currentlyDrawing = null
        case Selection() =>
          var command = new EditShape(shapeBeforeEdit, Reshapes.currentEvents.selectedShape)
          command.execute()
        case _ =>
      }
  }
}

/**
 * This trait draws intersection points between all drawn shapes.
 */
trait ShowIntersection extends DrawingPanel {
  override def paint(g: Graphics2D) = {
    super.paint(g)

    g.setColor(new Color(255, 0, 0))
    g.setStroke(new BasicStroke())
    getIntersectionPoints map (point => g.drawOval(point._1 - 3, point._2 - 3, 6, 6))
  }

  def getIntersectionPoints(): List[(Int, Int)] = {
    val points = MutableList[(Int, Int)]()

    if (Reshapes.currentEvents.allShapes.size == 0)
      return points.toList

    for (shape <- Reshapes.currentEvents.allShapes) {
      for (otherShape <- Reshapes.currentEvents.allShapes.filter(s => s != shape)) {
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
 * Draws a coordinate System onto the panel.
 */
trait ShowCoordinateSystem extends DrawingPanel {
  override def paint(g: Graphics2D) = {
    super.paint(g)

    g.setColor(new Color(200, 200, 200))
    g.setStroke(new BasicStroke())
    for (i <- 0 until size.height if i % 20 == 0) {
      g.drawLine(0, i, size.width, i)
      g.drawString((i / 20).toString(), 0, i)
    }
    for (i <- 0 until size.width if i % 20 == 0) {
      g.drawLine(i, 0, i, size.height)
      g.drawString((i / 20).toString(), i, 10)
    }
  }
}

/**
 * Writes the name of the shape besides them on drawing panel.
 */
trait ShowNameLabels extends DrawingPanel {
  override def paint(g: Graphics2D) = {
    super.paint(g)

    g.setColor(new Color(200, 200, 200))
    g.setStroke(new BasicStroke())
    Reshapes.currentEvents.allShapes map (shape => g.drawString(shape.toString(), shape.start.x, shape.start.y))
  }
}

