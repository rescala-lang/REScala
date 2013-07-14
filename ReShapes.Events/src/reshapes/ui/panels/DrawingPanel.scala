package reshapes.ui.panels

import java.awt.BasicStroke

import scala.collection.mutable.ListBuffer
import scala.swing.Color
import scala.swing.Graphics2D
import scala.swing.Panel
import scala.swing.Point
import scala.swing.event.MouseDragged
import scala.swing.event.MousePressed
import scala.swing.event.MouseReleased

import reshapes.drawing.CreateShape
import reshapes.drawing.DrawingSpaceState
import reshapes.drawing.EditShape
import reshapes.figures.Movable
import reshapes.figures.Resizable
import reshapes.figures.Shape
import reshapes.util.MathUtil

/**
 * Represents the panel where all shapes are drawn onto
 */
class DrawingPanel(val state: DrawingSpaceState) extends Panel {
  private var point: Point = null
  private var currentShape: Shape = null
  private var resizingMode: Boolean = false
  
  override def paint(g: Graphics2D) {
    g.setColor(java.awt.Color.WHITE)
    g.fillRect(0, 0, size.width, size.height)
    
    g.setColor(java.awt.Color.BLACK)
    if (currentShape != null) {
      currentShape.draw(g)
      for (shape <- state.shapes)
        if (!shape.selected)
          shape.draw(g)
    }
    else
      for (shape <- state.shapes)
        shape.draw(g)
  }
  
  listenTo(mouse.clicks, mouse.moves)
  
  reactions += {
    case e: MousePressed =>
      point = e.point
      state.selectedShape match {
        case null =>
          currentShape = state.nextShape.copy(
              path = List(point),
              strokeWidth = state.strokeWidth,
              color = state.color,
              current = {Shape.current += 1; Shape.current})
        case selectedShape =>
          currentShape = selectedShape.copy()
          resizingMode = MathUtil.isInCircle(currentShape.start, 6, e.point) ||
                         MathUtil.isInCircle(currentShape.end, 6, e.point)
      }
    case e: MouseDragged =>
      state.selectedShape match {
        case null =>
          currentShape = currentShape.copy(path = e.point :: currentShape.path)
        case _ =>
          if (resizingMode && currentShape.isInstanceOf[Resizable])
            currentShape = currentShape.asInstanceOf[Resizable].resizedShape(point, e.point)
          else if (currentShape.isInstanceOf[Movable])
            currentShape = currentShape.asInstanceOf[Movable].movedShape(point, e.point)
      }
      point = e.point
      repaint
    case e: MouseReleased =>
      state.selectedShape match {
        case null =>
          state execute new CreateShape(currentShape)
        case selectedShape =>
          state execute new EditShape(selectedShape, currentShape)
          state.selectedShape = currentShape
      }
      currentShape = null
      repaint
  }
  
  (state.selectedShapeChanged || //#EF
   state.shapesChanged || //#EF
   state.strokeWidthChanged || //#EF
   state.colorChanged) += { _ => repaint } //#HDL
}

/**
 * This trait draws intersection points between all drawn shapes
 */
trait ShowIntersection extends DrawingPanel {
  override def paint(g: Graphics2D) {
    super.paint(g)
    g.setColor(new Color(255, 0, 0))
    g.setStroke(new BasicStroke)
    for (point <- getIntersectionPoints)
      g.drawOval(point.x - 3, point.y - 3, 6, 6)
  }
  
  def getIntersectionPoints() = {
    val points = new ListBuffer[Point]
    
    for (shape <- state.shapes)
      for (otherShape <- state.shapes)
        if (shape != otherShape)
          for (line <- shape.toLines)
            for (otherLine <- otherShape.toLines) {
              val intersection = MathUtil.getIntersectionsOfTwoLines(line, otherLine)
              if (intersection != null)
                points += intersection
            }
    
    points.toList
  }
}

/**
 * Draws a coordinate System onto the panel
 */
trait ShowCoordinateSystem extends DrawingPanel {
  override def paint(g: Graphics2D) {
    super.paint(g)
    
    g.setColor(new Color(200, 200, 200))
    g.setStroke(new BasicStroke)
    
    for (i <- 0 until size.height if i % 20 == 0) {
      g.drawLine(0, i, size.width, i)
      g.drawString((i / 20).toString, 0, i)
    }
    
    for (i <- 0 until size.width if i % 20 == 0) {
      g.drawLine(i, 0, i, size.height)
      g.drawString((i / 20).toString, i, 10)
    }
  }
}

/**
 * Writes the name of the shape besides them on drawing panel
 */
trait ShowNameLabels extends DrawingPanel {
  override def paint(g: Graphics2D) {
    super.paint(g)
    
    g.setColor(new Color(200, 200, 200))
    g.setStroke(new BasicStroke)
    
    for (shape <- state.shapes)
      g.drawString(shape.toString, shape.start.x, shape.start.y)
  }
}

