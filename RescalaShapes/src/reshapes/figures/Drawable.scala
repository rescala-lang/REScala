package reshapes.figures

import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point

import reshapes.drawing.DrawingSpaceState
import reshapes.util.MathUtil

abstract class Shape(
    val drawingSpaceState: DrawingSpaceState,
    val strokeWidth: Int = 1,
    val color: Color = Color.BLACK,
    val current: Int = 0,
    val path: List[Point] = null /* the mouse path while drawing this shape */) {
  
  def selected = drawingSpaceState.selectedShape == this
  def start = if (path == null) null else path.head
  def end = if (path == null) null else path.last
  
  def draw(g: Graphics2D) = {
    if (start != null && end != null) {
      val stroke = if (!selected) new BasicStroke(strokeWidth) else new BasicStroke(strokeWidth,
        BasicStroke.CAP_BUTT,
        BasicStroke.JOIN_MITER,
        10.0f, Array(10.0f), 0.0f)
      
      g.setStroke(stroke)
      g.setColor(color)
      doDraw(g)
    }
  }
  
  def copy(
      drawingSpaceState: DrawingSpaceState = drawingSpaceState,
      strokeWidth: Int = strokeWidth,
      color: Color = color,
      current: Int = current,
      path: List[Point] = path): Shape
  
  override def toString(): String =
    this.getClass().getSimpleName() + " #" + current.toString()
  
  def doUpdate(path: List[Point]) = {}
  def doDraw(g: Graphics2D)
  
  def toLines(): List[(Int, Int, Int, Int)]
}

object Shape {
  var current = 0
}

trait Movable extends Shape {
  def movedShape(from: Point, to: Point) =
    copy(path = path map {p => new Point(p.x + to.x - from.x, p.y + to.y - from.y)})
}

trait Resizable extends Shape {
  def resizedShape(from: Point, to: Point) = {
    if (MathUtil.isInCircle(start, 6, from))
      copy(path = to :: path)
    else if (MathUtil.isInCircle(end, 6, from))
      copy(path = path :+ to)
    else
      this: Shape
  }
  
  override def draw(g: Graphics2D) = {
    super.draw(g)
    if (start != null && end != null && selected) {
      val origStroke = g.getStroke()
      g.setStroke(new BasicStroke(1))
      g.setColor(new Color(200, 200, 200))
      
      g.drawOval(start.x - 5, start.y - 5, 10, 10)
      g.drawOval(end.x - 5, end.y - 5, 10, 10)
      
      g.setStroke(origStroke)
    }
  }
}
