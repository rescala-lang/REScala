package reshapes.figures

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point

import reshapes.drawing.DrawingSpaceState

class Line(
    drawingSpaceState: DrawingSpaceState,
    strokeWidth: Int = 1,
    color: Color = Color.BLACK,
    current: Int = 0,
    path: List[Point] = List.empty)
  extends Shape(drawingSpaceState, strokeWidth, color, current, path) with Movable with Resizable {
  
  override def doDraw(g: Graphics2D) =
    g.drawLine(start.x, start.y, end.x, end.y)
  
  override def toLines() =
    List((start, end))
  
  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int, 
      color: Color,
      current: Int,
      path: List[Point]) =
    new Line(drawingSpaceState, strokeWidth, color, current, path)
}