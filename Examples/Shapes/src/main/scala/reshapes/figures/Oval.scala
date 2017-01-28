package reshapes.figures

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point

import reshapes.drawing.DrawingSpaceState

class Oval(
    drawingSpaceState: DrawingSpaceState,
    strokeWidth: Int = 1,
    color: Color = Color.BLACK,
    current: Int = 0,
    path: List[Point] = List.empty)
  extends Shape(drawingSpaceState, strokeWidth, color, current, path) with Movable with Resizable {
  
  override def doDraw(g: Graphics2D) {
    val width = math.abs(start.x - end.x)
    val height = math.abs(start.y - end.y)
    val x = math.min(start.x, end.x)
    val y = math.min(start.y, end.y)
    g.drawOval(x, y, width, height)
  }
  
  override def toLines() =
    // TODO: implement method
    List.empty
  
  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int, 
      color: Color,
      current: Int,
      path: List[Point]) =
    new Oval(drawingSpaceState, strokeWidth, color, current, path)
}