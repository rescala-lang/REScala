package reshapes.figures

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point

import reshapes.drawing.DrawingSpaceState

class Triangle(
    drawingSpaceState: DrawingSpaceState,
    strokeWidth: Int = 1,
    color: Color = Color.BLACK,
    current: Int = 0,
    path: List[Point] = List.empty)
  extends Shape(drawingSpaceState, strokeWidth, color, current, path) with Movable with Resizable {
  
  def doDraw(g: Graphics2D) = {
    toLines map (line => g.drawLine(line._1, line._2, line._3, line._4))
  }
  
  def toLines(): List[(Int, Int, Int, Int)] = {
    List((start.x, start.y, end.x, end.y), (start.x, start.y, start.x, end.y), (start.x, end.y, end.x, end.y))
  }
  
  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int, 
      color: Color,
      current: Int,
      path: List[Point]) =
    new Triangle(drawingSpaceState, strokeWidth, color, current, path)
}