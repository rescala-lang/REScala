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
  
  def doDraw(g: Graphics2D) = {
    var width = math.abs(start.x - end.x)
    var height = math.abs(start.y - end.y)
    var x = math.min(start.x, end.x)
    var y = math.min(start.y, end.y)

    g.drawOval(x, y, width, height)
  }
  
  def toLines(): List[(Int, Int, Int, Int)] = {
    // todo: implement
    List[(Int, Int, Int, Int)]()
  }
  
  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int, 
      color: Color,
      current: Int,
      path: List[Point]) =
    new Oval(drawingSpaceState, strokeWidth, color, current, path)
}