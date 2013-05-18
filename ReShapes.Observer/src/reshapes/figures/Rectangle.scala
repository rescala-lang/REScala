package reshapes.figures

import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point

import reshapes.drawing.DrawingSpaceState

class Rectangle(
    drawingSpaceState: DrawingSpaceState,
    strokeWidth: Int = 1,
    color: Color = Color.BLACK,
    current: Int = 0,
    path: List[Point] = List.empty)
  extends Shape(drawingSpaceState, strokeWidth, color, current, path) with Movable with Resizable {
  
  override def doDraw(g: Graphics2D) = {
    val width = math.abs(start.x - end.x)
    val height = math.abs(start.y - end.y)
    val x = math.min(start.x, end.x)
    val y = math.min(start.y, end.y)
    g.drawRect(x, y, width, height)
  }
  
  override def toLines() = {
    val width = math.abs(start.x - end.x)
    val height = math.abs(start.y - end.y)
    
    val topleft = new Point(math.min(start.x, end.x), math.min(start.y, end.y))
    val topright = new Point(topleft.x + width, topleft.y)
    val bottomleft = new Point(topleft.x, topleft.y + height)
    val bottomright = new Point(topleft.x + width, topleft.y + height)
    
    List((topleft, topright), (topleft, bottomleft),
         (bottomleft, bottomright), (bottomright, topright))
  }
  
  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int, 
      color: Color,
      current: Int,
      path: List[Point]) =
    new Rectangle(drawingSpaceState, strokeWidth, color, current, path)
}