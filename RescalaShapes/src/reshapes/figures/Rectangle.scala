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
    path: List[Point] = null)
  extends Shape(drawingSpaceState, strokeWidth, color, current, path) with Movable with Resizable {
  
  def doDraw(g: Graphics2D) = {
    var width = math.abs(start.x - end.x)
    var height = math.abs(start.y - end.y)
    var x = math.min(start.x, end.x)
    var y = math.min(start.y, end.y)

    g.drawRect(x, y, width, height)
  }
  
  def toLines(): List[(Int, Int, Int, Int)] = {
    var width = math.abs(start.x - end.x)
    var height = math.abs(start.y - end.y)

    val topleft = ((math.min(start.x, end.x), math.min(start.y, end.y)))
    val topright = ((topleft._1 + width, topleft._2))
    val bottomleft = ((topleft._1, topleft._2 + height))
    val bottomright = ((topleft._1 + width, topleft._2 + height))

    List((topleft, topright), (topleft, bottomleft), (bottomleft, bottomright), (bottomright, topright)) map
      (points => (points._1._1, points._1._2, points._2._1, points._2._2))
  }
  
  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int, 
      color: Color,
      current: Int,
      path: List[Point]) =
    new Rectangle(drawingSpaceState, strokeWidth, color, current, path)
}