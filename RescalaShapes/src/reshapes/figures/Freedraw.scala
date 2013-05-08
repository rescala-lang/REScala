package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D
import java.awt.Color
import reshapes.DrawingSpaceState

class Freedraw(
  drawingSpaceState: DrawingSpaceState,
  strokeWidth: Int = 1,
  color: Color = Color.BLACK,
  current: Int = 0,
  path: List[Point] = null)
    extends Shape(drawingSpaceState, strokeWidth, color, current, path) with Movable with Resizable {
  def doDraw(g: Graphics2D) = {
    toLines map (line => g.drawLine(line._1, line._2, line._3, line._4))
  }
  
  def toLines(): List[(Int, Int, Int, Int)] = {
    // transforms the list of points to a list of lines by connecting to adjacent points in the list
    path.tail.tail.foldLeft(List((path(0).x, path(0).y, path(1).x, path(1).y)))((a, b) => (a.head._3, a.head._4, b.x, b.y) :: a)
  }
  
  override def copy(
      drawingSpaceState: DrawingSpaceState,
      strokeWidth: Int, 
      color: Color,
      current: Int,
      path: List[Point]) =
    new Freedraw(drawingSpaceState, strokeWidth, color, current, path)
}