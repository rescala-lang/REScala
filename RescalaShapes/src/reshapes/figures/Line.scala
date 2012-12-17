package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Line(var start: Point = new Point(0, 0), var end: Point = new Point(0, 0)) extends Drawable {

  def update(path: List[Point]) = {
    start = path.first
    end = path.last
  }

  def draw(g: Graphics2D) = {
    g.drawLine(start.x, start.y, end.x, end.y)
  }
}