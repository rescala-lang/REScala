package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Rectangle extends Drawable {

  def update(path: List[Point]) = {
    start = path.first
    end = path.last
  }

  def draw(g: Graphics2D) = {
    g.drawRect(start.x, start.y, math.abs(start.x - end.x), math.abs(start.y - end.y))
  }
}