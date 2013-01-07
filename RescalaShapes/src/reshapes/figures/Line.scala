package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Line extends Drawable {

  def update(path: List[Point]) = {
    start = path.first
    end = path.last
  }

  def doDraw(g: Graphics2D) = {
    g.drawLine(start.x, start.y, end.x, end.y)
  }
}