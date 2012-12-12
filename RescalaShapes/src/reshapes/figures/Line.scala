package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Line(var start: Point = new Point(0, 0), var end: Point = new Point(0, 0)) {

  def draw(g: Graphics2D) = {
    g.drawLine(start.x, start.y, end.x, end.y)
  }
}