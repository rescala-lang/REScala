package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Freedraw extends Movable {

  def doDraw(g: Graphics2D) = {
    var lastPoint = path.first
    for (point <- path.tail) {
      g.drawLine(lastPoint.x, lastPoint.y, point.x, point.y)
      lastPoint = point
    }
  }
}