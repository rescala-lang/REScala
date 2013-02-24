package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Freedraw extends Movable {

  override def doDraw(g: Graphics2D) = {
    path map (point => g.fillOval(point.x, point.y, strokeWidth + 2, strokeWidth + 2))
  }
}