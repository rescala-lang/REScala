package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Line extends Movable with Resizable {

  override def doDraw(g: Graphics2D) = {
    g.drawLine(start.x, start.y, end.x, end.y)
  }
}