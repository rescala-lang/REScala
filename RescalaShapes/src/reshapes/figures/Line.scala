package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Line extends Drawable {

  def doDraw(g: Graphics2D) = {
    g.drawLine(start.x, start.y, end.x, end.y)
  }
}