package reshapes.figures
import java.awt.Graphics2D

class Triangle extends Movable with Resizable {

  def doDraw(g: Graphics2D) = {

    g.drawLine(start.x, start.y, end.x, end.y)
    g.drawLine(start.x, start.y, start.x, end.y)
    g.drawLine(start.x, end.y, end.x, end.y)

  }
}