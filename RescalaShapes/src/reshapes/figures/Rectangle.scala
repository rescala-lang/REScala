package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Rectangle extends Movable with Resizable {

  def doDraw(g: Graphics2D) = {
    var width = math.abs(start.x - end.x)
    var height = math.abs(start.y - end.y)
    var x = math.min(start.x, end.x)
    var y = math.min(start.y, end.y)

    g.drawRect(x, y, width, height)
  }
}