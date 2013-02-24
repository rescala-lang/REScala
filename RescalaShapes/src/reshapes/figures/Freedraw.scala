package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Freedraw extends Drawable {

  var path = List[Point]()

  override def doUpdate(path: List[Point]) = {
    this.path = path
  }

  def doDraw(g: Graphics2D) = {
    path map (point => g.fillOval(point.x, point.y, strokeWidth + 2, strokeWidth + 2))
  }
}