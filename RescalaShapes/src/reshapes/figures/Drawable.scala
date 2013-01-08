package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D
import java.awt.BasicStroke
import java.awt.Color

abstract class Drawable(var start: Point = new Point(0, 0), var end: Point = new Point(0, 0)) {

  var strokeWidth = 1
  var color = Color.BLACK

  def draw(g: Graphics2D) = {
    g.setStroke(new BasicStroke(strokeWidth))
    g.setColor(color)
    doDraw(g)
  }
  def update(path: List[Point])
  def doDraw(g: Graphics2D)

  override def toString(): String = {
    this.getClass().getName()
  }
}