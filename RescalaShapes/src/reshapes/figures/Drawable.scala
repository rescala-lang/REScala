package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D
import java.awt.BasicStroke
import java.awt.Color

abstract class Drawable(var start: Point = new Point(0, 0), var end: Point = new Point(0, 0)) {
  Drawable.current += 1

  var strokeWidth = 1
  var color = Color.BLACK
  var selected = false

  def draw(g: Graphics2D) = {
    g.setStroke(new BasicStroke(strokeWidth + (if (selected) 1 else 0)))
    g.setColor(color)
    doDraw(g)
  }

  def move(from: Point, to: Point) = {
    val deltaX = (if (from.x < to.x) 1 else -1) * math.abs(from.x - to.x)
    val deltaY = (if (from.y < to.y) 1 else -1) * math.abs(from.y - to.y)
    start.x += deltaX
    end.x += deltaX
    start.y += deltaY
    end.y += deltaY
  }

  override def toString(): String = {
    this.getClass().getSimpleName() + " #" + Drawable.current.toString()
  }

  // methods needed to be implemented by sublclasses
  def update(path: List[Point])
  def doDraw(g: Graphics2D)
}

object Drawable {
  private var current = 0
}