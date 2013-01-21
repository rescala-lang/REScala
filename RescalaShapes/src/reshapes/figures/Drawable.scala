package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D
import java.awt.BasicStroke
import java.awt.Color
import reshapes.util.MathUtil

@serializable
abstract class Drawable(var start: Point = new Point(0, 0), var end: Point = new Point(0, 0)) {
  Drawable.current += 1

  var strokeWidth = 1
  var color = Color.BLACK
  var selected = false
  var current = Drawable.current

  def draw(g: Graphics2D) = {
    val stroke = if (!selected) new BasicStroke(strokeWidth) else new BasicStroke(strokeWidth,
      BasicStroke.CAP_BUTT,
      BasicStroke.JOIN_MITER,
      10.0f, Array(10.0f), 0.0f)

    if (selected) {
      g.setColor(new Color(200, 200, 200))

      g.drawOval(start.x - 5, start.y - 5, 10, 10)
      g.drawOval(end.x - 5, end.y - 5, 10, 10)

    }

    g.setStroke(stroke)
    g.setColor(color)
    doDraw(g)
  }

  def moveOrResize(from: Point, to: Point) = {
    if (MathUtil.isInCircle(start, 6, from)) {
      start = to
    } else if (MathUtil.isInCircle(end, 6, to)) {
      end = to
    } else {
      val deltaX = (if (from.x < to.x) 1 else -1) * math.abs(from.x - to.x)
      val deltaY = (if (from.y < to.y) 1 else -1) * math.abs(from.y - to.y)
      start.x += deltaX
      end.x += deltaX
      start.y += deltaY
      end.y += deltaY
    }
  }

  override def toString(): String = {
    this.getClass().getSimpleName() + " #" + current.toString()
  }

  // methods needed to be implemented by sublclasses
  def update(path: List[Point])
  def doDraw(g: Graphics2D)
}

object Drawable {
  private var current = 0
}