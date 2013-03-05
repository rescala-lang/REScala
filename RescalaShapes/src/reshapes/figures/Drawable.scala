package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D
import java.awt.BasicStroke
import java.awt.Color
import reshapes.util.MathUtil
import java.util.UUID

@serializable
abstract class Shape {
  Shape.current += 1

  var strokeWidth = 1
  var color = Color.BLACK
  var selected = false
  var current = Shape.current
  var path: List[Point] = null

  def start = if (path == null) null else path.first
  def end = if (path == null) null else path.last
}

object Shape {
  private var current = 0
}

@serializable
abstract class Drawable {
  Drawable.current += 1

  var strokeWidth = 1
  var color = Color.BLACK
  var selected = false
  var current = Drawable.current
  var path: List[Point] = null

  def start = if (path == null) null else path.first
  def end = if (path == null) null else path.last

  val uuid = UUID.randomUUID()

  def draw(g: Graphics2D) = {
    if (start != null && end != null) {
      val stroke = if (!selected) new BasicStroke(strokeWidth) else new BasicStroke(strokeWidth,
        BasicStroke.CAP_BUTT,
        BasicStroke.JOIN_MITER,
        10.0f, Array(10.0f), 0.0f)

      g.setStroke(stroke)
      g.setColor(color)
      doDraw(g)
    }
  }

  def update(path: List[Point]) = {
    this.path = path

    doUpdate(path)
  }

  override def equals(obj: Any): Boolean = obj match {
    case obj: Drawable => obj.uuid == this.uuid
    case _ => false
  }

  override def toString(): String = {
    this.getClass().getSimpleName() + " #" + current.toString()
  }

  def doUpdate(path: List[Point]) = {}
  def doDraw(g: Graphics2D)
  /**
   * returns a list of lines representing the shape
   */
  def toLines(): List[(Int, Int, Int, Int)]
}

object Drawable {
  private var current = 0
}

trait Movable extends Drawable {

  def move(from: Point, to: Point) = {
    val deltaX = (if (from.x < to.x) 1 else -1) * math.abs(from.x - to.x)
    val deltaY = (if (from.y < to.y) 1 else -1) * math.abs(from.y - to.y)

    path = path map (point => new Point(point.x + deltaX, point.y + deltaY))
  }
}

trait Resizable extends Drawable {

  def resize(from: Point, to: Point) = {
    if (MathUtil.isInCircle(start, 6, from)) {
      path = to :: path.tail
    } else if (MathUtil.isInCircle(end, 6, to)) {
      path = (to :: path.reverse.tail).reverse
    }
  }

  override def draw(g: Graphics2D) = {
    super.draw(g)

    if (start != null && end != null && selected) {
      val origStroke = g.getStroke()
      g.setStroke(new BasicStroke(1))
      g.setColor(new Color(200, 200, 200))

      g.drawOval(start.x - 5, start.y - 5, 10, 10)
      g.drawOval(end.x - 5, end.y - 5, 10, 10)

      g.setStroke(origStroke)
    }
  }
}
