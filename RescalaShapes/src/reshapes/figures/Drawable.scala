package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

abstract class Drawable(var start: Point = new Point(0, 0), var end: Point = new Point(0, 0)) {
  def update(path: List[Point])
  def draw(g: Graphics2D)
}