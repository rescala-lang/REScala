package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

trait Drawable {
  def update(path: List[Point])
  def draw(g: Graphics2D)
}