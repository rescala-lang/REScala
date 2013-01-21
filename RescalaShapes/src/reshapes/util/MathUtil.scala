package reshapes.util
import java.awt.Point

object MathUtil {

  def isInCircle(center: Point, radius: Int, point: Point): Boolean = {
    val dist = math.sqrt((center.x - point.x) * (center.x - point.x) + (center.y - point.y) * (center.y - point.y))
    dist <= radius
  }
}