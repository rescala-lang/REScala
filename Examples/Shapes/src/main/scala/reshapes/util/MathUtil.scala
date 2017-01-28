package reshapes.util

import java.awt.Point

object MathUtil {
  def isInCircle(center: Point, radius: Int, point: Point): Boolean = {
    val dx = center.x - point.x
    val dy = center.y - point.y
    math.sqrt(dx * dx + dy * dy) <= radius
  }
  
  /**
   * returns the intersection points of two finite lines.
   * return null if the lines do not cross
   * algorithm/formular copied from 'http://mathworld.wolfram.com/Line-LineIntersection.html'
   */
  def getIntersectionsOfTwoLines(line1: (Point, Point), line2: (Point, Point)): Point = {
    val x1 = line1._1.x
    val y1 = line1._1.y
    val x2 = line1._2.x
    val y2 = line1._2.y
    val x3 = line2._1.x
    val y3 = line2._1.y
    val x4 = line2._2.x
    val y4 = line2._2.y

    try {
      val x = determinant(
        determinant(x1, y1, x2, y2),
        x1 - x2,
        determinant(x3, y3, x4, y4),
        x3 - x4) / determinant(
          x1 - x2,
          y1 - y2,
          x3 - x4,
          y3 - y4)
      
      val y = determinant(
        determinant(x1, y1, x2, y2),
        y1 - y2,
        determinant(x3, y3, x4, y4),
        y3 - y4) / determinant(
          x1 - x2,
          y1 - y2,
          x3 - x4,
          y3 - y4)
      
      if (x >= math.min(x1, x2) && x <= math.max(x1, x2) &&
          y >= math.min(y1, y2) && y <= math.max(y1, y2) &&
          x >= math.min(x3, x4) && x <= math.max(x3, x4) &&
          y >= math.min(y3, y4) && y <= math.max(y3, y4))
        new Point(x, y)
      else
        null
    }
    catch {
      case e: ArithmeticException => null
    }
  }
  
  /**
   * Calculates the determinant of:
   * | a b |
   * | c d |
   */
  def determinant(a: Int, b: Int, c: Int, d: Int): Int = {
    a * d - b * c
  }
}