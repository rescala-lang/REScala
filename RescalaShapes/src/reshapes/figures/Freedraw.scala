package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Freedraw extends Movable {

  def doDraw(g: Graphics2D) = {
    toLines map (line => g.drawLine(line._1, line._2, line._3, line._4))
  }

  def toLines(): List[(Int, Int, Int, Int)] = {
    // transforms the list of points to a list of lines by connecting to adjacent points in the list
    path.tail.tail.foldLeft(List((path(0).x, path(0).y, path(1).x, path(1).y)))((a, b) => (a.first._3, a.first._4, b.x, b.y) :: a)
  }
}