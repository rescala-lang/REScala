package reshapes.figures
import java.awt.Point
import java.awt.Graphics2D

class Line extends Movable with Resizable {

  def doDraw(g: Graphics2D) = {
    g.drawLine(start.x, start.y, end.x, end.y)
  }

  def toLines(): List[(Int, Int, Int, Int)] = {
    List((start.x, start.y, end.x, end.y))
  }
}