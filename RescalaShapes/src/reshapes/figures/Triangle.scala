package reshapes.figures
import java.awt.Graphics2D

class Triangle extends Movable with Resizable {

  def doDraw(g: Graphics2D) = {
    toLines map (line => g.drawLine(line._1, line._2, line._3, line._4))
  }

  def toLines(): List[(Int, Int, Int, Int)] = {
    List((start.x, start.y, end.x, end.y), (start.x, start.y, start.x, end.y), (start.x, end.y, end.x, end.y))
  }
}