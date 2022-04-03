package examples.miscellanea

import java.awt.{Dimension, Graphics2D, Point}

import rescala.default._

import scala.collection.mutable.ListBuffer
import scala.swing.{MainFrame, Panel, SimpleSwingApplication, Swing}

object PulsingCircle extends SimpleSwingApplication {
  lazy val application = new PulsingCircle
  def top              = application.frame

  override def main(args: Array[String]): Unit = {
    super.main(args)
    while (true) {
      Swing onEDTWait {
        application.base.transform(_ + 1)
        application.frame.repaint()
      }
      Thread sleep 20
    }
  }
}

class PulsingCircle {
  /*
  class Point(val x: Double,val y: Double) {
    def move(delta: Delta) = new Point(x + delta.x, y + delta.y)
    override def toString = "Point("+ x + "," + y +")"
  }

  class Line(m: Double, q: Double) {
    def translate(delta: Float) = new Line(m, q + delta)
    def rotate(delta: Float) = new Line(m + delta, q)
    override def toString = "Line("+ m + "," + q +")"
  }

   */

  val toDraw = ListBuffer[Function1[Graphics2D, Unit]]()
  type Delta = Point

  class Oval(center: Signal[Point], radius: Signal[Int]) {
    toDraw += ((g: Graphics2D) => { g.fillOval(center.now.x, center.now.y, radius.now, radius.now) })

    override def toString = "Circle(" + center + "," + radius + ")"
  }

  val base = Var(0)
  val time = Signal { base() % 200 } // time is cyclic :)

  val point1 = Signal { new Point(20 + time(), 20 + time()) }
  new Oval(point1, time)
  val point2 = Signal { new Point(40 + time(), 80 + time()) }
  new Oval(point2, time)
  val point3 = Signal { new Point(80 + time(), 40 + time()) }
  new Oval(point3, time)
  val point4 = Signal { new Point(160 + time(), 160 + time()) }
  new Oval(point4, time)

  // drawing code
  def top = frame
  val frame = new MainFrame {
    contents = new Panel() {
      preferredSize = new Dimension(600, 600)
      override def paintComponent(g: Graphics2D): Unit = {
        toDraw.foreach(x => x(g))
      }
    }
  }

}
