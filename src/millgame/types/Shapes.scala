package millgame.types

import scala.Numeric.Implicits._
import scala.language.implicitConversions
import java.awt.Color

//
// presentational attributes
//
case class Presentation[T: Numeric, ST, S <% Shape[ST]](
    shape: S, color: Color = Color.BLACK, width: T = 1) {
  def toDouble = Presentation(shape, color, width.toDouble)
  def toInt = Presentation(shape, color, width.toInt)
}

//
// shape
//
abstract class Shape[T: Numeric] {
  def toDouble: Shape[Double]
  def toInt: Shape[Int]
}

//
// point
//
object Point {
  implicit def fromPoint[T: Numeric](p: Point[T]) = (p.x, p.y)
  implicit def toPoint[T: Numeric](p: (T, T)) = Point(p._1, p._2)
}

case class Point[@specialized(Int, Double) T: Numeric](x: T, y: T) extends Shape[T] {
  def toDouble = Point(x.toDouble, y.toDouble)
  def toInt = Point(x.toInt, y.toInt)
  
  def +(p: Point[T]) = Point(x + p.x, y + p.y)
  def -(p: Point[T]) = Point(x - p.x, y - p.y)
  def unary_-() = Point(-x, -y)
  def *(d: T) = Point(x * d, y * d)
  def /(d: Double) = Point(x.toDouble / d, y.toDouble / d)
  
  def euclidian = math.sqrt((x * x + y * y).toDouble)
  def normalize = if (x == 0 && y == 0) this else this / euclidian
  def distance(p: Point[T]) = (p - this).euclidian
  def directionTo(p: Point[T]) = (p - this).normalize
}

//
// line
//
object Line {
  implicit def fromLine[T](l: Line[T]) = (l.from, l.to)
  implicit def toLine[T: Numeric, A <% Point[T], B <% Point[T]](l: (A, B)) =
    Line(l._1, l._2)
}

case class Line[T: Numeric](from: Point[T], to: Point[T]) extends Shape[T] {
  def toDouble = Line(from.toDouble, to.toDouble)
  def toInt = Line(from.toInt, to.toInt)
  
  def length = from distance to
}

//
// rectangle
//
object Rect {
  implicit def apply[T: Numeric](x: T, y: T, width: T, height: T): Rect[T] =
    Rect(Point(x, y), width, height)
}

case class Rect[T: Numeric](anchor: Point[T], width: T, height: T) extends Shape[T] {
  def toDouble = Rect(anchor.toDouble, width.toDouble, height.toDouble)
  def toInt = Rect(anchor.toInt, width.toInt, height.toInt)
}

//
// circle
//
object Circle {
  implicit def apply[T: Numeric](x: T, y: T, radius: T): Circle[T] =
    Circle(Point(x, y), radius)
}

case class Circle[T: Numeric](center: Point[T], radius: T) extends Shape[T] {
  def toDouble = Circle(center.toDouble, radius.toDouble)
  def toInt = Circle(center.toInt, radius.toInt)
}
