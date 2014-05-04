package millgame.types

import scala.Numeric.Implicits._
import scala.language.implicitConversions

//
// shape
//
abstract class Shape {
  def toDouble: Shape
  def toInt: Shape
}

//
// point
//
object Point {
  implicit def fromPoint[T: Numeric](p: Point[T]) = (p.x, p.y)
  implicit def toPoint[T: Numeric](p: (T, T)) = Point(p._1, p._2)
}

case class Point[@specialized(Int, Double) T: Numeric](x: T, y: T) extends Shape {
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

case class Line[T: Numeric](from: Point[T], to: Point[T]) extends Shape {
  def toDouble = Line(from.toDouble, to.toDouble)
  def toInt = Line(from.toInt, to.toInt)
  
  def length = from distance to
}

//
// rectangle
//
case class Rect[T: Numeric](anchor: Point[T], width: T, height: T) extends Shape {
  def toDouble = Rect(anchor.toDouble, width.toDouble, height.toDouble)
  def toInt = Rect(anchor.toInt, width.toInt, height.toInt)
}

//
// circle
//
case class Circle[T: Numeric](center: Point[T], radius: T) extends Shape {
  def toDouble = Circle(center.toInt, radius.toInt)
  def toInt = Circle(center.toDouble, radius.toDouble)
}
