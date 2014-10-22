package animal.types

import scala.language.implicitConversions

object Pos {
  implicit def fromTuple(t: (Int, Int)): Pos = new Pos(t._1, t._2)
  implicit def toTuple(pos: Pos): (Int, Int) = (pos.x, pos.y)
}

/** Helper class to do simple arithmetic on (Int, Int) tuples */
case class Pos(x: Int, y: Int) {
  def +(other: Pos): (Int, Int) = (x + other.x, y + other.y)
  def -(other: Pos): (Int, Int) = this + (- other)
  def unary_-(): (Int, Int) = (-x, -y)
  def /(scalar: Double): (Int, Int) =
    (scala.math.round(x / scalar).asInstanceOf[Int], scala.math.round(y / scalar).asInstanceOf[Int])

  def euclidianNorm: Double = scala.math.sqrt(x*x + y*y)
  def normalize: (Int, Int) = /(euclidianNorm)
  def distance(other: Pos): Double = Pos.fromTuple(other - this).euclidianNorm
  def directionTo(target: Pos): (Int, Int) = Pos.fromTuple(target - this).normalize
}

