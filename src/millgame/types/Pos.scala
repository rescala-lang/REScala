package millgame.types

object Pos {
  implicit def fromTuple(t: (Int, Int)): Pos = new Pos(t._1, t._2)
  def apply(x: Int, y: Int) = new Pos(x, y)
}

/** Helper class to do simple arithmetic on (Int, Int) tuples */
class Pos(val x: Int, val y: Int) extends Tuple2(x, y) {
  def +(other: Pos): (Int, Int) = (x + other.x, y + other.y)
  def -(other: Pos): (Int, Int) = this + (- other)
  def unary_-(): (Int, Int) = (-x, -y)
  def /(scalar: Double) = 
    (scala.math.round(x / scalar).asInstanceOf[Int], scala.math.round(y / scalar).asInstanceOf[Int])
  
  def euclidianNorm = scala.math.sqrt(x*x + y*y)
  def normalize = /(euclidianNorm)
  def distance(other: Pos) = ((other - this): Pos).euclidianNorm 
  def directionTo(target: Pos) = ((target - this): Pos).normalize 
}

