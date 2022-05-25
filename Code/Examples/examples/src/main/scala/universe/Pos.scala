package universe

/** Helper class to do simple arithmetic on (Int, Int) tuples */
case class Pos(x: Int, y: Int) {
  def distance(other: Pos): Double  = (other - this).euclidianNorm
  def directionTo(target: Pos): Pos = (target - this).normalize
  def -(other: Pos): Pos            = this + (other.unary_-)
  def +(other: Pos): Pos            = Pos(x + other.x, y + other.y)
  def unary_- : Pos                 = Pos(-x, -y)
  def normalize: Pos                = /(euclidianNorm)
  def /(scalar: Double): Pos =
    Pos(scala.math.round(x / scalar).asInstanceOf[Int], scala.math.round(y / scalar).asInstanceOf[Int])
  def euclidianNorm: Double = scala.math.sqrt((x * x + y * y).toDouble)
}
