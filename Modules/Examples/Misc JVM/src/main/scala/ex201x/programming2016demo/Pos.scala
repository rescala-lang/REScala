package ex201x.programming2016demo

case class Pos(x: Double, y: Double) {
  def *(v: Double) = Pos(x * v, y * v)
  def +(v: Pos)    = Pos(x + v.x, y + v.y)
}
