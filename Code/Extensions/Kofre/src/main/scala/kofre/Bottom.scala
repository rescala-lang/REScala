package kofre

/** Bottom.empty is the identity of Lattice.merge */
trait Bottom[A] {
  def empty: A
}
object Bottom {
  def empty[A](using bottom: Bottom[A]): A = bottom.empty
}
