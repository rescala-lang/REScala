package ersir.shared

import rescala.extra.lattices.Lattice

case class Epoche[A](sequence: Long, value: A) {
  def map[B](f: A => B): Epoche[B]      = copy(value = f(value))
  def next(value: A = value): Epoche[A] = Epoche(sequence + 1, value)
}

object Epoche {
  def apply[A](value: A): Epoche[A] = Epoche(0, value)

  implicit def lattice[A: Lattice]: Lattice[Epoche[A]] =
    new Lattice[Epoche[A]] {
      override def merge(left: Epoche[A], right: Epoche[A]): Epoche[A] =
        right.sequence.compareTo(left.sequence) match {
          case -1 => left
          case 1  => right
          case 0  => Epoche(right.sequence, Lattice.merge(left.value, right.value))
        }
    }

}
