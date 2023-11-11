package kofre.datatypes

import kofre.base.{Bottom, Lattice}
import kofre.dotted.HasDots
import kofre.syntax.{OpsSyntaxHelper, PermMutate, PermQuery}
import kofre.time.{Dots, Time}

case class Epoche[E](counter: Time, value: E)

object Epoche {

  def empty[E: Bottom]: Epoche[E] = Epoche(0, Bottom[E].empty)

  given bottom[E: Bottom]: Bottom[Epoche[E]] with
    override def empty: Epoche[E] = Epoche.empty

  given hasDots[E: HasDots: Bottom]: HasDots[Epoche[E]] = new {
    extension (dotted: Epoche[E])
      def dots: Dots                                = dotted.value.dots
      def removeDots(dots: Dots): Option[Epoche[E]] = dotted.value.removeDots(dots).map(nv => dotted.copy(value = nv))
  }

  extension [C, E](container: C)
    def epoche: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C)
      extends OpsSyntaxHelper[C, Epoche[E]](container) {
    def read(using PermQuery): E = current.value

    def write(using PermMutate)(value: E): C       = current.copy(value = value).mutator
    def epocheWrite(using PermMutate)(value: E): C = Epoche(current.counter + 1, value).mutator

    def map(using PermMutate)(f: E => E): C = write(f(current.value))
  }

  given latticeInstance[E: Lattice]: Lattice[Epoche[E]] = new Lattice[Epoche[E]] {

    override def lteq(left: Epoche[E], right: Epoche[E]): Boolean = (left, right) match {
      case (Epoche(cLeft, vLeft), Epoche(cRight, vRight)) =>
        cLeft < cRight || (cLeft == cRight && Lattice[E].lteq(vLeft, vRight))
    }

    /** Decomposes a lattice state into ic unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: Epoche[E]): Iterable[Epoche[E]] =
      val Epoche(c, v) = state
      Lattice[E].decompose(v).map(Epoche(c, _))

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Epoche[E], right: Epoche[E]): Epoche[E] = (left, right) match {
      case (Epoche(cLeft, vLeft), Epoche(cRight, vRight)) =>
        if (cLeft > cRight) left
        else if (cRight > cLeft) right
        else Epoche(cLeft, Lattice[E].merge(vLeft, vRight))
    }
  }
}
