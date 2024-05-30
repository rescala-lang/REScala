package rdts.datatypes

import rdts.base.{Bottom, Lattice}
import rdts.dotted.HasDots
import rdts.syntax.{OpsSyntaxHelper, PermMutate, PermQuery}
import rdts.time.{Dots, Time}

case class Epoch[E](counter: Time, value: E)

object Epoch {

  def empty[E: Bottom]: Epoch[E] = Epoch(0, Bottom[E].empty)

  given bottom[E: Bottom]: Bottom[Epoch[E]] with
    override def empty: Epoch[E] = Epoch.empty

  given hasDots[E: HasDots: Bottom]: HasDots[Epoch[E]] = new {
    extension (dotted: Epoch[E])
      def dots: Dots                               = dotted.value.dots
      def removeDots(dots: Dots): Option[Epoch[E]] = dotted.value.removeDots(dots).map(nv => dotted.copy(value = nv))
  }

  extension [C, E](container: C)
    def epoche: syntax[C, E] = syntax(container)

  implicit class syntax[C, E](container: C)
      extends OpsSyntaxHelper[C, Epoch[E]](container) {
    def read(using IsQuery): E = current.value

    def write(using IsMutator)(value: E): C       = current.copy(value = value).mutator
    def epocheWrite(using IsMutator)(value: E): C = Epoch(current.counter + 1, value).mutator

    def map(using IsMutator)(f: E => E): C = write(f(current.value))
  }

  given latticeInstance[E: Lattice]: Lattice[Epoch[E]] = new Lattice[Epoch[E]] {

    override def lteq(left: Epoch[E], right: Epoch[E]): Boolean = (left, right) match {
      case (Epoch(cLeft, vLeft), Epoch(cRight, vRight)) =>
        cLeft < cRight || (cLeft == cRight && Lattice[E].lteq(vLeft, vRight))
    }

    /** Decomposes a lattice state into ic unique irredundant join decomposition of join-irreducible states */
    override def decompose(state: Epoch[E]): Iterable[Epoch[E]] =
      val Epoch(c, v) = state
      Lattice[E].decompose(v).map(Epoch(c, _))

    /** By assumption: associative, commutative, idempotent. */
    override def merge(left: Epoch[E], right: Epoch[E]): Epoch[E] = (left, right) match {
      case (Epoch(cLeft, vLeft), Epoch(cRight, vRight)) =>
        if cLeft > cRight then left
        else if cRight > cLeft then right
        else Epoch(cLeft, Lattice[E].merge(vLeft, vRight))
    }
  }
}
