package rdts.datatypes

import rdts.base.Lattice.OrdinalLattices
import rdts.base.{Bottom, Decompose, Lattice}
import rdts.dotted.HasDots
import rdts.time.{Dots, Time}

case class Epoch[E](counter: Time, value: E) {

  type Delta = Epoch[E]

  def read: E = value

  def write(value: E): Delta = copy(value = value)

  def epocheWrite(value: E): Delta = Epoch(counter + 1, value)

  def map(f: E => E): Delta = write(f(value))
}

object Epoch {

  def empty[E: Bottom]: Epoch[E] = Epoch(0, Bottom[E].empty)

  given bottom[E: Bottom]: Bottom[Epoch[E]] with
    override def empty: Epoch[E] = Epoch.empty

  given hasDots[E: {HasDots, Bottom}]: HasDots[Epoch[E]] = new {
    extension (dotted: Epoch[E])
      def dots: Dots                               = dotted.value.dots
      def removeDots(dots: Dots): Option[Epoch[E]] = dotted.value.removeDots(dots).map(nv => dotted.copy(value = nv))
  }

  given decomposeInstance[E: Decompose]: Decompose[Epoch[E]] =
    case Epoch(c, v) => Decompose.decompose(v).map(Epoch(c, _))

  given latticeInstance[E: Lattice as E]: Lattice[Epoch[E]] = {
    given Lattice[Time] = Lattice.assertEquals
    val prodEpoche      = Lattice.productLattice[Epoch[E]]
    Lattice.Derivation.SumLattice[Epoch[E]](new OrdinalLattices[Epoch[E]] {
      override def compare(left: Epoch[E], right: Epoch[E]): Int = java.lang.Long.compare(left.counter, right.counter)
      override def lattice(elem: Epoch[E]): Lattice[Epoch[E]]    = prodEpoche
    })
  }
}
