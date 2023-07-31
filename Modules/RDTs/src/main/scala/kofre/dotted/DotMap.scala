package kofre.dotted

import kofre.base.Lattice
import kofre.time.Dots

/** DotMap is just a container to use maps in a dotted context.
  * Merge/<= are done per entry, with missing entries replaced by `Bottom.empty`.
  * Decompose decomposes all components.
  *
  * See [[kofre.datatypes.contextual.ObserveRemoveMap]] for a usage example.
  */
case class DotMap[K, V](repr: Map[K, V])

object DotMap {

  def empty[K, V]: DotMap[K, V] = DotMap(Map.empty)

  given lattice[K, V: Lattice]: Lattice[DotMap[K, V]] = Lattice.derived

  given hasDots[K, V: HasDots]: HasDots[DotMap[K, V]] with {
    extension (value: DotMap[K, V])
      override def dots: Dots =
        value.repr.valuesIterator.map(v => v.dots).reduceOption(_ union _).getOrElse(Dots.empty)
      override def removeDots(dots: Dots): Option[DotMap[K, V]] =
        val res = value.repr.flatMap { (k, v) =>
          HasDots.apply.removeDots(v)(dots).map(k -> _)
        }
        if res.isEmpty then None
        else Some(DotMap(res))

  }

}
