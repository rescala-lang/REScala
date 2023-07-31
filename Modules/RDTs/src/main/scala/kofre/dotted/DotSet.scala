package kofre.dotted

import kofre.base.Lattice
import kofre.time.{Dot, Dots}

/** DotsSets track causality of events without values.
  * They are the prototype of an [[kofre.datatypes.contextual.EnableWinsFlag]]
  */
case class DotSet(repr: Dots) {
  export repr.{contains, isEmpty, toSet}
}

object DotSet {

  val empty: DotSet = DotSet(Dots.empty)

  def from(it: Iterable[Dot]): DotSet = DotSet(Dots.from(it))

  given lattice: Lattice[DotSet] = Lattice.derived

  given hasDots: HasDots[DotSet] with {
    extension (value: DotSet)
      override def dots: Dots = value.repr

      override def removeDots(dots: Dots): Option[DotSet] =
        val res = value.repr.diff(dots)
        if res.isEmpty then None
        else Some(DotSet(res))
  }

}
